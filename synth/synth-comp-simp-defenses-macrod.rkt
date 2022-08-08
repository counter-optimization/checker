
#lang rosette

(require rosette/lib/synthax
         ; rosette/lib/value-browser
         rosette/lib/match
         rosette/lib/angelic)

(require "../serval/serval/x86.rkt"
         (prefix-in core: "../serval/serval/lib/core.rkt"))

(provide 
  (all-defined-out)
  (all-from-out "../serval/serval/x86.rkt"
                "../serval/serval/lib/core.rkt"))

;; Config

(error-print-width 100000)
; (displayln (format "Current grammar depth: ~a" (current-grammar-depth)))
; (displayln (format "Current error-print-width: ~a" (error-print-width)))

;; Common

(define (make-x86-64-cpu)
  (define mm (core:make-flat-memmgr #:bitwidth 64))
  (init-cpu mm))

(define (run-x86-64-impl #:insns insns #:cpu cpu)
  
  (define (run-insn i cpu)
    (match i
      ['noop #f]
      [_ (instruction-run i cpu)]))
  
  (if (list? insns)
    (for ([i insns])
      (run-insn i cpu))
    (run-insn insns cpu)))

;; Addition

; General utility
(define (assert-bvs-equiv bvs1 bvs2)
  (for ([b1 bvs1]
        [b2 bvs2])
    (assert (bveq b1 b2))))

(define (assume-bvs-equiv bvs1 bvs2)
  (for ([b1 bvs1]
        [b2 bvs2])
    (assume (bveq b1 b2))))

; (E)FLAG comparison functions
(define flag-symbols '(CF PF AF ZF SF OF))
; (displayln (format "flag-symbols: ~a" flag-symbols))

(define (get-all-flags #:cpu cpu)
  (for/list ([flag-sym flag-symbols])
    (cpu-flag-ref cpu flag-sym)))

(define assert-flags-equiv assert-bvs-equiv)
(define assume-flags-equiv assume-bvs-equiv)

(define all-regs (list rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15
                       eax ecx edx ebx esp ebp esi edi r8d r9d r10d r11d r12d r13d r14d r15d
                       ax cx dx bx sp bp si di r8w r9w r10w r11w r12w r13w r14w r15w
                       al cl dl))

; GPR comparison functions
(define all-regs-but-raxes (list rcx ;rdx
                                 rbx
                                 rsp rbp rsi rdi
                                 r8 r9 r10 r11
                                 r12 r13 r14 r15
                                 ecx ;edx
                                 ebx
                                 esp ebp esi edi
                                 r8d r9d r10d r11d
                                 r12d r13d r14d r15d
                                 ;dx dl
                                 cx cl))
; (displayln (format "all-regs-but-raxes: ~a" all-regs-but-raxes))

(define all-raxes (list rax eax ax al))
(define all-rdxes (list rdx edx dx dl))

(define (get-all-regs-but-raxes #:cpu cpu)
  (for/list ([reg all-regs-but-raxes])
    (cpu-gpr-ref cpu reg)))

(define (get-all-regs-raxes-only #:cpu cpu)
  (for/list ([reg all-raxes])
    (cpu-gpr-ref cpu reg)))

(define (get-all-regs-rdxes-only #:cpu cpu)
  (for/list ([reg all-rdxes])
    (cpu-gpr-ref cpu reg)))

(define assert-regs-equiv assert-bvs-equiv)
(define assume-regs-equiv assume-bvs-equiv)

(define (assume-all-regs-equiv cpu1 cpu2)
  (for ([reg-name all-regs])
    (define r1 (cpu-gpr-ref cpu1 reg-name))
    (define r2 (cpu-gpr-ref cpu2 reg-name))
    (assume (bveq r1 r2))))

(define (assume-all-flags-equiv cpu1 cpu2)
  (for ([flag-name flag-symbols])
    (define f1 (cpu-flag-ref cpu1 flag-name))
    (define f2 (cpu-flag-ref cpu2 flag-name))
    (assume (bveq f1 f2))))

(define (assert-all-regs-equiv cpu1 cpu2)
  (for ([reg-name all-regs])
    (define r1 (cpu-gpr-ref cpu1 reg-name))
    (define r2 (cpu-gpr-ref cpu2 reg-name))
    (assert (bveq r1 r2))))

(define (assert-all-flags-equiv cpu1 cpu2)
  (for ([flag-name flag-symbols])
    (define f1 (cpu-flag-ref cpu1 flag-name))
    (define f2 (cpu-flag-ref cpu2 flag-name))
    (assert (bveq f1 f2))))

; Synthesis impl and spec for adding eax and ecx
; Adding eax <- eax + ecx
(define (add-r/m32-r32-conc-impl)
  (define add (add-r/m32-r32 eax ecx))
  (list add))

(define (sub-r/m32-r32-conc-impl)
  (define sub (sub-r/m32-r32 eax ecx))
  (list sub))

(define-grammar (x86-64-sub-synth)
  [single-insn (choose*
                 (sub-r-i) 
                 (sub-r-r) 
                 (setcc) 
                 (cmov) 
                 (mul))]
  [sub-r-i (choose*
            (sub-r/m32-imm32 (reg32) (i32))
            (sub-r/m64-imm32 (reg64) (i32))
            (sub-r/m32-imm8 (reg32) (i8))
            (sub-r/m64-imm8 (reg64) (i8)))]
  [sub-r-r (choose*
            (sub-r/m32-r32 (reg32) (reg32))
            (sub-r/m64-r64 (reg64) (reg64)))]
  [setcc (choose* (setz (reg8)))]
  [cmov (choose*
          (cmovz-r32-r32 (reg32) (reg32))
          (cmovne-r32-r32 (reg32) (reg32)))]
  [mul (choose*
        (mul-r/m32 (reg32))
        (mul-r/m64 (reg64)))]
  [reg64 (choose* rax rcx rdx rdi)]
                 ;; rbx
                 ;;  rsp rbp rsi rdi
                 ;;  r8 r9 r10 r11
                 ;;  r12 r13 r14 r15)]
  [reg32 (choose* eax ecx edx edi)]
                 ;; ebx
                 ;;  esp ebp esi edi
                 ;;  r8d r9d r10d r11d
                 ;;  r12d r13d r14d r15d)]
  [reg16 (choose* ax cx dx di)]
                 ;; bx
                  ;; sp bp si di
                  ;; r8w r9w r10w r11w
  ;; r12w r13w r14w r15w)]
  [reg8 (choose* al cl dl)]
  [i32 (?? (bitvector 32))]
  [i16 (?? (bitvector 16))]
  [i8 (?? (bitvector 8))])

(define-grammar (x86-64-arith-insn)
  [insn (choose* (bin-insn-rr)
                 (bin-insn-ri)
                 ;; (bin-op-special)
                 ;; (shifts)
                 ;; (rotates)
                 (un-insn-r)
                 (un-insn-i))]
  
  [bin-insn-rr (choose*
                ((bin-op-rr32) (reg32) (reg32))
                ((bin-op-rr64) (reg64) (reg64)))]
  [bin-insn-ri (choose*
                ((bin-op-ri) (reg32) (imm32))
                ((bin-op-r64i32) (reg64) (imm32)))]
  [un-insn-r (choose*
              ((un-op-r8) (reg8))
              ((un-op-r16) (reg16))
              ((un-op-r32) (reg32))
              ((un-op-r64) (reg64)))]
  [un-insn-i ((un-op-i) (imm32))]
  
  [bin-op-rr32 (choose* add-r/m32-r32
                        and-r/m32-r32
                        cmovz-r32-r32
                        cmovne-r32-r32
                        adc-r/m32-r32
                        cmp-r/m32-r32
                        mov-r/m32-r32
                        or-r/m32-r32
                        sbb-r/m32-r32
                        sub-r/m32-r32
                        test-r/m32-r32
                        xchg-r/m32-r32
                        xor-r/m32-r32)]
  [bin-op-rr64 (choose* or-r/m64-r64
                        sbb-r/m64-r64
                        sub-r/m64-r64
                        test-r/m64-r64
                        xchg-r/m64-r64
                        xor-r/m64-r64)]
  [bin-op-r64i32 (choose* or-r/m64-imm32
                          sub-r/m64-imm32
                          test-r/m64-imm32
                          xor-r/m64-imm32)]
  [bin-op-ri (choose* add-r/m32-imm32
                      and-r/m32-imm32
                      cmp-r/m32-imm32
                      mov-r/m32-imm32
                      or-r/m32-imm32
                      sub-r/m32-imm32
                      test-r/m32-imm32
                      xor-r/m32-imm32)]
  [rotates (choose*
            (rol-r/m16-imm8 (reg16) (imm8))
            (rol-r/m32-imm8 (reg32) (imm8))
            (rol-r/m64-imm8 (reg64) (imm8))
            (ror-r/m16-imm8 (reg16) (imm8))
            (ror-r/m32-imm8 (reg32) (imm8))
            (ror-r/m64-imm8 (reg64) (imm8)))]
  [shifts (choose*
           (sar-r/m32-imm8 (reg32) (imm8))
           (sar-r/m64-imm8 (reg64) (imm8))
           (shl-r/m32-imm8 (reg32) (imm8))
           (shl-r/m64-imm8 (reg64) (imm8))
           (shr-r/m32-imm8 (reg32) (imm8))
           (shr-r/m64-imm8 (reg64) (imm8)))]
  [bin-op-special (choose*
                   (movsxd-r/m32-r64 (reg32) (reg64))
                   (movzx-r32-r/m16 (reg32) (reg16))
                   (movzx-r64-r/m16 (reg32) (reg16))
                   (sbb-r/m32-imm8 (reg32) (imm8))
                   (sbb-r/m64-imm8 (reg64) (imm8)))]

  [un-op-r8 (choose* set-ge-r8
                     set-l-r8
                     set-ne-r8)]
  [un-op-r16 (choose* rol-r/m16-cl
                      ror-r/m16-cl)]
  [un-op-r32 (choose* mul-r/m32
                      div-r/m32
                      bswap-r32
                      neg-r/m32
                      rol-r/m32-cl
                      ror-r/m32-cl
                      shl-r/m32-cl
                      sar-r/m32-cl
                      shr-r/m32-cl)]
  [un-op-r64 (choose* neg-r/m64
                      rol-r/m64-cl
                      ror-r/m64-cl
                      sar-r/m64-cl
                      shl-r/m64-cl
                      shr-r/m64-cl)]
  [un-op-i (choose* add-eax-imm32
                    and-eax-imm32
                    and-rax-imm32
                    cmp-eax-imm32
                    cmp-rax-imm32
                    or-eax-imm32
                    or-rax-imm32
                    sub-eax-imm32
                    sub-rax-imm32
                    test-eax-imm32
                    test-rax-imm32
                    xor-eax-imm32
                    xor-rax-imm32)]

  [reg64 (choose* rax rcx rdx)]
                 ;; rbx
                 ;;  rsp rbp rsi rdi
                 ;;  r8 r9 r10 r11
                 ;;  r12 r13 r14 r15)]
  [reg32 (choose* eax ecx edx)]
                 ;; ebx
                 ;;  esp ebp esi edi
                 ;;  r8d r9d r10d r11d
                 ;;  r12d r13d r14d r15d)]
  [reg16 (choose* ax cx dx)]
                 ;; bx
                  ;; sp bp si di
                  ;; r8w r9w r10w r11w
  ;; r12w r13w r14w r15w)]
  [reg8 (choose* al cl dl)]
  
  [imm32 (?? (bitvector 32))]
  [imm16 (?? (bitvector 16))]
  [imm8 (?? (bitvector 8))])

(define (generate-add-r/m32-r32-insns #:num-insns num-insns)
  (for/list ([i num-insns])
    (x86-64-arith-insn #:depth 20)))

(define-syntax (generate-sub-insns stx)
  (syntax-case stx ()
    [(_ num-insns)
     (with-syntax ([synth-calls (syntax->datum 
                                  #'(for/list ([i num-insns])
                                      (x86-64-sub-synth #:depth 3)))])
       #'synth-calls)]))

(define (generate-sub-insns-no-macro #:num-insns N)
  (for/list ([i N])
    (x86-64-sub-synth #:depth 3)))

(define zero-for-bw (λ (bw) (bv 0 bw)))
(define one-for-bw (λ (bw) (bv 1 bw)))

(define addident-for-bw zero-for-bw)
(define mulident-for-bw one-for-bw)
(define mulzero-for-bw zero-for-bw)

; gets bitwidth of operand as bitvector
(define (bitwidth-getter operand)
  (match operand
    [(struct gpr32 _) 32]
    [(struct gpr64 _) 64]
    [(struct gpr16 _) 16]
    [(struct gpr8 _) 8]
    ; probably an immediate
    [(list smt ...) (length operand)]
    [(? bv?) (bitvector-size (type-of operand))]
    [(quote implicit-rax) 64]
    [(quote implicit-eax) 32]
    [(quote implicit-ax) 16]
    [(quote implicit-al) 8]
    [_ (printf "unhandled case in bitwidth-getter for operand ~a\n" operand)
       (exit 2)]))

; returns the bitvector value behind the operand
(define (operand-decoder operand cpu)
  (match operand
    [(struct gpr32 _) (cpu-gpr-ref cpu operand)]
    [(struct gpr64 _) (cpu-gpr-ref cpu operand)]
    [(struct gpr16 _) (cpu-gpr-ref cpu operand)]
    [(struct gpr8 _) (cpu-gpr-ref cpu operand)]
    ; probably an immediate
    [(list smt ...) (decode-imm operand)]
    [(? bitvector?) operand]
    [(? bv?) operand]
    [(quote implicit-rax) (cpu-gpr-ref cpu rax)]
    [(quote implicit-eax) (cpu-gpr-ref cpu eax)]
    [(quote implicit-ax) (cpu-gpr-ref cpu ax)]
    [(quote implicit-al) (cpu-gpr-ref cpu al)]
    [_ (printf "unhandled case in operand-decoder for operand ~a\n" operand)
       (exit 3)]))

(define (assert-operand-is-not-special operand special-for-bw cpu)
  (define operand-bw (bitwidth-getter operand))
  (define operand-val (operand-decoder operand cpu))
  (define special (special-for-bw operand-bw))
  (assert (! (bveq special operand-val))))

(define (comp-simp-asserter #:insn insn #:cpu cpu)
  
  (define addident-checker
    (λ (op) (assert-operand-is-not-special op addident-for-bw cpu)))
  
  (define mulident-checker
    (λ (op) (assert-operand-is-not-special op mulident-for-bw cpu)))
  
  (define mulzero-checker
    (λ (op) (assert-operand-is-not-special op mulzero-for-bw cpu)))
  
  (match insn
    [(add-r/m32-r32 op1 op2)
     (begin
       (addident-checker op1)
       (addident-checker op2))]
    [(add-r/m32-imm32 op1 op2)
     (begin
       (addident-checker op1)
       (addident-checker op2))]
    
    [(add-eax-imm32 op1)
     (begin
       (addident-checker op1))]

    [(sub-r/m32-r32 op1 op2)
     (begin
       (addident-checker op2))]
    [(sub-r/m64-r64 op1 op2)
         (begin
       (addident-checker op2))]
    [(sub-r/m32-imm32 op1 op2)
         (begin
       (addident-checker op2))]
    [(sub-r/m32-imm8 op1 op2)
         (begin
       (addident-checker op2))]
    [(sub-r/m64-imm8 op1 op2)
         (begin
       (addident-checker op2))]
    [(sub-r/m64-imm32 op1 op2)
     (begin
       (addident-checker op2))]

    [(mul-r/m32 op1)
     (begin
       (mulident-checker op1)
       (mulzero-checker op1)
       (mulident-checker 'implicit-eax)
       (mulzero-checker 'implicit-eax))]
    [(mul-r/m64 op1)
     (begin
       (mulident-checker op1)
       (mulzero-checker op1)
       (mulident-checker 'implicit-eax)
       (mulzero-checker 'implicit-eax))]
    
    [_ #f]))
  
(define (apply-insn-specific-asserts #:insns insns
                                     #:asserter asserter
                                     #:cpu cpu)
  (for ([i insns])
    (for/all ([val i #:exhaustive])
      (asserter #:insn val #:cpu cpu))))

(define (sub-r/m32-r32-spec #:spec-cpu spec-cpu
                            #:impl-cpu impl-cpu)
  ; this is just the insn sequence:
  ; (list (sub-r/m32-r32 eax ecx))
  (define spec-insns (sub-r/m32-r32-conc-impl))

  ; these are the synthesis candidate instructions
  ; REPLACE_ME
  (define impl-insns (generate-sub-insns 1))
  
  ; 1. assume starting in the same state
  (assume-all-regs-equiv spec-cpu impl-cpu)
  (assume-all-flags-equiv spec-cpu impl-cpu)

  ; this is for sanity checking the comp-simp checker applied asserts.
  ; with these lines uncommented out (saying that eax and ecx never start
  ; with the value 0), then the synthesizer should be able to synthesize
  ; sub eax, ecx as a comp-simp-safe-for-these-assumptions, equivalent 
  ; insn sequence
  ; (assume (&& (! (bveq (cpu-gpr-ref impl-cpu eax) (bv 0 32)))
  ;             (! (bveq (cpu-gpr-ref impl-cpu ecx) (bv 0 32)))))

  ; 2. for all add insns in impl-insns, no comp simp can take place.
  (apply-insn-specific-asserts #:insns impl-insns
                               #:asserter comp-simp-asserter
                               #:cpu impl-cpu)

  ; 3. run the impl and the spec
  (run-x86-64-impl #:insns spec-insns #:cpu spec-cpu)
  (run-x86-64-impl #:insns impl-insns #:cpu impl-cpu)
  
  ; 4. preserves all registers but the dest reg
  ; (define spec-reg-state-after (get-all-regs-but-raxes #:cpu spec-cpu))
  ; (define impl-reg-state-after (get-all-regs-but-raxes #:cpu impl-cpu))
  ; (assert-regs-equiv spec-reg-state-after impl-reg-state-after)
  
  ; 5. sets all flags to expected values
  ; (define spec-flag-state-after (get-all-flags #:cpu spec-cpu))
  ; (define impl-flag-state-after (get-all-flags #:cpu impl-cpu))
  ; (assert-flags-equiv spec-flag-state-after impl-flag-state-after)
  
  ; 6. computes the add of eax and ecx into eax
  (define spec-eax (cpu-gpr-ref spec-cpu eax))
  (define impl-eax (cpu-gpr-ref impl-cpu eax))
  (assert (bveq spec-eax impl-eax)))

;; these **must** be the same as the ones used in
;; define-grammar
(define (forall-quantified-vars some-cpu)
  (for/list ([reg-id (list rax rcx rdx rdi eax ecx edi edi ax cx dx di al cl dl)])
    (cpu-gpr-ref some-cpu reg-id)))

; sorta the equivalent to if __name__ == '__main__' in python:
; see https://stackoverflow.com/questions/28591576/detecting-if-script-executed-from-command-line-in-racket
(module+ main
  ; Command line arg for insn seq length
  ; varied by the shell script that runs this code
  ; (define num-insns (string->number (vector-ref (current-command-line-arguments) 0)))
  ; (printf "num-insn: ~a\n" num-insns)

  ; synthesis calling code
  (define impl-cpu (make-x86-64-cpu))
  (define spec-cpu (make-x86-64-cpu))

  (define solution
    (synthesize
    #:forall (append (forall-quantified-vars impl-cpu)
                      (forall-quantified-vars spec-cpu))
    #:guarantee (sub-r/m32-r32-spec #:spec-cpu spec-cpu
                                    #:impl-cpu impl-cpu)))
  (printf "Solution is: ~a\n" solution)

  (if (sat? solution)
      (begin
        (printf "Solution found for n=1 insns.\n")
        (print-forms solution)
        (exit 0))
      (begin
        (printf "No solution.\n")
        (exit 1))))
