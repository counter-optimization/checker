#lang rosette

(require rosette/lib/synthax
         ;rosette/lib/value-browser
         rosette/lib/angelic)

(require "serval/serval/x86.rkt"
         (prefix-in core: "serval/serval/lib/core.rkt"))

;; Common

(define (make-x86-64-cpu)
  (define mm (core:make-flat-memmgr #:bitwidth 64))
  (init-cpu mm))

(define (run-x86-64-impl #:insns insns #:cpu cpu)
  (for ([i insns])
    (match i
      ['noop empty]
      [_ (interpret-insn cpu i)])))

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
(displayln (format "flag-symbols: ~a" flag-symbols))

(define (get-all-flags #:cpu cpu)
  (for/list ([flag-sym flag-symbols])
    (cpu-flag-ref cpu flag-sym)))

(define assert-flags-equiv assert-bvs-equiv)
(define assume-flags-equiv assume-bvs-equiv)

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
(displayln (format "all-regs-but-raxes: ~a" all-regs-but-raxes))

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

; Synthesis impl and spec for adding eax and ecx
; Adding eax <- eax + ecx
(define (add-r/m32-r32-conc-impl)
  (define add (add-r/m32-r32 eax ecx))
  (list add))

(define (sub-r/m32-r32-conc-impl)
  (define sub (sub-r/m32-r32 eax ecx))
  (list sub))

(define-grammar (x86-64-sub-synth)
  [insn (choose*
         (sub-r-i)
         (sub-r-r)
         (setcc)
         (mul))]
  [sub-r-i (choose*
            (sub-r/m32-imm32 (reg32) (imm32))
            (sub-r/m64-imm32 (reg64) (imm32))
            (sub-r/m32-imm8 (reg32) (imm8))
            (sub-r/m64-imm8 (reg64) (imm8)))]
  [sub-r-r (choose*
            (sub-r/m32-r32 (reg32) (reg32))
            (sub-r/m64-r64 (reg64) (reg64)))]
  [setcc (choose*
          (setz (reg8)))]
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
  [imm32 (?? (bitvector 32))]
  [imm16 (?? (bitvector 16))]
  [imm8 (?? (bitvector 8))])

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

(define (generate-sub-r/m32-r32-insns #:num-insns num-insns)
  (for/list ([i num-insns])
    (x86-64-sub-synth #:depth 20)))

(displayln (format "Current grammar depth: ~a" (current-grammar-depth)))
(error-print-width 4096)
(displayln (format "Current error-print-width: ~a" (error-print-width)))

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
    [(? list?) (length operand)]
    [(? bitvector?) (bitvector-size operand)]))

; returns the bitvector value behind the operand
(define (operand-decoder operand cpu)
  (match operand
    [(or (struct gpr32 _)
         (struct gpr64 _)
         (struct gpr16 _)
         (struct gpr8 _)) (cpu-gpr-ref cpu operand)]
    ; probably an immediate
    [(? list?) (decode-imm operand)]
    [(? bitvector?) operand]
    ['implicit-rax (cpu-gpr-ref cpu rax)]
    ['implicit-eax (cpu-gpr-ref cpu eax)]
    ['implicit-ax (cpu-gpr-ref cpu ax)]
    ['implicit-al (cpu-gpr-ref cpu al)]))

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
    [(or (add-r/m32-r32 op1 op2)
         (add-r/m32-imm32 op1 op2))
     (begin
       (addident-checker op1)
       (addident-checker op2))]
    
    [(or (add-eax-imm32 op1))
     (begin
       (addident-checker op1))]

    [(or (sub-r/m32-r32 op1 op2)
         (sub-r/m64-r64 op1 op2)
         (sub-r/m32-imm32 op1 op2)
         (sub-r/m32-imm8 op1 op2)
         (sub-r/m64-imm8 op1 op2)
         (sub-r/m64-imm32 op1 op2))
     (begin
       (addident-checker op2))]

    [(or (mul-r/m32 op1)
         (mul-r/m64 op1))
     (begin
       (mulident-checker op1)
       (mulzero-checker op1)
       (mulident-checker 'implicit-eax)
       (mulzero-checker 'implicit-eax))]))
  
(define (apply-insn-specific-asserts #:insns insns
                                     #:asserter asserter
                                     #:cpu cpu)
  (displayln "Instructions are:")
  (for ([i insns])
    (printf "----------\n")
    (displayln i)
    (for/all ([val i #:exhaustive])
      (asserter #:insn val #:cpu cpu))
    (printf "----------\n")))

(define (sub-r/m32-r32-spec #:spec-cpu spec-cpu
                            #:impl-cpu impl-cpu
                            #:num-insns num-insns)
  (define spec-insns (sub-r/m32-r32-conc-impl))

  ; these are the synthesis candidate instructions
  (define impl-insns (generate-sub-r/m32-r32-insns #:num-insns num-insns))

  ; 1. assume starting in the same state
  (define spec-reg-state-before (get-all-regs-but-raxes #:cpu spec-cpu))
  (define impl-reg-state-before (get-all-regs-but-raxes #:cpu impl-cpu))
  (assume-regs-equiv spec-reg-state-before impl-reg-state-before)

  (define spec-reg-raxes-state-before (get-all-regs-raxes-only #:cpu spec-cpu))
  (define impl-reg-raxes-state-before (get-all-regs-raxes-only #:cpu impl-cpu))
  (assume-regs-equiv spec-reg-raxes-state-before impl-reg-raxes-state-before)

  (define spec-reg-rdxes-state-before (get-all-regs-rdxes-only #:cpu spec-cpu))
  (define impl-reg-rdxes-state-before (get-all-regs-rdxes-only #:cpu impl-cpu))
  (assume-regs-equiv spec-reg-rdxes-state-before impl-reg-rdxes-state-before)

  (define spec-flag-state-before (get-all-flags #:cpu spec-cpu))
  (define impl-flag-state-before (get-all-flags #:cpu impl-cpu))
  (assume-flags-equiv spec-flag-state-before impl-flag-state-before)

  ;; (assume (&& (! (bveq (cpu-gpr-ref impl-cpu eax) (bv 0 32)))
  ;;             (! (bveq (cpu-gpr-ref impl-cpu ecx) (bv 0 32)))))

  ; 2. for all add insns in impl-insns, no comp simp can take place.
  (apply-insn-specific-asserts #:insns impl-insns
                               #:asserter comp-simp-asserter
                               #:cpu impl-cpu)

  ; 3. run the impl and the spec
  (run-x86-64-impl #:insns spec-insns #:cpu spec-cpu)
  (run-x86-64-impl #:insns impl-insns #:cpu impl-cpu)
  
  ; 4. preserves all registers but the dest reg
  ;; (define spec-reg-state-after (get-all-regs-but-raxes #:cpu spec-cpu))
  ;; (define impl-reg-state-after (get-all-regs-but-raxes #:cpu impl-cpu))
  ;; (assert-regs-equiv spec-reg-state-after impl-reg-state-after)
  
  ; 5. sets all flags to expected values
  ;; (define spec-flag-state-after (get-all-flags #:cpu spec-cpu))
  ;; (define impl-flag-state-after (get-all-flags #:cpu impl-cpu))
  ;; (assert-flags-equiv spec-flag-state-after impl-flag-state-after)
  
  ; 6. computes the add of eax and ecx into eax
  (define spec-eax (cpu-gpr-ref spec-cpu eax))
  (define impl-eax (cpu-gpr-ref impl-cpu eax))
  (assert (bveq spec-eax impl-eax)))

(define num-insns (string->number (vector-ref (current-command-line-arguments) 0)))
(displayln (format "num-insn: ~a" num-insns))

(define impl-cpu (make-x86-64-cpu))
(define spec-cpu (make-x86-64-cpu))

(define solution
  (synthesize
   #:forall (append (vector->list (cpu-gprs impl-cpu))
                    (vector->list (cpu-flags impl-cpu))
                    (vector->list (cpu-gprs spec-cpu))
                    (vector->list (cpu-flags spec-cpu)))
   #:guarantee (sub-r/m32-r32-spec #:spec-cpu spec-cpu
                                   #:impl-cpu impl-cpu
                                   #:num-insns num-insns)))
(printf "Solution is: ~a\n" solution)

(if (sat? solution)
    (begin
      (printf "Solution found for ~a insns.\n" num-insns)
      (print-forms solution)
      (exit 0))
    (begin
      (printf "No solution.\n")
      (exit 1)))
