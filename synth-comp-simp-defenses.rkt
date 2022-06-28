#lang rosette

(require rosette/lib/synthax
         rosette/lib/value-browser)

(require "serval/serval/x86.rkt"
         (prefix-in core: "serval/serval/lib/core.rkt"))

;; Common

(define (make-x86-64-cpu)
  (define mm (core:make-flat-memmgr #:bitwidth 64))
  (init-cpu mm))

(define (run-x86-64-impl #:insns insns #:cpu cpu)
  (for ([i insns])
    (interpret-insn cpu i)))

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
(define all-regs-but-raxes (list rcx rdx rbx
                                 rsp rbp rsi rdi
                                 r8 r9 r10 r11
                                 r12 r13 r14 r15
                                 ecx edx ebx
                                 esp ebp esi edi
                                 r8d r9d r10d r11d
                                 r12d r13d r14d r15d))
(displayln (format "all-regs-but-raxes: ~a" all-regs-but-raxes))

(define all-raxes (list rax eax ax al))

(define (get-all-regs-but-raxes #:cpu cpu)
  (for/list ([reg all-regs-but-raxes])
    (cpu-gpr-ref cpu reg)))

(define (get-all-regs-raxes-only #:cpu cpu)
  (for/list ([reg all-raxes])
    (cpu-gpr-ref cpu reg)))

(define assert-regs-equiv assert-bvs-equiv)
(define assume-regs-equiv assume-bvs-equiv)

; Synthesis impl and spec for adding eax and ecx
; Adding eax <- eax + ecx
; TODO:
;   - get the rest of the imm8/reg16/reg8 opcodes
;   - handle the special cases, i think someone said in slack
;     that choose and structs do not go well together
;   - handle rotate.rkt
;   - handle setcc.rkt
;     - needs gprs8 exported from registers.rkt
;   - handle shift.rkt, shld.rkt, shrd.rkt
(define (add-r/m32-r32-conc-impl)
  (define add (add-r/m32-r32 eax ecx))
  (list add))

(define-grammar (x86-64-arith-insn)
  [insn (choose (bin-insn-rr)
                (bin-insn-ri)
                (un-insn-r)
                (un-insn-i))]
  
  [bin-insn-rr (choose
                ((bin-op-rr32) (reg32) (reg32))
                ((bin-op-rr64) (reg64) (reg64)))]
  [bin-insn-ri (choose
                ((bin-op-ri) (reg32) (imm32))
                ((bin-op-r64i32) (reg64) (imm32)))]
  [un-insn-r (choose
              ((un-op-r32) (reg32))
              ((un-op-r64) (reg64)))]
  [un-insn-i ((un-op-i) (imm32))]
  
  [bin-op-rr32 (choose add-r/m32-r32
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
  [bin-op-rr64 (choose or-r/m64-r64
                       sbb-r/m64-r64
                       sub-r/m64-r64
                       test-r/m64-r64
                       xchg-r/m64-r64
                       xor-r/m64-r64)]
  [bin-op-r64i32 (choose or-r/m64-imm32
                         sub-r/m64-imm32
                         test-r/m64-imm32
                         xor-r/m64-imm32)]
  [bin-op-ri (choose add-r/m32-imm32
                     and-r/m32-imm32
                     cmp-r/m32-imm32
                     mov-r/m32-imm32
                     or-r/m32-imm32
                     sub-r/m32-imm32
                     test-r/m32-imm32
                     xor-r/m32-imm32)]
  [bin-op-special (choose
                   (movsxd-r/m32-r64 (reg32) (reg64))
                   (movzx-r32-r/m16 (reg32) (reg16))
                   (movzx-r64-r/m16 (reg32) (reg16))
                   (sbb-r/m32-imm8 (reg32) (imm8))
                   (sbb-r/m64-imm8 (reg64) (imm8)))]
  
  [un-op-r32 (choose mul-r/m32
                     div-r/m32
                     bswap-r32
                     neg-r/m32)]
  [un-op-r64 (choose neg-r/m64)]
  [un-op-i (choose add-eax-imm32
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

  [reg64 (choose rax rcx rdx rbx
                 rsp rbp rsi rdi
                 r8 r9 r10 r11
                 r12 r13 r14 r15)]
  [reg32 (choose eax ecx edx ebx
                 esp ebp esi edi
                 r8d r9d r10d r11d
                 r12d r13d r14d r15d)]
  [reg16 (choose ax cx dx bx
                 sp bp si di
                 r8w r9w r10w r11w
                 r12w r13w r14w r15w)]
  
  [imm32 (encode-imm (?? (bitvector 32)))]
  [imm16 (encode-imm (?? (bitvector 16)))]
  [imm8 (encode-imm (?? (bitvector 8)))])

(define (generate-add-r/m32-r32-insns #:num-insns num-insns)
  (for/list ([i num-insns])
    (x86-64-arith-insn #:depth 12)))

(displayln (format "Current grammar depth: ~a" (current-grammar-depth)))
(error-print-width 4096)
(displayln (format "Current error-print-width: ~a" (error-print-width)))

(define (add-r/m32-r32-spec #:spec-cpu spec-cpu
                            #:impl-cpu impl-cpu
                            #:num-insns num-insns)
  ;; (define spec-cpu (make-x86-64-cpu))
  (define spec-insns (add-r/m32-r32-conc-impl))
  
  ;; (define impl-cpu (make-x86-64-cpu))
  (define impl-insns (generate-add-r/m32-r32-insns #:num-insns num-insns))

  ; 1. assume starting in the same state
  (define spec-reg-state-before (get-all-regs-but-raxes #:cpu spec-cpu))
  (define impl-reg-state-before (get-all-regs-but-raxes #:cpu impl-cpu))
  (assume-regs-equiv spec-reg-state-before impl-reg-state-before)

  (define spec-reg-raxes-state-before (get-all-regs-raxes-only #:cpu spec-cpu))
  (define impl-reg-raxes-state-before (get-all-regs-raxes-only #:cpu impl-cpu))
  (assume-regs-equiv spec-reg-raxes-state-before impl-reg-raxes-state-before)

  (define spec-flag-state-before (get-all-flags #:cpu spec-cpu))
  (define impl-flag-state-before (get-all-flags #:cpu impl-cpu))
  (assume-flags-equiv spec-flag-state-before impl-flag-state-before)

  ; 2. for all add insns in impl-insns, no operand can be identity
  ; TODO

  ; 3. run the impl and the spec
  (run-x86-64-impl #:insns spec-insns #:cpu spec-cpu)
  (run-x86-64-impl #:insns impl-insns #:cpu impl-cpu)
  
  ; 4. preserves all registers but the dest reg
  (define spec-reg-state-after (get-all-regs-but-raxes #:cpu spec-cpu))
  (define impl-reg-state-after (get-all-regs-but-raxes #:cpu impl-cpu))
  (assert-regs-equiv spec-reg-state-after impl-reg-state-after)
  
  ; 5. sets all flags to expected values
  (define spec-flag-state-after (get-all-flags #:cpu spec-cpu))
  (define impl-flag-state-after (get-all-flags #:cpu impl-cpu))
  (assert-flags-equiv spec-flag-state-after impl-flag-state-after)
  
  ; 6. computes the add of eax and ecx into eax
  (define spec-eax (cpu-gpr-ref spec-cpu eax))
  (define impl-eax (cpu-gpr-ref impl-cpu eax))
  (assert (bveq spec-eax impl-eax)))

(define num-insns (string->number (vector-ref (current-command-line-arguments) 0)))
(displayln (format "num-insns: ~a" num-insns))

(define impl-cpu (make-x86-64-cpu))
(define spec-cpu (make-x86-64-cpu))

(define solution
  (synthesize
   #:forall (append (vector->list (cpu-gprs impl-cpu))
                    (vector->list (cpu-flags impl-cpu))
                    (vector->list (cpu-gprs spec-cpu))
                    (vector->list (cpu-flags spec-cpu)))
   #:guarantee (add-r/m32-r32-spec #:spec-cpu spec-cpu
                                   #:impl-cpu impl-cpu
                                   #:num-insns num-insns)))

(if (sat? solution)
    (begin
      (displayln (format "Solution found for ~a insns:" num-insns))
      (print-forms solution))
    (displayln "No solution."))
