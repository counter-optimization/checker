#lang rosette

(require 
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

(provide (all-defined-out))

; (define attempt
;   (list
;    (sub-r/m32-imm32 eax (bv (expt 2 31) 32))
;    (sub-r/m32-imm32 eax (bv (expt 2 31) 32))
;    (setz dl)
;    (sub-r/m64-imm32 rdx (bv (sub1 (expt 2 32)) 32))
;    (sub-r/m64-imm32 rdx (bv 1 32))
;    (sub-r/m64-r64 rax rdx)
;    (sub-r/m64-r64 rcx rax)
;    (sub-r/m64-r64 rcx rdx)))

; (define spec
;   (list
;    (sub-r/m32-r32 ecx eax)))

; (define attempt ; add32
;   (list
;    (mov-r64-imm64 rdx (bv (expt 2 32) 64))
;    (sub-r/m64-r64 rax rdx)
;    (sub-r/m64-r64 rcx rdx)
;    (add-r/m64-r64 rcx rax)
;    (mov-r64-imm64 rdx (bv (- (expt 2 33)) 64))
;    (sub-r/m64-r64 rcx rdx)))

; (define spec    ; add32
;   (list
;    (add-r/m32-r32 ecx eax)))

(define attempt ; cmov sub
  (list
   (mov-r/m32-imm32 edx (bv 0 32))
   (sub-r/m32-imm32 eax (bv (expt 2 31) 32))
   (sub-r/m32-imm32 eax (bv (expt 2 31) 32))
   (setz dl)
   (cmovz-r32-r32 r13d eax)
   (cmovz-r32-r32 r12d ecx)
   (cmovz-r32-r32 eax edx)
   (sub-r/m32-r32 ecx eax)
   (sub-r/m32-imm32 edx (bv (expt 2 31) 32))
   (sub-r/m32-imm32 edx (bv (expt 2 31) 32))
   (cmovne-r32-r32 ecx r12d)
   (cmovne-r32-r32 eax r13d)
   ))

(define spec    ; cmov sub
  (list
   (sub-r/m32-r32 ecx eax)))

(define (comp-simp-verify attempt)
  (define spec-cpu (comp-simp:make-x86-64-cpu))
  (define attempt-cpu (comp-simp:make-x86-64-cpu))

  (comp-simp:assume-all-regs-equiv spec-cpu attempt-cpu)
  (comp-simp:assume-all-flags-equiv spec-cpu attempt-cpu)

  (comp-simp:apply-insn-specific-asserts #:insns attempt
                                         #:asserter comp-simp:comp-simp-asserter
                                         #:cpu attempt-cpu)

  (comp-simp:run-x86-64-impl #:insns attempt #:cpu attempt-cpu)
  (comp-simp:run-x86-64-impl #:insns spec #:cpu spec-cpu)

  ;; (define spec-reg-state-after (comp-simp:get-all-regs-but-raxes #:cpu spec-cpu))
  ;; (define impl-reg-state-after (comp-simp:get-all-regs-but-raxes #:cpu attempt-cpu))
  ;; (comp-simp:assert-regs-equiv spec-reg-state-after impl-reg-state-after)
  
  ;; (define spec-flag-state-after (comp-simp:get-all-flags #:cpu spec-cpu))
  ;; (define impl-flag-state-after (comp-simp:get-all-flags #:cpu attempt-cpu))
  ;; (comp-simp:assert-flags-equiv spec-flag-state-after impl-flag-state-after)
  
  (define spec-ecx (cpu-gpr-ref spec-cpu ecx))
  (define impl-ecx (cpu-gpr-ref attempt-cpu ecx))
  (assert (bveq spec-ecx impl-ecx)))

(module+ main
  (define-symbolic* tester (bitvector 32))
  (define test (verify (bveq tester (bvsub (bvsub tester (bv (expt 2 31) 32)) (bv (expt 2 31) 32)))))
  (displayln test)
  (displayln (bv (sub1 (expt 2 32)) 32))
  (define cex (verify (comp-simp-verify attempt)))
  cex)
