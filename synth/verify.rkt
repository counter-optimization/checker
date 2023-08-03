#lang rosette

(require 
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  "arith-transforms.rkt"
  "bitwise-transforms.rkt"
  "shift-transforms.rkt"
  "mul-transforms.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

(provide (all-defined-out))

(define (comp-simp-verify attempt spec regs [flags '()])
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
  
  (for ([reg regs])
    (define spec-reg (cpu-gpr-ref spec-cpu reg))
    (define impl-reg (cpu-gpr-ref attempt-cpu reg))
    (assert (bveq spec-reg impl-reg)))

  (for ([flag flags])
    (define spec-flag (cpu-flag-ref spec-cpu flag))
    (define impl-flag (cpu-flag-ref attempt-cpu flag))
    (assert (bveq spec-flag impl-flag)))
  )

(module+ main
  (displayln "running verification...")
  ; (define cex (verify (comp-simp-verify attempt-mul16-p12 spec-mul16-p12 (list ax cx))))
  (define cex (verify (comp-simp-verify attempt-imul64-rr-p2 spec-imul64-rr-p2 (list rdx))))
  (displayln "done")
  cex)
