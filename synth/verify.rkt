#lang rosette

(require 
  racket/cmdline
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  "transform-list.rkt"
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

  (comp-simp:run-x86-64-impl #:insns attempt #:cpu attempt-cpu #:assert-cs true)
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

(define (run-verifier transform)
  (displayln "running verification...")
  (define cex (verify (comp-simp-verify (car transform) (cdr transform) (list eax))))
  (displayln "finished verification")
  (displayln cex)
  (displayln "\n")
  (if (equal? cex (unsat)) "unsat" "sat"))

(module+ main
  ; (define cex (verify (comp-simp-verify attempt-mul16-p12 spec-mul16-p12 (list ax cx))))
  (for/vector ([insn (current-command-line-arguments)])
    (displayln (format "Attempting to verify ~s" insn))
    (define transforms (comp-simp-transform insn))
    (define result
      (if (list? transforms)
        (map run-verifier transforms)
        "No transform found"))
    (displayln (format "Results for ~s: ~a" insn result))
    (displayln "\n")
    result))
