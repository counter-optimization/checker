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

(define (comp-simp-verify attempt attempt-cpu spec spec-cpu [flags '()])
  (comp-simp:assume-all-regs-equiv spec-cpu attempt-cpu)
  (comp-simp:assume-all-flags-equiv spec-cpu attempt-cpu)

  (comp-simp:run-x86-64-impl #:insns attempt #:cpu attempt-cpu #:assert-cs true
                             #:verbose (print-verbose))
  (comp-simp:run-x86-64-impl #:insns spec #:cpu spec-cpu)

  ;; (define spec-reg-state-after (comp-simp:get-all-regs-but-raxes #:cpu spec-cpu))
  ;; (define impl-reg-state-after (comp-simp:get-all-regs-but-raxes #:cpu attempt-cpu))
  (comp-simp:assert-all-regs-but-scratch-equiv spec-cpu attempt-cpu)
  
  ;; (define spec-flag-state-after (comp-simp:get-all-flags #:cpu spec-cpu))
  ;; (define impl-flag-state-after (comp-simp:get-all-flags #:cpu attempt-cpu))
  ;; (comp-simp:assert-flags-equiv spec-flag-state-after impl-flag-state-after)
  
  ; (for ([reg regs])
  ;   (define spec-reg (cpu-gpr-ref spec-cpu reg))
  ;   (define impl-reg (cpu-gpr-ref attempt-cpu reg))
  ;   (assert (bveq spec-reg impl-reg)))

  (for ([flag flags])
    (define spec-flag (cpu-flag-ref spec-cpu flag))
    (define impl-flag (cpu-flag-ref attempt-cpu flag))
    (assert (bveq spec-flag impl-flag)))
  )

(define (run-verifier transform)
  (displayln "running verification...")
  (define spec-cpu (comp-simp:make-x86-64-cpu))
  (define attempt-cpu (comp-simp:make-x86-64-cpu))
  (define attempt-cpu-copy 
    (struct-copy cpu attempt-cpu
                 [gprs (vector-copy (cpu-gprs attempt-cpu))]
                 [flags (vector-copy (cpu-flags attempt-cpu))]))
  (define cex (verify (comp-simp-verify (car transform) attempt-cpu
                                        (cdr transform) spec-cpu)))
  (define short-result (if (unsat? cex) "unsat" "sat"))
  (displayln (format "finished verification (~s)" short-result))
  (when (print-verbose)
    (displayln cex)
    (displayln ""))
  (when (sat? cex)
    (displayln "Solver found a counterexample. Identifying failures...")
    (comp-simp:run-x86-64-impl #:insns (car transform) #:cpu attempt-cpu-copy #:assert-cs true
      #:verbose (print-verbose) #:model cex)
    (displayln "done\n"))
  short-result)

(define print-verbose (make-parameter #f))

(define insns-to-transform
  (command-line
   #:once-each
   [("-v" "--verbose") "Print verbose verification results instead of simply sat/unsat"
                       (print-verbose #t)]
   #:args insns
   insns))

(module+ main
  ; (define cex (verify (comp-simp-verify attempt-mul16-p12 spec-mul16-p12 (list ax cx))))
  (define results
    (for/list ([insn insns-to-transform])
      (displayln (format "Attempting to verify ~s" insn))
      (define transforms (comp-simp-transform insn))
      (define res
        (if (list? transforms)
          (map run-verifier transforms)
          "N/A"))
      (when (equal? res "N/A")
        (displayln (format "Could not find transforms for ~s" insn)))
      (displayln "")
      res))
  (displayln (format "Results for ~a:" insns-to-transform))
  results)
