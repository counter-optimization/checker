#lang rosette

(require 
  racket/cmdline
  rosette/lib/synthax
  rosette/lib/match
  rosette/solver/smt/boolector
  "../serval/serval/x86.rkt"
  "transform-list.rkt"
  "arith-transforms.rkt"
  "bitwise-transforms.rkt"
  "shift-transforms.rkt"
  "mul-transforms.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

(provide (all-defined-out))

(define (comp-simp-verify attempt attempt-cpu spec spec-cpu)
  (comp-simp:assume-all-regs-equiv spec-cpu attempt-cpu)
  (comp-simp:assume-all-flags-equiv spec-cpu attempt-cpu)

  (comp-simp:run-x86-64-impl #:insns attempt #:cpu attempt-cpu #:assert-cs false
                             #:verbose (print-verbose))
  (comp-simp:run-x86-64-impl #:insns spec #:cpu spec-cpu)

  (comp-simp:assert-all-regs-but-scratch-equiv spec-cpu attempt-cpu)

  (when (check-flags)
    (comp-simp:assert-all-flags-equiv spec-cpu attempt-cpu))
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
  (displayln (format "finished verification (~s)\n" short-result))
  (when (print-verbose)
    (displayln cex)
    (displayln ""))
  (when (sat? cex)
    (displayln "Solver found a counterexample. Searching for comp simp failures...")
    (comp-simp:run-x86-64-impl #:insns (car transform) #:cpu attempt-cpu-copy #:assert-cs true
      #:verbose (print-verbose) #:model cex)
    (displayln "\nChecking register equivalence...")
    (comp-simp:compare-all-regs-but-scratch (evaluate spec-cpu cex)
                                            (evaluate attempt-cpu-copy cex))
    (when (check-flags)
      (displayln "\nChecking flag equivalence...")
      (comp-simp:compare-all-flags (evaluate spec-cpu cex) (evaluate attempt-cpu-copy cex)))
    (displayln "done\n"))
  short-result)

(define print-verbose (make-parameter #f))
(define check-flags (make-parameter #f))

(define insns-to-transform
  (command-line
   #:once-each
   [("-v" "--verbose") "Print verbose verification results instead of simply sat/unsat"
                       (print-verbose #t)]
   [("-f" "--check-flags") "Check equivalence of flags in addition to registers"
                           (check-flags #t)]
   #:args insns
   insns))

(module+ main
  (current-solver (boolector))
  (displayln (current-solver))
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
