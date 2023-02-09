#lang rosette

(require
  "common.rkt")

(provide
 sahf)

(define (interp-sahf cpu)
  (define v1 (cpu-gpr-ref cpu ah))
  (cpu-flag-set! cpu 'SF (bit 7 v1))
  (cpu-flag-set! cpu 'ZF (bit 6 v1))
  (cpu-flag-set! cpu 'AF (bit 4 v1))
  (cpu-flag-set! cpu 'PF (bit 2 v1))
  (cpu-flag-set! cpu 'CF (bit 0 v1)))

; 9E
(define-insn sahf ()
  #:decode [((byte #x9e))
            (list)]
  #:encode (list (byte #x9e))
  (lambda (cpu)
    (interp-sahf cpu)))
