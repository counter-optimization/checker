#lang rosette

(require
  "common.rkt")

(provide
 clc)

(define (interp-clc cpu)
  (cpu-flag-set! cpu 'CF (bit 0 (bv 0 8))))

; REX.W + 0F BA /4 ib
; decode/encode are probably wrong; only used for verification
(define-insn clc ()
  #:decode [((byte #xf8)) '()]
  #:encode (list (byte #xf8))
  (lambda (cpu) (interp-clc cpu)))
