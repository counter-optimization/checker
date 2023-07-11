#lang rosette

(require
  "common.rkt")

(provide
  not-r/m8)

(define (interpret-not cpu dst)
  (define v (cpu-gpr-ref cpu dst))
  (define res (bvnot v))
  (cpu-gpr-set! cpu dst res))

;; todo, fix decode, encode. these are neg's encode/decode
(define-insn not-r/m8 (dst)
  #:decode [((byte #xF7) (/3 r/m))
            (list (gpr32-no-rex r/m))]
           [((rex/r b) (byte #xF7) (/3 r/m))
            (list (gpr32 b r/m))]
  #:encode (list (rex/r dst) (byte #xF7) (/3 dst))
  interpret-neg)


