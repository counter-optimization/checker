#lang rosette

(require
  "common.rkt")

(provide
  movsxd-r/m32-r64)

; REX.W + 63 /r
(define-insn movsxd-r/m32-r64 (dst src)
  #:decode [((rex.w) (byte #x63) (/r reg r/m))
            (list (gpr64-no-rex r/m) (gpr32-no-rex reg))]
  #:encode (list (rex.w) (byte #x63) (/r (encode-gpr-modr/m src) (encode-gpr-modr/m dst)))
  (lambda (cpu dst src)
    (define src-contents (cpu-gpr-ref cpu src))
    (define sign-extended (sign-extend src-contents (bitvector 64)))
    (cpu-gpr-set! cpu dst sign-extended)))