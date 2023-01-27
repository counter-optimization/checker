#lang rosette

(require
  "common.rkt")

(provide
 bt-r/m64-imm8)

(define (interp-bt cpu src-reg offset)
  (define v1 (cpu-gpr-ref cpu src-reg))
  (define result (bit offset v1))
  (cpu-flag-set! cpu 'CF result))

; REX.W + 0F BA /4 ib
; decode/encode are probably wrong; only used for verification
(define-insn bt-r/m64-imm8 (src imm8)
  #:decode [((rex.w/r b) (byte #x0f) (byte #xba) (/4 r/m) i0)
            (list (gpr64 b r/m) i0)]
  #:encode (list (rex.w/r src) (byte #x0f) (byte #xba) (/4 src) (encode-imm imm8))
  (lambda (cpu src imm8)
    (interp-bt cpu src (bitvector->integer imm8))))
