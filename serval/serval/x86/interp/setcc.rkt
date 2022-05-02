#lang rosette

(require
  "common.rkt")

(provide
 (all-defined-out))

(define (interp-setcc cpu dst-reg cond)
  (cpu-gpr-set! cpu dst-reg (zero-extend (bool->bitvector cond) (bitvector 8))))

; 0f 9d c0
(define-insn set-ge-r8 (dst-reg)
  #:decode [((byte #x0f) (byte #x9d) (/0 r/m))
            (list (gpr8-no-rex r/m))]
  #:encode (list (byte #x0f) (byte #x9d) (/0 dst-reg))
  (lambda (cpu dst-reg)
    (define of (cpu-flag-ref cpu 'OF))
    (define sf (cpu-flag-ref cpu 'SF))
    (interp-setcc cpu dst-reg (bveq of sf))))

; 0f 9c c0
(define-insn set-l-r8 (dst-reg)
  #:decode [((byte #x0f) (byte #x9c) (/0 r/m))
            (list (gpr8-no-rex r/m))]
  #:encode (list (byte #x0f) (byte #x9c) (/0 dst-reg))
  (lambda (cpu dst-reg)
    (define of (cpu-flag-ref cpu 'OF))
    (define sf (cpu-flag-ref cpu 'SF))
    (interp-setcc cpu dst-reg (! (bveq of sf)))))

; 0f 95 c1
(define-insn set-ne-r8 (dst-reg)
  #:decode [((byte #x0f) (byte #x95) (/0 r/m))
            (list (gpr8-no-rex r/m))]
  #:encode (list (byte #x0f) (byte #x95) (/0 dst-reg))
  (lambda (cpu dst-reg)
    (interp-setcc cpu dst-reg (! (bvzero? (cpu-flag-ref cpu 'ZF))))))
