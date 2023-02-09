#lang rosette

(require
  "common.rkt")

(provide
 lahf)

(define (interp-lahf cpu)
  (define sf (cpu-flag-ref cpu 'SF))
  (define zf (cpu-flag-ref cpu 'ZF))
  (define af (cpu-flag-ref cpu 'AF))
  (define pf (cpu-flag-ref cpu 'PF))
  (define cf (cpu-flag-ref cpu 'CF))
  (define result (concat sf zf (bv 0 1) af (bv 0 1) pf (bv 1 1) cf))
  (cpu-gpr-set! cpu ah result))

; 9f
(define-insn lahf ()
  #:decode [((byte #x9f))
            (list)]
  #:encode (list (byte #x9f))
  (lambda (cpu)
    (interp-lahf cpu)))
