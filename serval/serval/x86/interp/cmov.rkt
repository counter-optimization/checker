#lang rosette

(require
  "common.rkt")

(provide 
  cmovz-r32-r32
  cmovne-r32-r32)

; cnd should be a bool
; dst and src should be gpr structs as defined in
; x86/register.rkt
(define (interp-cmov cpu dst src cnd)
  (cpu-gpr-set! cpu dst (if cnd
                            (cpu-gpr-ref cpu src)
                            (cpu-gpr-ref cpu dst))))

(define-insn cmovz-r32-r32 (dst src)
  #:decode [((byte #x0f) (byte #x44) (/r reg r/m))
            (list (gpr32-no-rex reg) (gpr32-no-rex r/m))]
  #:encode [(list (byte #x0f) (byte #x44) (/r dst src))]
  (lambda (cpu dst src)
    (define zf (cpu-flag-ref cpu 'ZF))
    (interp-cmov cpu dst src (! (bvzero? zf)))))


(define-insn cmovne-r32-r32 (dst src)
  #:decode [((byte #x0f) (byte #x45) (/r reg r/m))
            (list (gpr32-no-rex reg) (gpr32-no-rex r/m))]
  #:encode [(list (byte #x0f) (byte #x45) (/r dst src))]
  (lambda (cpu dst src)
    (define zf (cpu-flag-ref cpu 'ZF))
    (interp-cmov cpu dst src (bvzero? zf))))
