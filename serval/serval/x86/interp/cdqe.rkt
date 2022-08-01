#lang rosette

(require "common.rkt")

(provide
   cdqe)

(define zero-bit (bv #x0 1))

; REX.W 98
(define-insn cdqe ()
  #:decode [((rex.w _ _ _) (byte #x98))
            (cdqe)]
  #:encode (list (rex.w (bv 0 1) (bv 0 1) (bv 0 1)) (byte #x98))
  (lambda (cpu)
    (define eax-contents (cpu-gpr-ref cpu eax))
    (define sign-extended (sign-extend eax-contents (bitvector 64)))
    (cpu-gpr-set! cpu rax sign-extended)))
