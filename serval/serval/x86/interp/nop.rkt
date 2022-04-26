#lang rosette

(require "common.rkt")

(provide
 nop-7-byte)

(define-insn nop-7-byte ()
  #:decode [((byte #x0f) (byte #x1f) (byte #x80) (byte #x00) (byte #x00) (byte #x00) (byte #x00))
            (nop-7-byte)]
  #:encode (list (bv #x0f 8) (bv #x1f 8) (bv #x80 8) (bv #x00 8) (bv #x00 8) (bv #x00 8) (bv #x00 8))
  (lambda (cpu) (void)))