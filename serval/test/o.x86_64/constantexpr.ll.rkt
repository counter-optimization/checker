; DO NOT MODIFY.
;
; This file was automatically generated.

#lang rosette

(provide (all-defined-out))

(require (prefix-in core: serval/lib/core)
         
         serval/llvm
         serval/ubsan)

(define-global @a)

(define (@set_x %x)
; %entry
  (define-label (%entry) #:merge #f
    (store %x (getelementptr @a (array-offset (bv #x0000000000000000 64) 8) (struct-offset 0)) (bitvector 32) #:align 4)
    (ret))

  (enter! %entry))


