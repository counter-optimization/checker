; DO NOT MODIFY.
;
; This file was automatically generated.

#lang rosette

(provide (all-defined-out))

(require (prefix-in core: serval/lib/core)
         
         serval/llvm
         serval/ubsan)

(define-global @as)
(define-global @arr)

(define (@test %x)
; %entry
  (define-label (%entry) #:merge #f
    (set! %rem (and %x (bv #x00000003 32)))
    (set! %idxprom (zext %rem (bitvector 64)))
    (set! %y (getelementptr @as (array-offset (bv #x0000000000000000 64) 32) (array-offset %idxprom 8) (struct-offset 4)))
    (store (bv #x00000009 32) %y (bitvector 32) #:align 4)
    (set! %add (add %x (bv #x00000001 32)))
    (set! %rem6 (and %add (bv #x00000003 32)))
    (set! %idxprom7 (zext %rem6 (bitvector 64)))
    (set! %arrayidx8 (getelementptr @arr (array-offset (bv #x0000000000000000 64) 16) (array-offset %idxprom7 4)))
    (store (bv #x00000009 32) %arrayidx8 (bitvector 32) #:align 4)
    (ret (bv #x00000009 32)))

  (define-value %rem)
  (define-value %idxprom)
  (define-value %y)
  (define-value %add)
  (define-value %rem6)
  (define-value %idxprom7)
  (define-value %arrayidx8)
  (enter! %entry))


