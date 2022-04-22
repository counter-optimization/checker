; DO NOT MODIFY.
;
; This file was automatically generated.

#lang rosette

(provide (all-defined-out))

(require (prefix-in core: serval/lib/core)
         
         serval/llvm
         serval/ubsan)

(define-global @val)
(define-global @vals)

(define (@get_value)
; %entry
  (define-label (%entry) #:merge #f
    (set! %0 (load @val (bitvector 64) #:align 8))
    (ret %0))

  (define-value %0)
  (enter! %entry))

(define (@get_value_i %i)
; %entry
  (define-label (%entry) #:merge #f
    (set! %cmp (icmp/ult %i (bv #x0000000000001000 64)))
    (br %cmp %cond.true %cond.end))

; %cond.true
  (define-label (%cond.true) #:merge #f
    (set! %arrayidx (getelementptr @vals (array-offset (bv #x0000000000000000 64) 16384) (array-offset %i 4)))
    (set! %0 (load %arrayidx (bitvector 32) #:align 4))
    (br %cond.end))

; %cond.end
  (define-label (%cond.end) #:merge #f
    (set! %cond (phi [%0 %cond.true] [(bv #xffffffff 32) %entry]))
    (ret %cond))

  (define-value %cmp)
  (define-value %arrayidx)
  (define-value %0)
  (define-value %cond)
  (enter! %entry))

(define (@set_value %x)
; %entry
  (define-label (%entry) #:merge #f
    (store %x @val (bitvector 64) #:align 8)
    (ret))

  (enter! %entry))

(define (@set_value_i %i %x)
; %entry
  (define-label (%entry) #:merge #f
    (set! %cmp (icmp/ult %i (bv #x0000000000001000 64)))
    (br %cmp %if.then %if.end))

; %if.then
  (define-label (%if.then) #:merge #f
    (set! %arrayidx (getelementptr @vals (array-offset (bv #x0000000000000000 64) 16384) (array-offset %i 4)))
    (store %x %arrayidx (bitvector 32) #:align 4)
    (br %if.end))

; %if.end
  (define-label (%if.end) #:merge #f
    (ret))

  (define-value %cmp)
  (define-value %arrayidx)
  (enter! %entry))


