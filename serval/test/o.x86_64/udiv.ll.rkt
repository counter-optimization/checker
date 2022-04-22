; DO NOT MODIFY.
;
; This file was automatically generated.

#lang rosette

(provide (all-defined-out))

(require (prefix-in core: serval/lib/core)
         
         serval/llvm
         serval/ubsan)

(define-global @.src)
(define-global @0)
(define-global @1)

(define (@udiv %x %y)
; %entry
  (define-label (%entry) #:merge #f
    (set! %cmp.not (icmp/eq %y (bv #x00000000 32)))
    (br %cmp.not %return %cont))

; %cont
  (define-label (%cont) #:merge #f
    (set! %div (udiv %x %y))
    (br %return))

; %return
  (define-label (%return) #:merge #f
    (set! %retval.0 (phi [%div %cont] [(bv #x00000000 32) %entry]))
    (ret %retval.0))

  (define-value %cmp.not)
  (define-value %div)
  (define-value %retval.0)
  (enter! %entry))

(define (@udiv_buggy %x %y)
; %entry
  (define-label (%entry) #:merge #f
    (set! %call (call @foo %x %y))
    (ret %call))

  (define-value %call)
  (enter! %entry))

(define (@foo %x %y)
; %entry
  (define-label (%entry) #:merge #f
    (set! %.not (icmp/eq %y (bv #x00000000 32)))
    (br %.not %handler.divrem_overflow %cont))

; %handler.divrem_overflow
  (define-label (%handler.divrem_overflow) #:merge #f
    (set! %0 (zext %x (bitvector 64)))
    (__ubsan_handle_divrem_overflow (list (list #"udiv.c\0" 13 14) (list 0 10 #"'unsigned int'\0")) %0 (bv #x0000000000000000 64))
    (br %cont))

; %cont
  (define-label (%cont) #:merge #f
    (set! %div (udiv %x %y))
    (ret %div))

  (define-value %.not)
  (define-value %0)
  (define-value %div)
  (enter! %entry))


