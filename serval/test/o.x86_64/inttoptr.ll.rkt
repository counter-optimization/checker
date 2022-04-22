; DO NOT MODIFY.
;
; This file was automatically generated.

#lang rosette

(provide (all-defined-out))

(require (prefix-in core: serval/lib/core)
         
         serval/llvm
         serval/ubsan)

(define-global @global)
(define-global @.src)
(define-global @0)
(define-global @1)
(define-global @pointer)
(define-global @2)

(define (@getglobal)
; %entry
  (define-label (%entry) #:merge #f
    (ret (ptrtoint @global #f)))

  (enter! %entry))

(define (@add1 %x)
; %entry
  (define-label (%entry) #:merge #f
    (set! %add (add %x (bv #x0000000000000001 64)))
    (ret %add))

  (define-value %add)
  (enter! %entry))

(define (@sub1 %x)
; %entry
  (define-label (%entry) #:merge #f
    (set! %sub (add %x (bv #xffffffffffffffff 64)))
    (ret %sub))

  (define-value %sub)
  (enter! %entry))

(define (@test1)
; %entry
  (define-label (%entry) #:merge #f
    (set! %call2 (call @sub1 (ptrtoint @global #f)))
    (set! %call3 (call @add1 %call2))
    (set! %0 (inttoptr %call3))
    (store (bv #x00000005 32) %0 (bitvector 32) #:align 4)
    (set! %call4 (call @add1 (ptrtoint @global #f)))
    (set! %call5 (call @sub1 %call4))
    (set! %1 (inttoptr %call5))
    (set! %2 (load %1 (bitvector 32) #:align 4))
    (set! %3 (call llvm.sadd.with.overflow.i32 %2 (bv #xfffffffb 32)))
    (set! %4 (extractvalue %3 1))
    (br %4 %handler.sub_overflow %cont))

; %handler.sub_overflow
  (define-label (%handler.sub_overflow) #:merge #f
    (set! %5 (zext %2 (bitvector 64)))
    (__ubsan_handle_sub_overflow (list (list #"inttoptr.c\0" 23 45) (list 0 11 #"'int'\0")) %5 (bv #x0000000000000005 64))
    (br %cont))

; %cont
  (define-label (%cont) #:merge #f
    (set! %6 (extractvalue %3 0))
    (ret %6))

  (define-value %call2)
  (define-value %call3)
  (define-value %0)
  (define-value %call4)
  (define-value %call5)
  (define-value %1)
  (define-value %2)
  (define-value %3)
  (define-value %4)
  (define-value %5)
  (define-value %6)
  (enter! %entry))

(define (@test2)
; %entry
  (define-label (%entry) #:merge #f
    (store (bv #x00000042 32) @global (bitvector 32) #:align 4)
    (store @global @pointer pointer #:align 8)
    (set! %0 (load @pointer pointer #:align 8))
    (set! %1 (load %0 (bitvector 32) #:align 4))
    (set! %2 (call llvm.sadd.with.overflow.i32 %1 (bv #xffffffbe 32)))
    (set! %3 (extractvalue %2 1))
    (br %3 %handler.sub_overflow %cont))

; %handler.sub_overflow
  (define-label (%handler.sub_overflow) #:merge #f
    (set! %4 (zext %1 (bitvector 64)))
    (__ubsan_handle_sub_overflow (list (list #"inttoptr.c\0" 32 21) (list 0 11 #"'int'\0")) %4 (bv #x0000000000000042 64))
    (br %cont))

; %cont
  (define-label (%cont) #:merge #f
    (set! %5 (extractvalue %2 0))
    (ret %5))

  (define-value %0)
  (define-value %1)
  (define-value %2)
  (define-value %3)
  (define-value %4)
  (define-value %5)
  (enter! %entry))


