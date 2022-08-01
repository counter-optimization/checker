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
(define-global @2)
(define-global @3)
(define-global @4)
(define-global @5)
(define-global @6)
(define-global @7)
(define-global @table)

(define (@add0 %x)
; %entry
  (define-label (%entry) #:merge #f
    (ret %x))

  (enter! %entry))

(define (@add1 %x)
; %entry
  (define-label (%entry) #:merge #f
    (set! %0 (call llvm.sadd.with.overflow.i64 %x (bv #x0000000000000001 64)))
    (set! %1 (extractvalue %0 1))
    (br %1 %handler.add_overflow %cont))

; %handler.add_overflow
  (define-label (%handler.add_overflow) #:merge #f
    (__ubsan_handle_add_overflow (list (list #"jumptable.c\0" 9 29) (list 0 13 #"'long'\0")) %x (bv #x0000000000000001 64))
    (br %cont))

; %cont
  (define-label (%cont) #:merge #f
    (set! %2 (extractvalue %0 0))
    (ret %2))

  (define-value %0)
  (define-value %1)
  (define-value %2)
  (enter! %entry))

(define (@add2 %x)
; %entry
  (define-label (%entry) #:merge #f
    (set! %0 (call llvm.sadd.with.overflow.i64 %x (bv #x0000000000000002 64)))
    (set! %1 (extractvalue %0 1))
    (br %1 %handler.add_overflow %cont))

; %handler.add_overflow
  (define-label (%handler.add_overflow) #:merge #f
    (__ubsan_handle_add_overflow (list (list #"jumptable.c\0" 10 29) (list 0 13 #"'long'\0")) %x (bv #x0000000000000002 64))
    (br %cont))

; %cont
  (define-label (%cont) #:merge #f
    (set! %2 (extractvalue %0 0))
    (ret %2))

  (define-value %0)
  (define-value %1)
  (define-value %2)
  (enter! %entry))

(define (@add3 %x)
; %entry
  (define-label (%entry) #:merge #f
    (set! %0 (call llvm.sadd.with.overflow.i64 %x (bv #x0000000000000003 64)))
    (set! %1 (extractvalue %0 1))
    (br %1 %handler.add_overflow %cont))

; %handler.add_overflow
  (define-label (%handler.add_overflow) #:merge #f
    (__ubsan_handle_add_overflow (list (list #"jumptable.c\0" 11 29) (list 0 13 #"'long'\0")) %x (bv #x0000000000000003 64))
    (br %cont))

; %cont
  (define-label (%cont) #:merge #f
    (set! %2 (extractvalue %0 0))
    (ret %2))

  (define-value %0)
  (define-value %1)
  (define-value %2)
  (enter! %entry))

(define (@add4 %x)
; %entry
  (define-label (%entry) #:merge #f
    (set! %0 (call llvm.sadd.with.overflow.i64 %x (bv #x0000000000000004 64)))
    (set! %1 (extractvalue %0 1))
    (br %1 %handler.add_overflow %cont))

; %handler.add_overflow
  (define-label (%handler.add_overflow) #:merge #f
    (__ubsan_handle_add_overflow (list (list #"jumptable.c\0" 12 29) (list 0 13 #"'long'\0")) %x (bv #x0000000000000004 64))
    (br %cont))

; %cont
  (define-label (%cont) #:merge #f
    (set! %2 (extractvalue %0 0))
    (ret %2))

  (define-value %0)
  (define-value %1)
  (define-value %2)
  (enter! %entry))

(define (@add5 %x)
; %entry
  (define-label (%entry) #:merge #f
    (set! %0 (call llvm.sadd.with.overflow.i64 %x (bv #x0000000000000005 64)))
    (set! %1 (extractvalue %0 1))
    (br %1 %handler.add_overflow %cont))

; %handler.add_overflow
  (define-label (%handler.add_overflow) #:merge #f
    (__ubsan_handle_add_overflow (list (list #"jumptable.c\0" 13 29) (list 0 13 #"'long'\0")) %x (bv #x0000000000000005 64))
    (br %cont))

; %cont
  (define-label (%cont) #:merge #f
    (set! %2 (extractvalue %0 0))
    (ret %2))

  (define-value %0)
  (define-value %1)
  (define-value %2)
  (enter! %entry))

(define (@add6 %x)
; %entry
  (define-label (%entry) #:merge #f
    (set! %0 (call llvm.sadd.with.overflow.i64 %x (bv #x0000000000000006 64)))
    (set! %1 (extractvalue %0 1))
    (br %1 %handler.add_overflow %cont))

; %handler.add_overflow
  (define-label (%handler.add_overflow) #:merge #f
    (__ubsan_handle_add_overflow (list (list #"jumptable.c\0" 14 29) (list 0 13 #"'long'\0")) %x (bv #x0000000000000006 64))
    (br %cont))

; %cont
  (define-label (%cont) #:merge #f
    (set! %2 (extractvalue %0 0))
    (ret %2))

  (define-value %0)
  (define-value %1)
  (define-value %2)
  (enter! %entry))

(define (@add7 %x)
; %entry
  (define-label (%entry) #:merge #f
    (set! %0 (call llvm.sadd.with.overflow.i64 %x (bv #x0000000000000007 64)))
    (set! %1 (extractvalue %0 1))
    (br %1 %handler.add_overflow %cont))

; %handler.add_overflow
  (define-label (%handler.add_overflow) #:merge #f
    (__ubsan_handle_add_overflow (list (list #"jumptable.c\0" 15 29) (list 0 13 #"'long'\0")) %x (bv #x0000000000000007 64))
    (br %cont))

; %cont
  (define-label (%cont) #:merge #f
    (set! %2 (extractvalue %0 0))
    (ret %2))

  (define-value %0)
  (define-value %1)
  (define-value %2)
  (enter! %entry))

(define (@init_table)
; %entry
  (define-label (%entry) #:merge #f
    (store @add0 (getelementptr @table (array-offset (bv #x0000000000000000 64) 64) (array-offset (bv #x0000000000000000 64) 8)) pointer #:align 16)
    (store @add1 (getelementptr @table (array-offset (bv #x0000000000000000 64) 64) (array-offset (bv #x0000000000000001 64) 8)) pointer #:align 8)
    (store @add2 (getelementptr @table (array-offset (bv #x0000000000000000 64) 64) (array-offset (bv #x0000000000000002 64) 8)) pointer #:align 16)
    (store @add3 (getelementptr @table (array-offset (bv #x0000000000000000 64) 64) (array-offset (bv #x0000000000000003 64) 8)) pointer #:align 8)
    (store @add4 (getelementptr @table (array-offset (bv #x0000000000000000 64) 64) (array-offset (bv #x0000000000000004 64) 8)) pointer #:align 16)
    (store @add5 (getelementptr @table (array-offset (bv #x0000000000000000 64) 64) (array-offset (bv #x0000000000000005 64) 8)) pointer #:align 8)
    (store @add6 (getelementptr @table (array-offset (bv #x0000000000000000 64) 64) (array-offset (bv #x0000000000000006 64) 8)) pointer #:align 16)
    (store @add7 (getelementptr @table (array-offset (bv #x0000000000000000 64) 64) (array-offset (bv #x0000000000000007 64) 8)) pointer #:align 8)
    (ret))

  (enter! %entry))

(define (@mret)
; %entry
  (define-label (%entry) #:merge #f
    (asm 'mret)
    (unreachable))

  (enter! %entry))

(define (@call_func %x %y)
; %entry
  (define-label (%entry) #:merge #f
    (set! %and (and %y (bv #x0000000000000007 64)))
    (set! %arrayidx (getelementptr @table (array-offset (bv #x0000000000000000 64) 64) (array-offset %and 8)))
    (set! %0 (load %arrayidx pointer #:align 8))
    (set! %call (call %0 %x))
    (ret %call))

  (define-value %and)
  (define-value %arrayidx)
  (define-value %0)
  (define-value %call)
  (enter! %entry))


