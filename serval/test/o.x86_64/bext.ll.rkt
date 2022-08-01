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

(define (@bext %rs1 %rs2)
; %entry
  (define-label (%entry) #:merge #f
    (br %cont))

; %for.cond.cleanup
  (define-label (%for.cond.cleanup) #:merge #f
    (ret %r.2))

; %cont
  (define-label (%cont) #:merge #f
    (set! %j.024 (phi [(bv #x00000000 32) %entry] [%j.1 %for.inc]))
    (set! %i.023 (phi [(bv #x00000000 32) %entry] [%9 %for.inc]))
    (set! %r.022 (phi [(bv #x00000000 32) %entry] [%r.2 %for.inc]))
    (set! %0 (shl (bv #x00000001 32) %i.023))
    (set! %1 (and %0 %rs2))
    (set! %tobool.not (icmp/eq %1 (bv #x00000000 32)))
    (br %tobool.not %for.inc %cont2))

; %cont2
  (define-label (%cont2) #:merge #f
    (set! %2 (and %0 %rs1))
    (set! %tobool5.not (icmp/eq %2 (bv #x00000000 32)))
    (br %tobool5.not %if.end %if.then6))

; %if.then6
  (define-label (%if.then6) #:merge #f
    (set! %3 (icmp/ult %j.024 (bv #x00000020 32)))
    (br %3 %cont8 %handler.shift_out_of_bounds7))

; %handler.shift_out_of_bounds7
  (define-label (%handler.shift_out_of_bounds7) #:merge #f
    (set! %4 (zext %j.024 (bitvector 64)))
    (__ubsan_handle_shift_out_of_bounds (list (list #"bext.c\0" 13 37) (list 0 10 #"'uint_xlen_t' (aka 'unsigned int')\0") (list 0 11 #"'int'\0")) (bv #x0000000000000001 64) %4)
    (br %cont8))

; %cont8
  (define-label (%cont8) #:merge #f
    (set! %shl (shl (bv #x00000001 32) %j.024))
    (set! %or (or %shl %r.022))
    (br %if.end))

; %if.end
  (define-label (%if.end) #:merge #f
    (set! %r.1 (phi [%or %cont8] [%r.022 %cont2]))
    (set! %5 (call llvm.sadd.with.overflow.i32 %j.024 (bv #x00000001 32)))
    (set! %6 (extractvalue %5 0))
    (set! %7 (extractvalue %5 1))
    (br %7 %handler.add_overflow %for.inc))

; %handler.add_overflow
  (define-label (%handler.add_overflow) #:merge #f
    (set! %8 (zext %j.024 (bitvector 64)))
    (__ubsan_handle_add_overflow (list (list #"bext.c\0" 14 14) (list 0 11 #"'int'\0")) %8 (bv #x0000000000000001 64))
    (br %for.inc))

; %for.inc
  (define-label (%for.inc) #:merge #f
    (set! %r.2 (phi [%r.022 %cont] [%r.1 %handler.add_overflow] [%r.1 %if.end]))
    (set! %j.1 (phi [%j.024 %cont] [%6 %handler.add_overflow] [%6 %if.end]))
    (set! %9 (add %i.023 (bv #x00000001 32)))
    (set! %exitcond.not (icmp/eq %9 (bv #x00000020 32)))
    (br %exitcond.not %for.cond.cleanup %cont))

  (define-value %j.024)
  (define-value %i.023)
  (define-value %r.022)
  (define-value %0)
  (define-value %1)
  (define-value %tobool.not)
  (define-value %2)
  (define-value %tobool5.not)
  (define-value %3)
  (define-value %4)
  (define-value %shl)
  (define-value %or)
  (define-value %r.1)
  (define-value %5)
  (define-value %6)
  (define-value %7)
  (define-value %8)
  (define-value %r.2)
  (define-value %j.1)
  (define-value %9)
  (define-value %exitcond.not)
  (enter! %entry))


