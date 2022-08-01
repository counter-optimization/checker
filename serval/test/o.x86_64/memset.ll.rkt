; DO NOT MODIFY.
;
; This file was automatically generated.

#lang rosette

(provide (all-defined-out))

(require (prefix-in core: serval/lib/core)
         
         serval/llvm
         serval/ubsan)

(define-global @buffer)
(define-global @a)
(define-global @pages)

(define (@mret)
; %entry
  (define-label (%entry) #:merge #f
    (asm 'mret)
    (unreachable))

  (enter! %entry))

(define (@test_byte_buffer)
; %entry
  (define-label (%entry) #:merge #f
    (set! %call (call memset (getelementptr @buffer (array-offset (bv #x0000000000000000 64) 10) (array-offset (bv #x0000000000000000 64) 1)) (bv #x00000000 32) (bv #x000000000000000a 64)))
    (ret))

  (define-value %call)
  (enter! %entry))

(define (@test_a0)
; %entry
  (define-label (%entry) #:merge #f
    (set! %call (call memset (bitcast @a) (bv #x00000000 32) (bv #x000000000000000c 64)))
    (ret))

  (define-value %call)
  (enter! %entry))

(define (@test_ai %i)
; %entry
  (define-label (%entry) #:merge #f
    (set! %cmp (icmp/ult %i (bv #x000000000000000a 64)))
    (br %cmp %if.then %if.end))

; %if.then
  (define-label (%if.then) #:merge #f
    (set! %arrayidx (getelementptr @a (array-offset (bv #x0000000000000000 64) 120) (array-offset %i 12)))
    (set! %0 (bitcast %arrayidx))
    (set! %call (call memset %0 (bv #x00000000 32) (bv #x000000000000000c 64)))
    (br %if.end))

; %if.end
  (define-label (%if.end) #:merge #f
    (ret))

  (define-value %cmp)
  (define-value %arrayidx)
  (define-value %0)
  (define-value %call)
  (enter! %entry))

(define (@test_an)
; %entry
  (define-label (%entry) #:merge #f
    (set! %call (call memset (bitcast @a) (bv #x00000000 32) (bv #x0000000000000078 64)))
    (ret))

  (define-value %call)
  (enter! %entry))

(define (@test_b %i)
; %entry
  (define-label (%entry) #:merge #f
    (set! %cmp (icmp/ult %i (bv #x000000000000000a 64)))
    (br %cmp %if.then %if.end))

; %if.then
  (define-label (%if.then) #:merge #f
    (set! %b (getelementptr @a (array-offset (bv #x0000000000000000 64) 120) (array-offset %i 12) (struct-offset 4)))
    (set! %0 (bitcast %b))
    (set! %call (call memset %0 (bv #x00000000 32) (bv #x0000000000000008 64)))
    (br %if.end))

; %if.end
  (define-label (%if.end) #:merge #f
    (ret))

  (define-value %cmp)
  (define-value %b)
  (define-value %0)
  (define-value %call)
  (enter! %entry))

(define (@test_b_z %i)
; %entry
  (define-label (%entry) #:merge #f
    (set! %cmp (icmp/ult %i (bv #x000000000000000a 64)))
    (br %cmp %if.then %if.end))

; %if.then
  (define-label (%if.then) #:merge #f
    (set! %z (getelementptr @a (array-offset (bv #x0000000000000000 64) 120) (array-offset %i 12) (struct-offset 4) (struct-offset 4)))
    (set! %0 (bitcast %z))
    (set! %call (call memset %0 (bv #x00000000 32) (bv #x0000000000000004 64)))
    (br %if.end))

; %if.end
  (define-label (%if.end) #:merge #f
    (ret))

  (define-value %cmp)
  (define-value %z)
  (define-value %0)
  (define-value %call)
  (enter! %entry))

(define (@test_pages %lower %upper)
; %entry
  (define-label (%entry) #:merge #f
    (set! %cmp (icmp/uge %upper %lower))
    (set! %cmp1 (icmp/ult %upper (bv #x000000000000000b 64)))
    (set! %or.cond (and %cmp %cmp1))
    (br %or.cond %if.then %if.end))

; %if.then
  (define-label (%if.then) #:merge #f
    (set! %0 (getelementptr @pages (array-offset (bv #x0000000000000000 64) 40960) (array-offset %lower 4096) (array-offset (bv #x0000000000000000 64) 1)))
    (set! %sub (sub %upper %lower))
    (set! %mul (shl %sub (bv #x000000000000000c 64)))
    (set! %call (call memset %0 (bv #x00000000 32) %mul))
    (br %if.end))

; %if.end
  (define-label (%if.end) #:merge #f
    (ret))

  (define-value %cmp)
  (define-value %cmp1)
  (define-value %or.cond)
  (define-value %0)
  (define-value %sub)
  (define-value %mul)
  (define-value %call)
  (enter! %entry))

(define (@test_buggy_too_large_a)
; %entry
  (define-label (%entry) #:merge #f
    (set! %call (call memset (bitcast @a) (bv #x00000000 32) (bv #x0000000000000084 64)))
    (ret))

  (define-value %call)
  (enter! %entry))

(define (@test_buggy_too_large_b)
; %entry
  (define-label (%entry) #:merge #f
    (set! %call (call memset (bitcast (getelementptr @a (array-offset (bv #x0000000000000000 64) 120) (array-offset (bv #x0000000000000000 64) 12) (struct-offset 4) (struct-offset 4))) (bv #x00000000 32) (bv #x0000000000000008 64)))
    (ret))

  (define-value %call)
  (enter! %entry))

(define (@test_buggy_out_of_bounds %i)
; %entry
  (define-label (%entry) #:merge #f
    (set! %arrayidx (getelementptr @a (array-offset (bv #x0000000000000000 64) 120) (array-offset %i 12)))
    (set! %0 (bitcast %arrayidx))
    (set! %call (call memset %0 (bv #x00000000 32) (bv #x000000000000000c 64)))
    (ret))

  (define-value %arrayidx)
  (define-value %0)
  (define-value %call)
  (enter! %entry))


