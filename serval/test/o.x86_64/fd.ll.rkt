; DO NOT MODIFY.
;
; This file was automatically generated.

#lang rosette

(provide (all-defined-out))

(require (prefix-in core: serval/lib/core)
         
         serval/llvm
         serval/ubsan)

(define-global @procs)
(define-global @current)
(define-global @files)

(define (@close %fd)
; %entry
  (define-label (%entry) #:merge #f
    (set! %0 (icmp/ugt %fd (bv #x000000000000000f 64)))
    (br %0 %cleanup %if.end))

; %if.end
  (define-label (%if.end) #:merge #f
    (set! %1 (load @current (bitvector 64) #:align 8))
    (set! %arrayidx2 (getelementptr @procs (array-offset (bv #x0000000000000000 64) 8192) (array-offset %1 128) (struct-offset 0) (array-offset %fd 8)))
    (set! %2 (load %arrayidx2 (bitvector 64) #:align 8))
    (set! %cmp3 (icmp/ugt %2 (bv #x000000000000007f 64)))
    (br %cmp3 %cleanup %if.end5))

; %if.end5
  (define-label (%if.end5) #:merge #f
    (set! %refcount (getelementptr @files (array-offset (bv #x0000000000000000 64) 1024) (array-offset %2 8) (struct-offset 0)))
    (set! %3 (load %refcount (bitvector 64) #:align 8))
    (set! %dec (add %3 (bv #xffffffffffffffff 64)))
    (store %dec %refcount (bitvector 64) #:align 8)
    (br %cleanup))

; %cleanup
  (define-label (%cleanup) #:merge #f
    (set! %retval.0 (phi [(bv #x00000000 32) %if.end5] [(bv #xffffffff 32) %entry] [(bv #xffffffff 32) %if.end]))
    (ret %retval.0))

  (define-value %0)
  (define-value %1)
  (define-value %arrayidx2)
  (define-value %2)
  (define-value %cmp3)
  (define-value %refcount)
  (define-value %3)
  (define-value %dec)
  (define-value %retval.0)
  (enter! %entry))


