#lang rosette

(require "synth-comp-simp-defenses-macrod.rkt")

;; Helpers for testing

; Just the regs used in comp simp testing
(define reg64s (list rax rcx rdx rdi))
(define reg32s (list eax ecx edx edi))
(define reg16s (list ax cx dx di))
(define reg8s (list al cl dl))

(define (get-special-imms bitwidth)
  (define special-ints (list 1 0 -1 256 700))
  (for/list ([n special-ints])
    (bv (modulo n bitwidth) bitwidth)))

(define special-imms-for-8 (get-special-imms 8))
(define special-imms-for-16 (get-special-imms 16))
(define special-imms-for-32 (get-special-imms 32))
(define special-imms-for-64 (get-special-imms 64))
(printf "special-imms-for-8: ~a\n" special-imms-for-8)
(printf "special-imms-for-16: ~a\n" special-imms-for-16)
(printf "special-imms-for-32: ~a\n" special-imms-for-32)
(printf "special-imms-for-64: ~a\n" special-imms-for-64)

(define arg-types '(r32-i32 
                    r64-i32
                    r32-i8
                    r64-i8
                    r32-r32
                    r64-r64
                    r8
                    r16
                    r32
                    r64))
(printf "arg-types ~a\n" arg-types)

; quick sanity check--fail if i accidentally
; added a dupe arg type. it would create a lot of
; extra work for the tester
(when (check-duplicates arg-types)
    (raise (format "Duplicate arg type: ~a\n" 
                    (check-duplicates arg-types))))

(define (get-all-arg-list-for-type argtype)
  (unless (member argtype arg-types)
    (raise (format "Unrecognized type: ~a\n" argtype)))
    
  (match argtype
    ['r32-i32 (cartesian-product reg32s special-imms-for-32)]
    ['r64-i32 (cartesian-product reg64s special-imms-for-32)]
    ['r32-i8 (cartesian-product reg32s special-imms-for-8)]
    ['r64-i8 (cartesian-product reg64s special-imms-for-8)]
    ['r32-r32 (cartesian-product reg32s reg32s)]
    ['r64-r64 (cartesian-product reg64s reg64s)]
    ['r8 reg8s]
    ['r16 reg16s]
    ['r32 reg32s]
    ['r64 reg64s]
    [_ (begin
        (printf "Uncaught type in get-all-arg-list-for-type: ~a\n" argtype)
        (exit 1))]))

(define insn-to-type-map 
    (list
        (cons sub-r/m32-imm32 'r32-i32)
        (cons sub-r/m64-imm32 'r64-i32)
        (cons sub-r/m32-imm8 'r32-i8)
        (cons sub-r/m64-imm8 'r64-i8)
        (cons sub-r/m32-r32 'r32-r32)
        (cons sub-r/m64-r64 'r64-r64)
        (cons setz 'r8)
        (cons cmovz-r32-r32 'r32-r32)
        (cons cmovne-r32-r32 'r32-r32)
        (cons mul-r/m32 'r32)
        (cons mul-r/m64 'r64)))

; another sanity check that all types are in the
; arg-types list
(for ([pair insn-to-type-map])
    (match-let ([(cons insn type) pair])
        (unless (member type arg-types)
            (raise (format "Unrecognized type: ~a\n" type)))))
