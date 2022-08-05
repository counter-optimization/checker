#lang rosette

(require 
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

;; Helpers for testing
; Just the regs used in comp simp testing
(define reg64s (list rax rcx rdx rdi))
(define reg32s (list eax ecx edx edi))
(define reg16s (list ax cx dx di))
(define reg8s (list al cl dl))

(define (get-special-imms bitwidth)
  (define special-ints 
    (list 1 
          0 
          -1 
          (sub1 (expt 2 8)) 
          (sub1 (expt 2 16))
          (sub1 (expt 2 32))
          (sub1 (expt 2 64))))
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

  (define (wrap-plain-list-in-lists xs)
    (for/list ([x xs])
      (list x)))
    
  (match argtype
    ['r32-i32 (cartesian-product reg32s special-imms-for-32)]
    ['r64-i32 (cartesian-product reg64s special-imms-for-32)]
    ['r32-i8 (cartesian-product reg32s special-imms-for-8)]
    ['r64-i8 (cartesian-product reg64s special-imms-for-8)]
    ['r32-r32 (cartesian-product reg32s reg32s)]
    ['r64-r64 (cartesian-product reg64s reg64s)]
    ['r8 (wrap-plain-list-in-lists reg8s)]
    ['r16 (wrap-plain-list-in-lists reg16s)]
    ['r32 (wrap-plain-list-in-lists reg32s)]
    ['r64 (wrap-plain-list-in-lists reg64s)]
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


;; Generate every single instruction with every combo of 
;; registers and immediates (for some small predefined set
;; of special immediates, see definition `get-special-imms`)
(define insns-to-test '())

(for ([insn-type-pair insn-to-type-map])
  (match-let ([(cons insn arg-types) insn-type-pair])
    (define all-args (get-all-arg-list-for-type arg-types))
    (for ([arg-list all-args])
      (set! insns-to-test (cons (apply insn arg-list) insns-to-test)))))

;; print the insns to test to for a quick visual check
; (displayln "insns-to-test: ")
; (for ([insn insns-to-test])
;   (displayln insn))

;; Generating test code for an insn

; Generate the 'spec' insn sequence for an insn.
; The test synthesizer will be tasked to come up 
; with an equivalent instruction sequence. Since
; these are just insn sequences containing just one
; instruction, it should be able to synthesize an equiv
; insn sequence of length one. Note that the synthesized
; insn is not necessarily the same instruction as in the
; spec. For example, for some immediate values < 2^8-1,
; sub-r/m32-imm32 and sub-r/m32-imm8 may overlap. This is
; why in the definition for `get-special-imms`, there are 
; special values at UINT8_MAX, UINT16_MAX, UINT32_MAX, and
; UINT64_MAX. Hopefully, these will exercise all 'paths'
; in the sub synth grammar.

; Here, spec refers to the concrete insn
; sequence we are using as the reference
; implementation. anything prefixed with 
; impl is used by the synthesized insn sequence.
(define (run-and-check-synthesis #:spec-cpu spec-cpu
                       #:spec-insns spec-insns
                       #:impl-cpu impl-cpu
                       #:apply-asserts [apply-asserts #f])
  ; the synthesizer AST to choose from. insn sequence
  ; of length exactly 1
  (define impl-insns (comp-simp:generate-sub-insns 1))

  ; start the cpus in the same initial state
  (comp-simp:assume-all-flags-equiv spec-cpu impl-cpu)
  (comp-simp:assume-all-regs-equiv spec-cpu impl-cpu)

  (when apply-asserts
    (comp-simp:apply-insn-specific-asserts #:insns impl-insns
                                           #:asserter comp-simp:comp-simp-asserter
                                           #:cpu impl-cpu))
  
  ; Run the synthesized code
  (comp-simp:run-x86-64-impl #:insns impl-insns #:cpu impl-cpu)

  ; Run the spec code
  (comp-simp:run-x86-64-impl #:insns spec-insns #:cpu spec-cpu)

  ; Check that the spec and synth code compute the same 'function'
  (comp-simp:assert-all-regs-equiv spec-cpu impl-cpu)
  (comp-simp:assert-all-flags-equiv spec-cpu impl-cpu))

;; Run all of the individual insn synth tests
(module+ main
  (printf "Running all of the individual insn synth tests...\n")
  (define failed '())
  (define succeeded '())

  (for ([insn insns-to-test])
    (printf "---------------------\nTesting insn: ~a\n" insn)
    (define start-time (current-milliseconds))

    (define spec-cpu (comp-simp:make-x86-64-cpu))
    (define impl-cpu (comp-simp:make-x86-64-cpu))
    (define spec-for-insn (list insn))

    (define result
      (synthesize
      #:forall (append 
                  (comp-simp:forall-quantified-vars impl-cpu)
                  (comp-simp:forall-quantified-vars spec-cpu))
      #:guarantee (run-and-check-synthesis #:spec-cpu spec-cpu
                                           #:spec-insns spec-for-insn
                                           #:impl-cpu impl-cpu
                                           #:apply-asserts #f)))

    (if (sat? result)
        (begin
          (printf "Successfully synthesized insn sequence\n")
          (printf "Solution was: ~a\n" result)
          (printf "Spec insn was: ~a\n" (car spec-for-insn))
          (set! succeeded (cons insn succeeded)))
        (begin
          (printf "FAILURE: Could not synthesize insn sequence\n")
          (printf "Insn was: ~a\n" (car spec-for-insn))
          (set! failed (cons insn failed))))

    (define end-time (current-milliseconds))
    (printf "Done testing insn: ~a. Time: ~a ms\n"
            insn
            (- end-time start-time)))
            
    (printf "--------\nTest summary:\n")
    (printf "Num failed: ~a\n" (length failed))
    (printf "Num succeeded: ~a\n" (length succeeded))
    (when (> (length failed) 0)
      (printf "Failing instructions:\n")
      (for ([insn failed])
        (printf "\t~a\n" insn))))


