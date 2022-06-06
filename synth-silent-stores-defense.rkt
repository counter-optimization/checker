#lang rosette

(require rosette/lib/synthax)

(define-grammar (all-bv-ops x y)
  [expr
   (choose x y (?? (bitvector 8)) (?? (bitvector 16))     
           ((bop) (expr) (expr))
           ((uop) (expr))
           (extract (?? integer?) (?? integer?) (expr)))]
           ;(bvnot (expr)))]
  [bop
   (choose bvand
           bvor
           bvxor
           bvshl
           bvlshr
           concat)]
;           bvslt
;           bvult
;           bvsle
;           bvule
;           bvsgt
;           bvugt
;           bvsge
;           bvuge
;           bvneg
;           bvadd
;           bvshl
;           bvlshr
;           bvashr
;           bvrol
;           bvror
          ;bvmul
          ;bvsdiv
          ;bvudiv
          ;bvsrem
          ;bvurem
          ;bvsmod
  [uop
   (choose bvnot)])
;           lsb
;           msb

(define (is-correct? impl x y)
  (define result (impl x y))
  (assert (&& (equal? 8 (length (bitvector->bits result)))
              (! (bveq result x))
              (! (bveq result y)))))

(define (current-impl x y)
  (define top-half (extract 8 4 x))
  (define bottom-half (extract 3 0 y))
  (define tmp (concat top-half bottom-half))
  (bvnot tmp))

(define (impl x y)
  (all-bv-ops x y #:depth 4))

(define (exploded-impl x y)
  (define v0 ((choose bvand bvor)
              (choose x y (?? (bitvector 8)))
              (choose x y (?? (bitvector 8)))))
  (define v1 ((choose bvand bvor)
              (choose x y v0 (?? (bitvector 8)))
              (choose x y v0 (?? (bitvector 8)))))
  (define v2 ((choose bvand bvor)
              (choose x y v0 v1 (?? (bitvector 8)))
              (choose x y v0 v1 (?? (bitvector 8)))))
  (define v3 ((choose bvand bvor)
              (choose x y v0 v1 v2 (?? (bitvector 8)))
              (choose x y v0 v1 v2 (?? (bitvector 8)))))
  (define v4 ((choose bvand bvor)
              (choose x y v0 v1 v2 v3 (?? (bitvector 8)))
              (choose x y v0 v1 v2 v3 (?? (bitvector 8)))))
  (bvnot v4))

(define-symbolic x (bitvector 8))
(define-symbolic y (bitvector 8))

(define (mov-8m-8r x y)
  (define changed-x (all-bv-ops x y #:depth 1))
  (define v1 (zero-extend changed-x (bitvector 16)))
  (define v2 (bvshl v1 (bv 8 16)))

  (define changed-y (all-bv-ops x y #:depth 1))
  (define v3 (zero-extend y (bitvector 16)))
  (define v4 (bvor v2 v3))
  
  (define v5 (bvlshr v4 (bv 4 16)))
  
  (define v6 (extract 7 0 v5))
  (define changed-result (all-bv-ops v6 v6 #:depth 1))
  ;(bvnot v6))
  changed-result)

(define (in-place-mem-change x)
  (bvnot (bvand x (bv #xF0 8))))

(define in-place-sol (verify (assert (! (bveq (in-place-mem-change x) x)))))
(displayln in-place-sol)

;(define solution
;  (synthesize
;    #:forall (list x y)
;    #:guarantee (is-correct? mov-8m-8r x y)))

;(if (sat? solution)
;    (print-forms solution)
;    (displayln "No solution."))

(exit)

(define result (mov-8m-8r x y))
(displayln "Checking result...")
(define sol (verify (assert (&& (! (bveq result x))
	                        (! (bveq result y))))))
(displayln (format "Solution: ~a" sol))
(unless (unsat? sol)
  (displayln (evaluate (list x y) sol)))

(displayln (bvlshr (bv #xF0F0 16) (bv 4 16)))

;(define solution
;  (synthesize
;   #:forall (list x y)
;   #:guarantee (is-correct? impl x y)))
;
;(if (sat? solution)
;    (print-forms solution)
;    (displayln "No solution."))

(exit)


(require "serval/serval/x86/base.rkt"
         "serval/serval/x86.rkt"
         (prefix-in core: "serval/serval/lib/core.rkt")
         "serval/serval/x86/interp.rkt")


(define-grammar (x86-ops)
  [insn-list (choose empty (cons (expr) (insn-list)))]
  [expr (choose ((bin-reg-op) (gpr) (gpr))
                ((bin-imm-op) (gpr) (imm32))
                ((un-reg-op) (gpr))
                ((tri-op) (gpr) (gpr) (imm8)))]
  [imm8 (?? (bitvector 8))]
  [imm32 (?? (bitvector 32))]
  [gpr (choose rdi rsi r11)]
  [bin-reg-op (choose and-r/m64-r64
                      xor-r/m64-r64
                      or-r/m64-r64)]
  [bin-imm-op (choose and-r/m64-imm32
                      or-r/m64-imm32
                      xor-r/m64-imm32)]
  [un-reg-op (choose neg-r/m64)]
  [tri-op (choose shld-r/m64-r64-imm8)])

; insns
; and-r/m64-r64 dst src
; xor-r/m64-r64 dst src
; or-r/m64-r64 dst src
; neg-r/m64 dst
; shld-r/m64-r64-imm8 dst src imm8
; and-r/m64-imm32 dst imm32
; or-r/m64-imm32 dst imm32
; xor-r/m64-imm32 dst imm32
(define (run-x86-impl cpu insns)
  (for ([i insns])
    (interpret-insn cpu i)))
  
(define mm (core:make-flat-memmgr #:bitwidth 64))
(define cpu (init-cpu mm))

(define (x86-impl)
  (x86-ops #:depth 20))

(define (x86-impl-is-correct? oldop1 oldop2 cpu)
  (define insns (x86-impl))
  (run-x86-impl cpu insns)
  (define result (cpu-gpr-ref cpu r11))
  (define newrdi (cpu-gpr-ref cpu rdi))
  (define newrsi (cpu-gpr-ref cpu rsi))
  (assert (&& (bveq result (bvxor newrdi newrsi))
              (bveq newrdi oldop1)
              (bveq newrsi oldop2))))
  
  ;(assert (&& (! (bveq result oldop1))
  ;            (! (bveq result oldop2)))))

;(define (check)
;  (define mm (core:make-flat-memmgr #:bitwidth 64))
;  (define cpu (init-cpu mm))
;  (define insns (list (xor-r/m64-r64 r11 r11)
;                      (xor-r/m64-r64 r11 rdi)
;                      (xor-r/m64-r64 r11 rsi)))
;  (run-x86-impl cpu insns)
;  (verify
;   (assert (bveq (bvxor (cpu-gpr-ref cpu rdi)
;                        (cpu-gpr-ref cpu rsi))
;                 (cpu-gpr-ref cpu r11)))))

(define myrdi (cpu-gpr-ref cpu rdi))
(define myrsi (cpu-gpr-ref cpu rsi))

;(define solution
;  (synthesize
;   #:forall (list myrdi myrsi)
;   #:guarantee (x86-impl-is-correct? myrdi myrsi cpu)))
;
;(if (sat? solution)
;    (print-forms solution)
;    (displayln "No solution."))

  

