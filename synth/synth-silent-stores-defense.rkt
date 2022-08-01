#lang rosette

(require rosette/lib/synthax
         rosette/lib/angelic)

(define-grammar (all-bv-ops x y)
  [expr
   (choose x y
           (?? (bitvector 8))
           (?? (bitvector 16))
           (?? (bitvector 32))
           ((bop) (expr) (expr))
           ((uop) (expr))
           (extract (?? integer?) (?? integer?) (expr)))]
  [bop
   (choose bvand
           bvor
           bvxor
           bvshl
           bvlshr
           concat
;           bvslt
;           bvult
;           bvsle
;           bvule
;           bvsgt
;           bvugt
;           bvsge
;           bvuge
           bvneg
           bvadd
           bvshl
           bvlshr
           bvashr
           bvrol
           bvror
           bvmul
           bvsdiv
           bvudiv
           bvsrem
           bvurem
           bvsmod)]
  [uop
   (choose bvnot)])

(define-grammar (binop x y)
  [expr ((bop) x y)]
  [bop
   (choose bvand
           bvor
           bvxor
           concat
           bvneg
           bvadd
           bvshl
           bvlshr
           bvashr
           bvrol
           bvror
           bvmul
           bvsdiv
           bvudiv
           bvsub
           bvsrem
           bvurem
           bvsmod)])

(define-grammar (guarded-binop x y)
  [expr ((bop) x y)]
  [bop
   (choose guarded-bvand
           guarded-bvor
           guarded-bvxor
           guarded-bvlshr
           concat
           bvneg
           guarded-bvadd
           guarded-bvshl
           guarded-bvashr
           bvrol
           bvror
           guarded-bvmul
           guarded-bvsub
           bvsdiv
           guarded-bvudiv
           bvsrem
           bvurem
           bvsmod)])


(define (assert-not-bveq value)
  (λ (x)
    (let ([bitwidth (bitvector-size x)])
      (assert (! (bveq x (bv value bitwidth)))))))

(define assert-not-bvzero (assert-not-bveq 0))
(define assert-not-bvone (assert-not-bveq 1))

(define (guarded-bvadd x y)
  (assert-not-bvzero x)
  (assert-not-bvzero y)
  (bvadd x y))

(define (guarded-bvmul x y)
  (assert-not-bvzero x)
  (assert-not-bvzero y)
  (assert-not-bvone x)
  (assert-not-bvone y)
  (bvmul x y))

(define (guarded-bvsub x y)
  (assert-not-bvzero y)
  (bvsub x y))

(define (guarded-bvand x y)
  (assert-not-bvzero x)
  (assert-not-bvzero y)
  (let ([x-bw (bitvector-size x)]
        [y-bw (bitvector-size y)])
    (assert (&& (! (bveq x (bvnot (bv 0 x-bw))))
                (! (bveq y (bvnot (bv 0 y-bw)))))))
  (bvand x y))

(define (guarded-bvor x y)
  (assert-not-bvzero x)
  (assert-not-bvzero y)
  (bvor x y))

(define (guarded-bvxor x y)
  (assert-not-bvzero x)
  (assert-not-bvzero y)
  (bvxor x y))

(define (guarded-bvshl x y)
  (assert-not-bvzero x)
  (assert-not-bvzero y)
  (bvshl x y))

(define (guarded-bvlshr x y)
  (assert-not-bvzero x)
  (assert-not-bvzero y)
  (bvlshr x y))

(define (guarded-bvashr x y)
  (assert-not-bvzero x)
  (assert-not-bvzero y)
  (bvashr x y))

(define (guarded-bvudiv x y)
  (assert-not-bvzero x)
  (assert-not-bvone y)
  (bvudiv x y))

(define-grammar (unop x y)
  [expr (choose
         (extract (?? integer?) (?? integer?) (x-or-y))
         (bvnot (x-or-y)))]
  [x-or-y (choose x y)])

(define current-depth 2)
(define max-single-depth 10)

(define (is-correct? impl x y)
  (define result (impl x y))
  (assert (bveq result (bvadd x y))))

(define (impl x y)
  (all-bv-ops x y #:depth current-depth))

(define (ssa-op x y)
  (choose*
   (guarded-binop x y  #:depth max-single-depth)
   (unop x y #:depth max-single-depth)))

(define (ssa-impl x y)
  (define x1-op1 (choose x y))
  (define x1-op2 (choose x y))
  (define x1 (ssa-op x1-op1 x1-op2))
  
  (define x2-op1 (choose x1 x y))
  (define x2-op2 (choose x1 x y))
  (define x2 (ssa-op x2-op1 x2-op2))
  
  (define x3-op1 (choose x2 x1 x y))
  (define x3-op2 (choose x2 x1 x y))
  (define x3 (ssa-op x3-op1 x3-op2))

  (define x4-op1 (choose x3 x2 x1 x y))
  (define x4-op2 (choose x3 x2 x1 x y))
  (define x4 (ssa-op x4-op1 x4-op2))

  (define x5-op1 (choose x4 x3 x2 x1 x y))
  (define x5-op2 (choose x4 x3 x2 x1 x y))
  (define x5 (ssa-op x5-op1 x5-op2))

  (define x6-op1 (choose x5 x4 x3 x2 x1 x y))
  (define x6-op2 (choose x5 x4 x3 x2 x1 x y))
  (define x6 (ssa-op x6-op1 x6-op2))

  (define x7-op1 (choose x6 x5 x4 x3 x2 x1 x y))
  (define x7-op2 (choose x6 x5 x4 x3 x2 x1 x y))
  (define x7 (ssa-op x7-op1 x7-op2))

  (define x8-op1 (choose x7 x6 x5 x4 x3 x2 x1 x y))
  (define x8-op2 (choose x7 x6 x5 x4 x3 x2 x1 x y))
  (define x8 (ssa-op x8-op1 x8-op2))

  (define x9-op1 (choose x8 x7 x6 x5 x4 x3 x2 x1 x y))
  (define x9-op2 (choose x8 x7 x6 x5 x4 x3 x2 x1 x y))
  (define x9 (ssa-op x9-op1 x9-op2))

  (define x10-op1 (choose x9 x8 x7 x6 x5 x4 x3 x2 x1 x y))
  (define x10-op2 (choose x9 x8 x7 x6 x5 x4 x3 x2 x1 x y))
  (define x10 (ssa-op x10-op1 x10-op2))

  (define x11-op1 (choose x10 x9 x8 x7 x6 x5 x4 x3 x2 x1 x y))
  (define x11-op2 (choose x10 x9 x8 x7 x6 x5 x4 x3 x2 x1 x y))
  (define x11 (ssa-op x11-op1 x11-op2))

  x11)

(define-symbolic* x y (bitvector 32))

(define result (synthesize
                #:forall (list x y)
                #:guarantee (is-correct? ssa-impl x y)))

(printf "result is: ~a\n" result)
(if (sat? result)
    (begin
      (printf "Found solution for depth ~a\n" current-depth)
      (print-forms result))
    (begin
      (printf "No solution for depth ~a\n" current-depth)))

;; Silent store stuff

;; (define (is-correct? impl x y)
;;   (define result (impl x y))
;;   (assert (&& (equal? 8 (length (bitvector->bits result)))
;;               (! (bveq result x))
;;               (! (bveq result y)))))

;; (define (current-impl x y)
;;   (define top-half (extract 8 4 x))
;;   (define bottom-half (extract 3 0 y))
;;   (define tmp (concat top-half bottom-half))
;;   (bvnot tmp))

;; (define (impl x y)
;;   (all-bv-ops x y #:depth 4))

;; (define (exploded-impl x y)
;;   (define v0 ((choose bvand bvor)
;;               (choose x y (?? (bitvector 8)))
;;               (choose x y (?? (bitvector 8)))))
;;   (define v1 ((choose bvand bvor)
;;               (choose x y v0 (?? (bitvector 8)))
;;               (choose x y v0 (?? (bitvector 8)))))
;;   (define v2 ((choose bvand bvor)
;;               (choose x y v0 v1 (?? (bitvector 8)))
;;               (choose x y v0 v1 (?? (bitvector 8)))))
;;   (define v3 ((choose bvand bvor)
;;               (choose x y v0 v1 v2 (?? (bitvector 8)))
;;               (choose x y v0 v1 v2 (?? (bitvector 8)))))
;;   (define v4 ((choose bvand bvor)
;;               (choose x y v0 v1 v2 v3 (?? (bitvector 8)))
;;               (choose x y v0 v1 v2 v3 (?? (bitvector 8)))))
;;   (bvnot v4))

;; (define-symbolic x (bitvector 8))
;; (define-symbolic y (bitvector 8))

;; (define (mov-8m-8r x y)
;;   (define changed-x (all-bv-ops x y #:depth 1))
;;   (define v1 (zero-extend changed-x (bitvector 16)))
;;   (define v2 (bvshl v1 (bv 8 16)))

;;   (define changed-y (all-bv-ops x y #:depth 1))
;;   (define v3 (zero-extend y (bitvector 16)))
;;   (define v4 (bvor v2 v3))
  
;;   (define v5 (bvlshr v4 (bv 4 16)))
  
;;   (define v6 (extract 7 0 v5))
;;   (define changed-result (all-bv-ops v6 v6 #:depth 1))
;;   ;(bvnot v6))
;;   changed-result)

;; (define (in-place-mem-change x)
;;   (bvnot (bvand x (bv #xF0 8))))

;; (define result (mov-8m-8r x y))


;; (require "serval/serval/x86/base.rkt"
;;          "serval/serval/x86.rkt"
;;          (prefix-in core: "serval/serval/lib/core.rkt")
;;          "serval/serval/x86/interp.rkt")


;; (define-grammar (x86-ops)
;;   [insn-list (choose empty (cons (expr) (insn-list)))]
;;   [expr (choose ((bin-reg-op) (gpr) (gpr))
;;                 ((bin-imm-op) (gpr) (imm32))
;;                 ((un-reg-op) (gpr))
;;                 ((tri-op) (gpr) (gpr) (imm8)))]
;;   [imm8 (?? (bitvector 8))]
;;   [imm32 (?? (bitvector 32))]
;;   [gpr (choose rdi rsi r11)]
;;   [bin-reg-op (choose and-r/m64-r64
;;                       xor-r/m64-r64
;;                       or-r/m64-r64)]
;;   [bin-imm-op (choose and-r/m64-imm32
;;                       or-r/m64-imm32
;;                       xor-r/m64-imm32)]
;;   [un-reg-op (choose neg-r/m64)]
;;   [tri-op (choose shld-r/m64-r64-imm8)])

;; ; insns
;; ; and-r/m64-r64 dst src
;; ; xor-r/m64-r64 dst src
;; ; or-r/m64-r64 dst src
;; ; neg-r/m64 dst
;; ; shld-r/m64-r64-imm8 dst src imm8
;; ; and-r/m64-imm32 dst imm32
;; ; or-r/m64-imm32 dst imm32
;; ; xor-r/m64-imm32 dst imm32
;; (define (run-x86-impl cpu insns)
;;   (for ([i insns])
;;     (interpret-insn cpu i)))
  
;; (define mm (core:make-flat-memmgr #:bitwidth 64))
;; (define cpu (init-cpu mm))

;; (define (x86-impl)
;;   (x86-ops #:depth 20))

;; (define (x86-impl-is-correct? oldop1 oldop2 cpu)
;;   (define insns (x86-impl))
;;   (run-x86-impl cpu insns)
;;   (define result (cpu-gpr-ref cpu r11))
;;   (define newrdi (cpu-gpr-ref cpu rdi))
;;   (define newrsi (cpu-gpr-ref cpu rsi))
;;   (assert (&& (bveq result (bvxor newrdi newrsi))
;;               (bveq newrdi oldop1)
;;               (bveq newrsi oldop2))))
  
;;   ;(assert (&& (! (bveq result oldop1))
;;   ;            (! (bveq result oldop2)))))

;; ;(define (check)
;; ;  (define mm (core:make-flat-memmgr #:bitwidth 64))
;; ;  (define cpu (init-cpu mm))
;; ;  (define insns (list (xor-r/m64-r64 r11 r11)
;; ;                      (xor-r/m64-r64 r11 rdi)
;; ;                      (xor-r/m64-r64 r11 rsi)))
;; ;  (run-x86-impl cpu insns)
;; ;  (verify
;; ;   (assert (bveq (bvxor (cpu-gpr-ref cpu rdi)
;; ;                        (cpu-gpr-ref cpu rsi))
;; ;                 (cpu-gpr-ref cpu r11)))))

;; (define myrdi (cpu-gpr-ref cpu rdi))
;; (define myrsi (cpu-gpr-ref cpu rsi))

;; ;(define solution
;; ;  (synthesize
;; ;   #:forall (list myrdi myrsi)
;; ;   #:guarantee (x86-impl-is-correct? myrdi myrsi cpu)))
;; ;
;; ;(if (sat? solution)
;; ;    (print-forms solution)
;; ;    (displayln "No solution."))

  

