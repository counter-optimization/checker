#lang rosette

(require rosette/lib/synthax)

(define-grammar (all-bv-ops x y)
  [expr
   (choose x y (?? (bitvector 8))     
           ((bop) (expr) (expr)))]
           ;(bvnot (expr)))]
  [bop
   (choose
           bvand
           bvor)]
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
  (assert (&& (! (bveq result x))
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

(define solution
  (synthesize
   #:forall (list x y)
   #:guarantee (is-correct? impl x y)))

(if (sat? solution)
    (print-forms solution)
    (displayln "No solution."))

