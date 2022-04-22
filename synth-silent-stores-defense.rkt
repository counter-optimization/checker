#lang rosette

(require rosette/lib/synthax)

(define-grammar (all-bv-ops x y)
  [expr
   (choose x y (?? (bitvector 32))        
           ((bop) (expr) (expr))  
           ((uop) (expr)))]
  [bop
   (choose bveq
           bvslt
           bvult
           bvsle
           bvule
           bvsgt
           bvugt
           bvsge
           bvuge
           bvneg
           bvadd
           bvmul
           bvsdiv
           bvudiv
           bvsrem
           bvurem
           bvsmod)]
  [uop
   (choose bvnot
           bvand
           bvor
           bvxor
           bvshl
           bvlshr
           bvashr
           lsb
           msb
           bvrol
           bvror)])

(define (is-correct? impl x y)
  (define result (impl x y))
  (assert (&& (! (bveq result x))
              (! (bveq result y)))))

(define (current-impl x y)
  (define top-half (extract 31 16 x))
  (define bottom-half (extract 15 0 y))
  (define tmp (concat top-half bottom-half))
  (bvnot tmp))

(define (impl x y)
  (all-bv-ops x y #:depth 5))

(define-symbolic x (bitvector 32))
(define-symbolic y (bitvector 32))

(define solution
  (synthesize
   #:forall (list x y)
   #:guarantee (is-correct? impl x y)))

(if (sat? solution)
    (print-forms (generate-forms solution))
    (displayln "No solution."))

