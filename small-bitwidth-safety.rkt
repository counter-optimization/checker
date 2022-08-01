#lang rosette

(define-symbolic* x (bitvector 32))
(define-symbolic* y (bitvector 32))

(printf "Small bitwidth mul is safe for 1? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvmul x y) (bv 1 32)))
   (assert (bveq (bvmul small-x small-y) (bv 1 4)))))

(printf "Small bitwidth mul is safe for 0? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assert (=> (bveq (bvmul x y) (bv 0 32))
               (bveq (bvmul small-x small-y) (bv 0 4))))))

(printf "Small bitwidth add is safe for 1? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvadd x y) (bv 1 32)))
   (assert (bveq (bvadd small-x small-y) (bv 1 4)))))

(printf "Small bitwidth add is safe for 0? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assert (=> (bveq (bvadd x y) (bv 0 32))
               (bveq (bvadd small-x small-y) (bv 0 4))))))

(printf "Small bitwidth lshr is safe for 1? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvlshr x y)
                 (bv 1 32)))
   (assert (bveq (bvlshr small-x small-y)
                 (bv 1 4)))))

(printf "Small bitwidth lshr is safe for 0? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvlshr x y)
                 (bv 0 32)))
   (assert (bveq (bvlshr small-x small-y)
                 (bv 0 4)))))

(printf "Small bitwidth ashr is safe for 1? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvashr x y)
                 (bv 1 32)))
   (assert (bveq (bvashr small-x small-y)
                 (bv 1 4)))))

(printf "Small bitwidth ashr is safe for 0? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvashr x y)
                 (bv 0 32)))
   (assert (bveq (bvashr small-x small-y)
                 (bv 0 4)))))

(printf "Small bitwidth shl is safe for 1? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvshl x y)
                 (bv 1 32)))
   (assert (bveq (bvshl small-x small-y)
                 (bv 1 4)))))

(printf "Small bitwidth shl is safe for 0? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvshl x y)
                 (bv 0 32)))
   (assert (bveq (bvshl small-x small-y)
                 (bv 0 4)))))

(printf "Small bitwidth sub is safe for 1? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvsub x y) (bv 1 32)))
   (assert (bveq (bvsub small-x small-y)
                 (bv 1 4)))))

(printf "Small bitwidth sub is safe for 0? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvsub x y) (bv 0 32)))
   (assert (bveq (bvsub small-x small-y)
                 (bv 0 4)))))

(printf "Small bitwidth sdiv is safe for 1? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvsdiv x y) (bv 1 32)))
   (assert (bveq (bvsdiv small-x small-y)
                 (bv 1 4)))))

(printf "Small bitwidth sdiv is safe for 0? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvsdiv x y) (bv 0 32)))
   (assert (bveq (bvsdiv small-x small-y)
                 (bv 0 4)))))

(printf "Small bitwidth udiv is safe for 1? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvudiv x y) (bv 1 32)))
   (assert (bveq (bvudiv small-x small-y)
                 (bv 1 4)))))

(printf "Small bitwidth udiv is safe for 0? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvudiv x y) (bv 0 32)))
   (assert (bveq (bvudiv small-x small-y)
                 (bv 0 4)))))

(printf "Small bitwidth xor is safe for 1? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvxor x y) (bv 1 32)))
   (assert (bveq (bvxor small-x small-y)
                 (bv 1 4)))))

(printf "Small bitwidth xor is safe for 0? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvxor x y) (bv 0 32)))
   (assert (bveq (bvxor small-x small-y)
                 (bv 0 4)))))

(printf "Small bitwidth and is safe for 1? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvand x y) (bv 1 32)))
   (assert (bveq (bvand small-x small-y)
                 (bv 1 4)))))

(printf "Small bitwidth and is safe for 0? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvand x y) (bv 0 32)))
   (assert (bveq (bvand small-x small-y)
                 (bv 0 4)))))

(printf "Small bitwidth or is safe for 1? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvor x y) (bv 1 32)))
   (assert (bveq (bvor small-x small-y)
                 (bv 1 4)))))

(printf "Small bitwidth or is safe for 0? ")
(verify
 (begin
   (define small-x (extract 3 0 x))
   (define small-y (extract 3 0 y))
   (assume (bveq (bvor x y) (bv 0 32)))
   (assert (bveq (bvor small-x small-y)
                 (bv 0 4)))))

(verify
 (begin
   (=> (bveq (bvmul x y) (bv 1 32))
       (bveq
        (bvmul (extract 7 0 x) (extract 7 0 y))
        (bv 1 8)))))