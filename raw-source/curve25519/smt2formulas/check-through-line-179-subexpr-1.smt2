(declare-const tmp11 (_ BitVec 16))

(declare-const f44 (_ BitVec 16))

(declare-const f29 (_ BitVec 16))

(declare-const f43 (_ BitVec 16))

(declare-const f28 (_ BitVec 16))

(declare-const f42 (_ BitVec 16))

(declare-const f27 (_ BitVec 16))

(declare-const f41 (_ BitVec 16))

(declare-const f26 (_ BitVec 16))

(declare-const f40 (_ BitVec 16))

(declare-const f25 (_ BitVec 16))

(declare-const f34 (_ BitVec 16))

(declare-const f19 (_ BitVec 16))

(declare-const f33 (_ BitVec 16))

(declare-const f18 (_ BitVec 16))

(declare-const f32 (_ BitVec 16))

(declare-const f17 (_ BitVec 16))

(declare-const f31 (_ BitVec 16))

(declare-const f16 (_ BitVec 16))

(declare-const f30 (_ BitVec 16))

(declare-const f15 (_ BitVec 16))

(declare-const f24 (_ BitVec 16))

(declare-const f23 (_ BitVec 16))

(declare-const f22 (_ BitVec 16))

(declare-const f21 (_ BitVec 16))

(declare-const f20 (_ BitVec 16))

(declare-const f14 (_ BitVec 16))

(declare-const f13 (_ BitVec 16))

(declare-const f12 (_ BitVec 16))

(declare-const f11 (_ BitVec 16))

(declare-const f10 (_ BitVec 16))

(assert (= f30 f15))

(assert (= f31 f16))

(assert (= f32 f17))

(assert (= f33 f18))

(assert (= f34 f19))

(assert (= f40 f25))

(assert (= f41 f26))

(assert (= f42 f27))

(assert (= f43 f28))

(assert (= f44 f29))

(assert (= tmp11 (bvmul f21 (_ bv19 16))))

(assert (not (= (_ bv1 16) f22)))

(check-sat)
