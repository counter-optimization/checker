#lang rosette

(require
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

(provide (all-defined-out))

(struct verification-setup (inregs outreg))

(struct verification-task (code setupdata))

;;;; MOV8*

;;; setup data
(define mov8-verif-setup (verification-setup (list cl al) dl))

;;; transform
;; given two 8-bit bitvectors, returns a third that is
;; not equal to x and not equal to y
;; use cl, al
;; put results in dl
;; say cl is new value
;; say al is the old value in memory
(define attempt-mov8
  (list
   (mov-r/m64-imm32 r10 (bv (expt 2 31) 32))
   (mov-r/m8-r8 r10b cl)
   (and-r/m8-r8 r10b (bv 2147483888 32)) ; 0x8000.00F0
   (not-r/m8 r10b)
   (mov-r/m64-imm32 r11 (bv (expt 2 31) 32))
   (mov-r/m8-r8 r11b al)
   (and-r/m8-r8 r11b (bv 2147483663 32)) ; 0x8000.000F
   (not-r/m8 r11b)
   (or-r/m32-r32 r11d r10d)
   (mov-r/m8-r8 dl r11b)))

(define (silent-stores-verify attempt verif-setup)
  (define spec-cpu (comp-simp:make-x86-64-cpu))
  (define attempt-cpu (comp-simp:make-x86-64-cpu))

  (comp-simp:assume-all-regs-equiv spec-cpu attempt-cpu)
  (comp-simp:assume-all-flags-equiv spec-cpu attempt-cpu)

  (comp-simp:apply-insn-specific-asserts #:insns attempt	
                                         #:asserter comp-simp:comp-simp-asserter
                                         #:cpu attempt-cpu)

  (comp-simp:run-x86-64-impl #:insns attempt #:cpu attempt-cpu)
  (comp-simp:run-x86-64-impl #:insns spec #:cpu spec-cpu)

  (define inregs (verification-setup-inregs verif-setup))
  (define outreg (verification-setup-outreg verif-setup))
  (for ([reg inregs])
    (define spec-reg (cpu-gpr-ref spec-cpu reg))
    (define impl-reg (cpu-gpr-ref attempt-cpu reg))
    (assert (bveq spec-reg impl-reg)))

  (define spec-out (cpu-gpr-ref spec-cpu outreg))
  (define att-out (cpu-gpr-ref attempt-cpu outreg))
  (assert (bveq att-out spec-out)))

(module+ main
  (displayln "running verification...")
  (define tasks (list
  	  	  (verification-task attempt-mov8 mov8-verif-setup)))
  (for ([task tasks])
    (define code (verification-task-code task))
    (define setup (verification-task-setupdata task))
    (define result (verify (silent-stores-verify code setup)))
    (displayln result))
  (displayln "done"))
		  
  
   