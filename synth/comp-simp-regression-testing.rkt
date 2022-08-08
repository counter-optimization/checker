#lang rosette

(require
 (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt")
 "../serval/serval/x86.rkt"
 (prefix-in testing: "comp-simp-synth-testing.rkt"))

;; These are regression tests from running the
;; randomized sequence synthesis tests over the weekend.

(define (test-seq-for-exceptions insns)
  (define fresh-cpu (comp-simp:make-x86-64-cpu))
  (comp-simp:run-x86-64-impl #:insns insns #:cpu fresh-cpu))

(define test-sequences
  (list
   (list
    (sub-r/m32-r32 edi ecx)
    (sub-r/m64-imm8 rdi (bv #x0 8)))

   (list
    (sub-r/m32-r32 edx ecx)
    (sub-r/m64-imm8 rcx (bv #x07 8)))

   (list
    (sub-r/m64-r64 rdi rcx)
    (cmovz-r32-r32 eax eax))

   (list
    (setz dl)
    (sub-r/m64-imm8 rdi (bv #x1 8))
    (sub-r/m32-imm32 eax (bv #x0 32)))))

(printf "Starting tests, don't forget to `raco symtrace` this file.\n")

(for ([insn-seq test-sequences])
  (printf "Running test for insn seq: ~a\n" insn-seq)
  (test-seq-for-exceptions insn-seq))

(printf "Done\n")

(printf "Now testing that they can be synthesized.\n")

(module+ main
  (define succeeded empty)
  (define failed empty)
  
  (for ([insn-seq test-sequences])
    (clear-vc!)

    (printf "Testing insn: ~a\n" insn-seq)
    
    (define spec-cpu (comp-simp:make-x86-64-cpu))
    (define synth-cpu (comp-simp:make-x86-64-cpu))

    ; want the same length insn seq synthd as the
    ; concrete insn seq
    (define target-insn-seq-len (length insn-seq))

    (define result
      (synthesize
       #:forall (append (comp-simp:forall-quantified-vars spec-cpu)
                        (comp-simp:forall-quantified-vars synth-cpu))
       #:guarantee (testing:run-and-check-synthesis #:spec-cpu spec-cpu
                                                    #:spec-insns insn-seq
                                                    #:impl-cpu synth-cpu
                                                    #:apply-asserts #f
                                                    #:num-insns target-insn-seq-len)))

    (if (sat? result)
        (begin
          (printf "Successfully synthesized insn sequence\n")
          ;; (printf "Solution was: ~a\n" result)
          (printf "Spec insn(s) were: ~a\n" insn-seq)
          (set! succeeded (cons insn-seq succeeded)))
        (begin
          (printf "FAILURE: Could not synthesize insn sequence\n")
          (printf "Insn was: ~a\n" insn-seq)
          (set! failed (cons insn-seq failed))))))
  
