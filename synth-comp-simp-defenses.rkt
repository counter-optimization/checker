#lang rosette

(require rosette/lib/synthax
         rosette/lib/value-browser)

(require "serval/serval/x86.rkt"
         (prefix-in core: "serval/serval/lib/core.rkt"))

;; Common

(define (make-x86-64-cpu)
  (define mm (core:make-flat-memmgr #:bitwidth 64))
  (init-cpu mm))

(define (run-x86-64-impl #:insns insns #:cpu cpu)
  (for ([i insns])
    (interpret-insn cpu i)))

;; Addition

; Add r/m32, r32 (01 /r)
;; (define-insn add-r/m32-r32 (dst src)
;;   #:decode [((byte #x01) (/r reg r/m))
;;             (list (gpr32-no-rex r/m) (gpr32-no-rex reg))]
;;            [((rex/r r b) (byte #x01) (/r reg r/m))
;;             (list (gpr32 b r/m) (gpr32 r reg))]
;;   #:encode (list (rex/r src dst) (byte #x01) (/r src dst))
;;   (lambda (cpu dst src)
;;     (interpret-add cpu dst (cpu-gpr-ref cpu src))))

;; of all add instructions, does not add identity elem as either operand

;; General utility
(define (assert-bvs-equiv bvs1 bvs2)
  (for ([b1 bvs1]
        [b2 bvs2])
    (assert (bveq b1 b2))))

(define (assume-bvs-equiv bvs1 bvs2)
  (for ([b1 bvs1]
        [b2 bvs2])
    (assume (bveq b1 b2))))

;; (E)FLAG comparison functions
(define flag-symbols '(CF PF AF ZF SF OF))
(displayln (format "flag-symbols: ~a" flag-symbols))

(define (get-all-flags #:cpu cpu)
  (for/list ([flag-sym flag-symbols])
    (cpu-flag-ref cpu flag-sym)))

(define assert-flags-equiv assert-bvs-equiv)
(define assume-flags-equiv assume-bvs-equiv)

;; GPR comparison functions
(define (is-not-rax-subreg reg)
  (define reg-string (symbol->string reg))
  (not (string-contains? reg-string "a")))

;; (define all-regs (filter is-not-rax-subreg (append gprs64 gprs32 gprs16 gprs8)))
(define all-regs-but-raxes (list rcx rdx rbx
                                 rsp rbp rsi rdi
                                 r8 r9 r10 r11
                                 r12 r13 r14 r15
                                 ecx edx ebx
                                 esp ebp esi edi
                                 r8d r9d r10d r11d
                                 r12d r13d r14d r15d))
(displayln (format "all-regs-but-raxes: ~a" all-regs-but-raxes))

(define all-raxes (list rax eax ax al))

(define (get-all-regs-but-raxes #:cpu cpu)
  (for/list ([reg all-regs-but-raxes])
    (cpu-gpr-ref cpu reg)))

(define (get-all-regs-raxes-only #:cpu cpu)
  (for/list ([reg all-raxes])
    (cpu-gpr-ref cpu reg)))

(define assert-regs-equiv assert-bvs-equiv)
(define assume-regs-equiv assume-bvs-equiv)

;; Synthesis impl and spec for adding eax and ecx
; Adding eax <- eax + ecx
(define (add-r/m32-r32-conc-impl)
  (define add (add-r/m32-r32 eax ecx))
  (list add))

(define-grammar (x86-64-arith-insn-list)
  [insn-list (choose empty (cons (bin-exp) (insn-list)))]
  [bin-exp ((bin-op) (reg32) (reg32))]
  [bin-op (choose add-r/m32-r32)]
  [reg32 (choose eax ecx edx ebx
                 esp ebp esi edi
                 r8d r9d r10d r11d
                 r12d r13d r14d r15d)])

(define-grammar (triv-add-synth)
  [expr (add-r/m32-r32 (reg) (reg))]
  [reg (choose eax ecx edx ebx esp ebp esi edi)])

(define (generate-add-r/m32-r32-insns)
  ;; (x86-64-arith-insn-list #:depth 6))
  (list (triv-add-synth #:depth 2)))

(displayln (format "Current grammar depth: ~a" (current-grammar-depth)))
(error-print-width 4096)
(displayln (format "Current error-print-width: ~a" (error-print-width)))

(define (add-r/m32-r32-spec #:spec-cpu spec-cpu #:impl-cpu impl-cpu)
  ;; (define spec-cpu (make-x86-64-cpu))
  (define spec-insns (add-r/m32-r32-conc-impl))
  
  ;; (define impl-cpu (make-x86-64-cpu))
  (define impl-insns (generate-add-r/m32-r32-insns))

  ;; (displayln (format "spec-cpu: ~a" spec-cpu))
  ;; (displayln (format "impl-cpu: ~a" impl-cpu))

  ; 1. assume starting in the same state
  (define spec-reg-state-before (get-all-regs-but-raxes #:cpu spec-cpu))
  (define impl-reg-state-before (get-all-regs-but-raxes #:cpu impl-cpu))
  (assume-regs-equiv spec-reg-state-before impl-reg-state-before)

  (define spec-reg-raxes-state-before (get-all-regs-raxes-only #:cpu spec-cpu))
  (define impl-reg-raxes-state-before (get-all-regs-raxes-only #:cpu impl-cpu))
  (assume-regs-equiv spec-reg-raxes-state-before impl-reg-raxes-state-before)

  (define spec-flag-state-before (get-all-flags #:cpu spec-cpu))
  (define impl-flag-state-before (get-all-flags #:cpu impl-cpu))
  (assume-flags-equiv spec-flag-state-before impl-flag-state-before)

  ; 2. for all add insns in impl-insns, no operand can be identity
  ; TODO

  ; 3. run the impl and the spec
  (run-x86-64-impl #:insns spec-insns #:cpu spec-cpu)
  (run-x86-64-impl #:insns impl-insns #:cpu impl-cpu)
  
  ; 4. preserves all registers but the dest reg
  (define spec-reg-state-after (get-all-regs-but-raxes #:cpu spec-cpu))
  (define impl-reg-state-after (get-all-regs-but-raxes #:cpu impl-cpu))
  (assert-regs-equiv spec-reg-state-after impl-reg-state-after)
  
  ; 5. sets all flags to expected values
  (define spec-flag-state-after (get-all-flags #:cpu spec-cpu))
  (define impl-flag-state-after (get-all-flags #:cpu impl-cpu))
  (assert-flags-equiv spec-flag-state-after impl-flag-state-after)
  
  ; 6. computes the add of eax and ecx into eax
  (define spec-eax (cpu-gpr-ref spec-cpu eax))
  (define impl-eax (cpu-gpr-ref impl-cpu eax))
  (assert (bveq spec-eax impl-eax)))

;; (clear-vc!)
;; (define cex (verify (add-r/m32-r32-spec)))
;; (displayln cex)
;; (render-value/window cex)

(define impl-cpu (make-x86-64-cpu))
(define spec-cpu (make-x86-64-cpu))

(define solution
  (synthesize
   #:forall (append (vector->list (cpu-gprs impl-cpu))
                    (vector->list (cpu-flags impl-cpu))
                    (vector->list (cpu-gprs spec-cpu))
                    (vector->list (cpu-flags spec-cpu)))
   #:guarantee (add-r/m32-r32-spec #:spec-cpu spec-cpu #:impl-cpu impl-cpu)))

(if (sat? solution)
    (begin
      (displayln "Solution found:")
      (print-forms solution))
    (displayln "No solution."))

;; (define base-cpu (make-x86-64-cpu))
;; (cpu-gpr-set! base-cpu eax (bv 1 32))
;; (cpu-gpr-set! base-cpu ecx (bv 2 32))
;; (define impl-insns (add-r/m32-r32-conc-impl #:cpu base-cpu))
;; (run-x86-64-impl #:insns impl-insns #:cpu base-cpu)
;; (define eax-final-val (cpu-gpr-ref base-cpu eax))
;; (displayln eax-final-val)
;; (displayln (get-all-flags #:cpu base-cpu))

;; Multiplication



