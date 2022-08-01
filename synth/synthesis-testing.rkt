#lang rosette

(require rosette/lib/synthax
         ;rosette/lib/value-browser
         rosette/lib/match
         rosette/lib/angelic)

(require "serval/serval/x86.rkt"
         (prefix-in core: "serval/serval/lib/core.rkt"))

; Serval setup
(define (make-x86-64-cpu)
  (define mm (core:make-flat-memmgr #:bitwidth 64))
  (init-cpu mm))

(define (run-x86-64-impl #:insns insns #:cpu cpu)
  
  (define (run-insn i cpu)
    (match i
      ['noop #f]
      [_ (instruction-run i cpu)]))
  
  (if (list? insns)
    (for ([i insns])
      (run-insn i cpu))
    (run-insn insns cpu)))

(define all-regs (list rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15
                       eax ecx edx ebx esp ebp esi edi r8d r9d r10d r11d r12d r13d r14d r15d
                       ax cx dx bx sp bp si di r8w r9w r10w r11w r12w r13w r14w r15w
                       al cl dl))

(define flag-symbols '(CF PF AF ZF SF OF))

(define (assume-all-regs-equiv cpu1 cpu2)
  (for ([reg-name all-regs])
    (define r1 (cpu-gpr-ref cpu1 reg-name))
    (define r2 (cpu-gpr-ref cpu2 reg-name))
    (assume (bveq r1 r2))))

(define (assume-all-flags-equiv cpu1 cpu2)
  (for ([flag-name flag-symbols])
    (define f1 (cpu-flag-ref cpu1 flag-name))
    (define f2 (cpu-flag-ref cpu2 flag-name))
    (assume (bveq f1 f2))))

(displayln (format "Current grammar depth: ~a" (current-grammar-depth)))
(error-print-width 100000)
(displayln (format "Current error-print-width: ~a" (error-print-width)))

(define-syntax (cpu-pp-from-sol stx)
  (syntax-case stx ()
    [(_ cpu sol) 
     (with-syntax ([reg-names #`(list #,@(map symbol->string (cdr (syntax->datum #'(list rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15
                       eax ecx edx ebx esp ebp esi edi r8d r9d r10d r11d r12d r13d r14d r15d
                       ax cx dx bx sp bp si di r8w r9w r10w r11w r12w r13w r14w r15w
                       al cl dl)))))]
                   [regs (syntax->datum #'(list rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15
                       eax ecx edx ebx esp ebp esi edi r8d r9d r10d r11d r12d r13d r14d r15d
                       ax cx dx bx sp bp si di r8w r9w r10w r11w r12w r13w r14w r15w
                       al cl dl))]
                   [flags (syntax->datum #''(CF PF AF ZF SF OF))])
       #'(begin
          (printf "CPU: \n")
          (for ([regname reg-names]
                [reg regs])
            (printf "~a: ~a\n" 
                    regname
                    (evaluate (sol (cpu-gpr-ref cpu reg)) sol)))
          (for ([flagname flags])
            (printf "~a: ~a\n" 
                    flagname
                    (evaluate (sol (cpu-flag-ref cpu flagname)) sol)))))]))

; (define (cpu-pp-from-sol cpu sol)
;   (define eax-val (evaluate (sol (cpu-gpr-ref cpu eax)) sol))
;   (printf "CPU:\n")
;   (printf "EAX: ~a\n" eax-val))

; test insn sequences
(define synthd-4-length
  (list
   (sub-r/m64-r64 rdi rdx)
   (cmovz-r32-r32 edi edi)
   (cmovz-r32-r32 edx ecx)
   (sub-r/m64-r64 rax rdx)))

(define (sub-r/m32-r32-conc-impl)
  (define sub (sub-r/m32-r32 eax ecx))
  (list sub))

; run the test insn sequences
(define (check-equivalences #:test-insns test-insns 
                            #:spec-insns spec-insns)
  (define impl-cpu (make-x86-64-cpu))
  (define spec-cpu (make-x86-64-cpu))
  
  (assume-all-regs-equiv impl-cpu spec-cpu)
  (assume-all-flags-equiv impl-cpu spec-cpu)
  
  (run-x86-64-impl #:insns test-insns #:cpu impl-cpu)
  (run-x86-64-impl #:insns spec-insns #:cpu spec-cpu)
  
  (printf "Is insn sequence:\n\n")
  (for ([insn test-insns])
    (displayln insn))
  (printf "\nequivalent to:\n")
  (for ([insn spec-insns])
    (displayln insn))
  (printf "\n?:\n") 
  
  (define (is-correct-result? conc-cpu spec-cpu)
    (define spec-eax (cpu-gpr-ref spec-cpu eax))
    (define conc-eax (cpu-gpr-ref conc-cpu eax))
    (assert (bveq spec-eax conc-eax)))
  
  (define result (verify
                   (begin
                       (assert (is-correct-result? impl-cpu spec-cpu)))))
  
  (displayln (if (unsat? result) "yes" "no"))
  
  (when (sat? result)
    (printf "impl-cpu:\n")
    (cpu-pp-from-sol impl-cpu result)
    (printf "spec-cpu\n")
    (cpu-pp-from-sol spec-cpu result)))

(check-equivalences #:test-insns synthd-4-length 
                    #:spec-insns (sub-r/m32-r32-conc-impl))


