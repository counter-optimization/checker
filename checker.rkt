#lang rosette

(require
  "serval/serval/x86.rkt"
  "serval/serval/x86/base.rkt"
  "serval/serval/lib/bvarith.rkt"
  (prefix-in core: "serval/serval/lib/core.rkt")
  rosette/lib/value-browser)

(provide (all-defined-out))

;; handling obj files

(define ftfp-obj-file "ftfp.o.txt")
(define sample-function "_fix_convert_from_int64")
;(define sample-function "_fix_abs")

;; Hard coding in how llvm's objdump dumps out disassembly of an object file
(define (is-func-definition-line? insn-line funcname)
  (define objdump-formatted-name (string-append "<" funcname ">:"))
  (string-contains? insn-line objdump-formatted-name))

(define (objdump-file->insn-lines filename funcname)
  (define lines (port->lines (open-input-file filename #:mode 'text)))
  (set! lines (map string-trim lines))
  (define is-func-start? (lambda (line)
                          (is-func-definition-line? line funcname)))
  (define func-start (memf is-func-start? lines))
  (define func-end (index-of func-start ""))
  (take func-start func-end))

(define (text-line->serval-insn text-line)
  (define regexp-result (regexp-match* #px"^([a-f0-9]+):(\\s[a-f0-9]{2})+" text-line))
  (when (empty? regexp-result)
      (displayln (format "Couldn't match insn line regex for text line: ~a" text-line)))
  (define match (car regexp-result))
  (define normalized-results (map string-trim (string-split match ":")))
  (define address (car normalized-results))
  (define bytes (string-split (cadr normalized-results)))
  (define insn-bytes-list (for/list ([byte-string bytes])
                            (bv (string->number byte-string 16) 8)))
  (define serval-insn (decode insn-bytes-list))
  serval-insn)

;; Helpers for defining events/hooks for the extending interpreters/checkers

(define (is-special-name? name special-name-prefixes)
  (ormap (lambda (prefix) (string-contains? name prefix))
         special-name-prefixes))

;; x86 insns in serval are in intel syntax--i.e., dst comes before src
;; => to tell if the insn is a store, check if dst operand is register-indirect?
;; => to tell if an insn is a load, check if src operand is register-indirect?
;; some insns are implicit load/store (push/pop) with operands encoded in opcode,
;;   those are handled specially

;; these are prefixes
(define special-store-name-prefixes (list "push"))
(define special-load-name-prefixes (list "leave" "pop"))

(define (is-store-insn? insn)
  (define vector-insn (struct->vector insn))
  (define name (symbol->string (vector-ref vector-insn 0)))
  (or (is-special-name? name special-store-name-prefixes)
      (and 
       (! (string-contains? name "cmp"))
       (match (struct->vector insn)
         ; 2 or more operands
         [(vector name dst src others ...) (register-indirect? dst)]
         ; 1 operand
         [(vector name dst) (register-indirect? dst)]
         ; no operands
         [(vector name) #f]))))

(define (is-load-insn? insn)
  (define vector-insn (struct->vector insn))
  (define name (symbol->string (vector-ref vector-insn 0)))
  (or (is-special-name? name special-load-name-prefixes)
      (match (struct->vector insn)
        ; 2 or more operands
        [(vector name dst src others ...) (register-indirect? src)]
        ; 1 operand
        [(vector name src) (register-indirect? src)]
        ; no operands
        [(vector name) #f])))

;; TBD: for computation simplification stuff
(define (is-alu-insn? insn)
  #f)

;; extending interpreters -- for activation condition checkers
;; `checkers' contains all checkers
(define checkers empty)
(define on-store-checkers empty)
(define on-load-checkers empty)
(define on-alu-checkers empty)

(define (add-checker-to-store c store)
  (define new (cons c store))
  (set! store new))

(define (add-checker #:checker c #:events es)
  (for ([e es])
    (match e
      ['store (set! on-store-checkers (cons c on-store-checkers))]
      ['load (set! on-load-checkers (cons c on-load-checkers))]
      ['alu (set! on-alu-checkers (cons c on-alu-checkers))])))

(define (set-up-cpu)
  (define mm (core:make-flat-memmgr #:bitwidth 64))
  (define cpu (init-cpu mm))
  cpu)

(define (run-all-checkers-on-insn insn cpu checkers)
  (for ([c checkers])
    (c insn cpu)))

;; First, run all the checkers, then run the base interpreter.
(define (run-insn insn cpu #:base base-interp)
  (begin
    (cond
      [(is-store-insn? insn) (run-all-checkers-on-insn insn cpu on-store-checkers)]
      [(is-load-insn? insn) (run-all-checkers-on-insn insn cpu on-load-checkers)]
      [(is-alu-insn? insn) (run-all-checkers-on-insn insn cpu on-alu-checkers)])
    (base-interp insn cpu)))

(define (base-interpreter insn cpu)
  (displayln (format "Running insn: ~a" insn))
  (interpret-insn cpu insn))

(define (sequential-execution insns cpu #:base base-interp)
  (for ([insn insns])
    (run-insn insn cpu #:base base-interp)))

;; tracks the last address and last value stored
;; silent store activation condition:
;; given the current store addr and last stored value,
;;   ensure that the last stored value and address could not be the same
;; running this on the sequential base interpreter 
(define (ss-checker insn cpu)
  (displayln (format "Running ss-checker on insn: ~a" insn))
  (define (is-push? name-sym)
    (is-special-name? (symbol->string name-sym) special-store-name-prefixes))
  (define mm (cpu-memmgr cpu))
  (match (struct->vector insn)
    [(vector name dst src rst ...)
     (begin
       (define store-size (match src
                            [(or (? gpr64?) (? gpr64-no-rex?)) 8]
                            [(or (? gpr32?) (? gpr32-no-rex?)) 4]
                            [(or (? gpr16?) (? gpr16-no-rex?)) 2]
                            [(or (? gpr8?) (? gpr8-no-rex?)) 1]
                            [(? bv?) (quotient (bv-size src) 8)]))
       (define size-in-bytes (bv store-size 64))
       (define offset (bv 0 64)) ; TODO
       (define addr (zero-extend (cpu-gpr-ref cpu dst) i64))
       (displayln (format "sizeof addr: ~a, sizeof offset: ~a" (bv-size addr) (bv-size offset)))
       (displayln (format "Reading from addr ~a, offset: ~a, size: ~a" addr offset size-in-bytes))
       (define old-val (core:memmgr-load mm addr offset size-in-bytes))
       (define new-val (cond
                         [(bv? src) src]
                         [else (cpu-gpr-ref cpu src)]))
       (displayln (format "old-val is ~a" old-val))
       (displayln (format "new-val is ~a" new-val))
       (assert (! (bveq new-val old-val))))]
    [(vector (? is-push?) src)
     (begin
       (define addr (get-push-dst cpu))
       (define old-val (core:memmgr-load mm addr (bv 0 64) (bv 8 64)))
       (define new-val (cpu-gpr-ref cpu src))
       (assert (! (bveq old-val new-val))))]))

(define (comp-simpl-checker insn cpu)
  (displayln (format "Running comp-simpl-checker on insn: ~a" insn))

  (define (name-contains? word)
    (lambda (name-sym) (string-contains? (symbol->string name-sym) word)))
  
  (define is-mul? (name-contains? "mul"))
  (define is-add? (name-contains? "add"))

  (match-let ([(vector name-sym dst src rst ...) (struct->vector insn)])
    ; Check for mul,add identities
    (define ident-val (if (is-mul? name-sym) 1 0))
    (define right (cond
                    [(bv? src) src]
                    [else (cpu-gpr-ref cpu src)]))
    (define right-bitwidth (bv-size right))
    (define rights-ident (bv ident-val right-bitwidth))
    (assert (! (bveq right rights-ident)))))

;  (match (struct->vector insn)
;    [(vector (? is-mul?) dst src rst ...)
;     (define left (cpu-gpr-ref cpu dst))
;     (define right (cond
;                     [(bv? src) src]
;                     [else (cpu-gpr-ref cpu src)]))
;     (define left-bitwidth (bv-size left))
;     (define right-bitwidth (bv-size right))
;     (define lefts-ident (bv 1 left-bitwidth))
;     (define rights-ident (bv 1 right-bitwidth))
;     (assert (&& (! (bveq left lefts-ident))
;                 (! (bveq right rights-ident))))]
;    [(vector (? is-add?) dst src rst ...) void]))
                          
;; main visitor and interpreter
(clear-vc!) ; It feels like there is a bug somewhere that this is necessary.

(define insn-lines (objdump-file->insn-lines ftfp-obj-file sample-function))
(define is-convert-from-int64-definition-line? (lambda (insn-line)
                                                 (! (is-func-definition-line? insn-line sample-function))))
(define only-insn-lines (filter is-convert-from-int64-definition-line? insn-lines))
(define serval-insns (flatten (map text-line->serval-insn only-insn-lines)))

(add-checker #:checker ss-checker #:events '(store))
(displayln (format "Number of store checkers: ~a" (length on-store-checkers)))
(displayln (format "Number of load checkers: ~a" (length on-load-checkers)))
(displayln (format "Number of alu checkers: ~a" (length on-alu-checkers)))

(define cpu (set-up-cpu))
;(cpu-gpr-set! cpu rdi (bv 0 64)) ; first arg 0
(sequential-execution serval-insns cpu #:base base-interpreter)
(displayln (vc))
(verify (vc))
(render-value/window (vc))