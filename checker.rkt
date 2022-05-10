#lang rosette

(require
  "serval/serval/x86.rkt"
  "serval/serval/x86/base.rkt"
  (prefix-in core: "serval/serval/lib/core.rkt")
  rosette/lib/value-browser)

(provide (all-defined-out))

;; handling obj files

(define ftfp-obj-file "ftfp.o.txt")
(define sample-function "_fix_convert_from_int64")

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
      (match (struct->vector insn)
        ; 2 or more operands
        [(vector name dst src others ...) (register-indirect? dst)]
        ; 1 operand
        [(vector name dst) (register-indirect? dst)]
        ; no operands
        [(vector name) #f])))

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
  (set! store (cons c store)))

(define (event-name-to-store event-name)
  (match event-name
    ['store on-store-checkers]
    ['load on-load-checkers]
    ['alu on-alu-checkers]))

(define (add-checker #:checker c #:events es)
  (define add-checker-to-events (map event-name-to-store (remove-duplicates es)))
  (for ([event add-checker-to-events])
    (add-checker-to-store c event)))

(define (set-up-cpu args ...)
  (define mm (core:make-flat-memmgr #:bitwidth 64))
  (define cpu (init-cpu mm))
  cpu)

(define (run-all-checkers-on-insn insn cpu checkers)
  (for ([c checkers])
    (c insn cpu)))

;; First, run all the checkers, then run the base interpreter.
(define (run-interpreters insn cpu #:base base-interp)
  (begin
    (cond
      [(is-store-insn? insn) (run-all-checkers-on-insn insn cpu on-store-checkers)]
      [(is-load-insn? insn) (run-all-checkers-on-insn insn cpu on-load-checkers)]
      [(is-alu-insn? insn) (run-all-checkers-on-insn insn cpu on-alu-checkers)])
    (base-interp insn cpu)))
                          
;; main visitor and interpreter
(clear-vc!) ; It feels like there is a bug somewhere that this is necessary.

(define insn-lines (objdump-file->insn-lines ftfp-obj-file sample-function))
(define is-convert-from-int64-definition-line? (lambda (insn-line)
                                                 (! (is-func-definition-line? insn-line sample-function))))
(define only-insn-lines (filter is-convert-from-int64-definition-line? insn-lines))
(define serval-insns (flatten (map text-line->serval-insn only-insn-lines)))
;(for ([insn serval-insns])
;  (displayln (format "insn: ~a" insn)))

; test concrete execution on straightline code
;(cpu-gpr-set! cpu rdi (bv 0 64)) ; first arg 0
(for ([insn serval-insns])
  (displayln (format "interpreting insn: ~a" insn))
  (let ([is-store (is-store-insn? insn)]
        [is-load (is-load-insn? insn)])
    (displayln (format "is-store-insn? ~a" is-store))
    (displayln (format "is-load-insn? ~a" is-load))
    (when (and is-store is-load)
      (raise "insn cannot be both load and store"))))
  ;(interpret-insn cpu insn))
; (render-value/window (cpu-gpr-ref cpu rax))
