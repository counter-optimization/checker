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


;; For defining hook functions for the uarch interpreters
(define x86-insn-defs-module-path "serval/serval/x86/interp.rkt")

(define (filter-exported-insn-names f module-path)
  (define-values (funcs-and-consts macros)
    (module->exports module-path))
  (define insn-export-names (cdr (car macros)))
  (define get-insn-name car)
  (filter f (map get-insn-name insn-export-names)))

(define (contains-r/m? x) (string-contains? x "r/m"))

;; TODO: these should really be added to the define-insn macros
(define (is-store-insn? insn-name)
  (match (string-split (symbol->string insn-name) "-")
    [(list opcode (? contains-r/m?) src) #t]
    [(list "push" rst ...) #t]
    [_ #f]))

(define (is-load-insn? insn-name)
  (match (string-split (symbol->string insn-name) "-")
    [(list opcode dst (? contains-r/m?)) #t]
    [(list (or "pop" "leave") rst ...) #t]
    [_ #f]))

(define (get-store-insns module-path)
  (filter-exported-insn-names is-store-insn? module-path))

(define (get-load-insns module-path)
  (filter-exported-insn-names is-load-insn? module-path))

(define (to-be-handled)
  (define found
    (append (get-store-insns x86-insn-defs-module-path)
            (get-load-insns x86-insn-defs-module-path)))
  (define all-insns (filter-exported-insn-names identity x86-insn-defs-module-path))
  (filter (lambda (insn) (not (member insn found))) all-insns))
  
;; main visitor and interpreter stuff

(clear-vc!)
(define insn-lines (objdump-file->insn-lines ftfp-obj-file sample-function))
(define is-convert-from-int64-definition-line? (lambda (insn-line)
                                                 (! (is-func-definition-line? insn-line sample-function))))
(define only-insn-lines (filter is-convert-from-int64-definition-line? insn-lines))
(define serval-insns (flatten (map text-line->serval-insn only-insn-lines)))
;(for ([insn serval-insns])
;  (displayln (format "insn: ~a" insn)))

; test concrete execution on straightline code
(define mm (core:make-flat-memmgr #:bitwidth 64))
(define cpu (init-cpu mm))
(cpu-gpr-set! cpu rdi (bv 0 64)) ; first arg 0
(for ([insn serval-insns])
  ;(displayln (format "interpreting insn: ~a" insn))
  (interpret-insn cpu insn))
; (render-value/window (cpu-gpr-ref cpu rax))