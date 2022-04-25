#lang rosette

(require "serval/serval/x86.rkt"
         "serval/serval/x86/interp/encoding.rkt")

(provide (all-defined-out))

(define ftfp-obj-file "ftfp.o.txt")
(define sample-function "_fix_convert_from_int64")

;; Hard coding in how llvm's objdump dumps out disassembly of an object file
(define (is-func-definition? funcname)
  (lambda (line)
    (define objdump-formatted-name (string-append "<" funcname ">:"))
    (string-contains? line objdump-formatted-name)))

(define (objdump-file->insns filename funcname)
  (define lines (port->lines (open-input-file filename #:mode 'text)))
  (set! lines (map string-trim lines))
    
  (define func-start (memf (is-func-definition? funcname) lines))
  (define func-end (index-of func-start ""))
  (take func-start func-end))

(define (text-line->serval-insn text-line)
  (define regexp-result (regexp-match* #px"^([a-f0-9]+):(\\s[a-f0-9]{2})+" text-line))
  (define match (car regexp-result))
  (define normalized-results (map string-trim (string-split match ":")))
  (define address (car normalized-results))
  (define bytes (string-split (cadr normalized-results)))
  (define insn-bytes-list (for/list ([byte-string bytes])
                            (bv (string->number byte-string 16) 8)))
  (displayln (format "insn bytes are: ~a" insn-bytes-list))
  (decode insn-bytes-list))


(clear-vc!)
(for ([line (objdump-file->insns ftfp-obj-file sample-function)])
  (define check-for-func-name (is-func-definition? sample-function))
  (displayln line)
  (unless (check-for-func-name line)
    (displayln (text-line->serval-insn line))))
