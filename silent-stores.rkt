#lang rosette

(require "serval/serval/x86.rkt"
         "serval/serval/x86/interp/encoding.rkt")

(provide (all-defined-out))

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
  (displayln (format "insn bytes are: ~a" insn-bytes-list))
  (define serval-insn (decode insn-bytes-list))
  (displayln serval-insn)
  serval-insn)


(clear-vc!)
(define insn-lines (objdump-file->insn-lines ftfp-obj-file sample-function))
(define is-convert-from-int64-definition-line? (lambda (insn-line)
                                                 (! (is-func-definition-line? insn-line sample-function))))
(define only-insn-lines (filter is-convert-from-int64-definition-line? insn-lines))
(map text-line->serval-insn only-insn-lines)
