#lang rosette

(require
  "common.rkt")

(provide
  imul-r/m16 imul-r/m32 imul-r/m64
  imul-r32-r/m32
  imul-r64-r/m64
  imul-r64-r/m64-imm8
  imul-r64-r/m64-imm32)


(define (interpret-imul cpu src n)
  (define v1 (trunc n (cpu-gpr-ref cpu rax)))
  (define v2 (cpu-gpr-ref cpu src))
  (define lower ((core:bvmul-proc) v1 v2))
  (define upper ((core:bvmulhsu-proc) v1 v2))
  (define upper-bit 
    (if (bveq (sign-extend lower (bitvector (* 2 n))) (concat upper lower))
        (bv #b0 1)
        (bv #b1 1)))
  (cpu-gpr-set! cpu rax (zero-extend lower (bitvector 64)))
  (cpu-gpr-set! cpu rdx (zero-extend upper (bitvector 64)))
  ; CF and OF are cleared if the higher-order bits are 0
  (cpu-flag-set! cpu 'CF upper-bit)
  (cpu-flag-set! cpu 'OF upper-bit)
  ; SF, ZF, AF, and PF are undefined
  (cpu-flag-havoc! cpu 'SF 'ZF 'AF 'PF))

(define (interpret-imul-2 cpu dst src n)
  (define v1 (cpu-gpr-ref cpu dst))
  (define v2 (cpu-gpr-ref cpu src))
  (define lower ((core:bvmul-proc) v1 v2))
  (define upper ((core:bvmulhsu-proc) v1 v2))
  (define upper-bit 
    (if (bveq (sign-extend lower (bitvector (* 2 n))) 
              (concat upper lower))
        (bv #b0 1)
        (bv #b1 1)))
  (cpu-gpr-set! cpu dst lower)
  ; CF and OF are cleared if the higher-order bits are 0
  (cpu-flag-set! cpu 'CF upper-bit)
  (cpu-flag-set! cpu 'OF upper-bit)
  ; SF, ZF, AF, and PF are undefined
  (cpu-flag-havoc! cpu 'SF 'ZF 'AF 'PF))

(define (interpret-imul-3 cpu dst src imm n)
  (define v1 (cpu-gpr-ref cpu src))
  (define v2 imm)
  (define lower ((core:bvmul-proc) v1 v2))
  (define upper ((core:bvmulhsu-proc) v1 v2))
  (define upper-bit 
    (if (bveq (sign-extend lower (bitvector (* 2 n))) 
              (concat upper lower))
        (bv #b0 1)
        (bv #b1 1)))
  (cpu-gpr-set! cpu dst lower)
  ; CF and OF are cleared if the higher-order bits are 0
  (cpu-flag-set! cpu 'CF upper-bit)
  (cpu-flag-set! cpu 'OF upper-bit)
  ; SF, ZF, AF, and PF are undefined
  (cpu-flag-havoc! cpu 'SF 'ZF 'AF 'PF))

; F7 /5
(define-insn imul-r/m16 (src)
  #:decode [((byte #xF7) (/5 r/m))
            (list (gpr16-no-rex r/m))]
           [((rex/r b) (byte #xF7) (/5 r/m))
            (list (gpr16 b r/m))]
  #:encode (list (rex/r src) (byte #xF7) (/5 src))
  (lambda (cpu src)
    (interpret-imul cpu src 16)))

; F7 /5
(define-insn imul-r/m32 (src)
  #:decode [((byte #xF7) (/5 r/m))
            (list (gpr32-no-rex r/m))]
           [((rex/r b) (byte #xF7) (/5 r/m))
            (list (gpr32 b r/m))]
  #:encode (list (rex/r src) (byte #xF7) (/5 src))
  (lambda (cpu src)
    (interpret-imul cpu src 32)))

(define-insn imul-m32 (src)
  #:decode [((byte #xF7) (modr/m (== (bv #b01 2)) (== (bv 5 3)) r/m) disp8)
            (list (register-indirect (gpr64-no-rex r/m) disp8 32))]
           [((byte #xF7) (modr/m (== (bv #b10 2)) (== (bv 5 3)) r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64-no-rex r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 32))]
           [((rex/r b) (byte #xF7) (modr/m (== (bv #b01 2)) (== (bv 5 3)) r/m) disp8)
            (list (register-indirect (gpr64 b r/m) disp8 32))]
           [((rex/r b) (byte #xF7) (modr/m (== (bv #b10 2)) (== (bv 5 3)) r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 32))]
  #:encode (let ([e (register-encode src)])
             (list (rex/r (first e)) (byte #xF7) (modr/m (second e) (bv 5 3) (third e)) (fourth e)))
  (lambda (cpu src)
    (interpret-imul cpu src 32)))

; REX.W + F7 /5
(define-insn imul-r/m64 (src)
  #:decode [((rex.w/r b) (byte #xF7) (/5 r/m))
            (list (gpr64 b r/m))]
  #:encode (list (rex.w/r src) (byte #xF7) (/5 src))
  (lambda (cpu src)
    (interpret-imul cpu src 64)))

; 0F AF /r
(define-insn imul-r32-r/m32 (dst src)
  #:decode [((byte #x0F) (byte #xAF) (/r reg r/m))
            (list (gpr32-no-rex reg) (gpr32-no-rex r/m))]
           [((rex/r r b) (byte #x0F) (byte #xAF) (/r reg r/m))
            (list (gpr32 r reg) (gpr32 b r/m))]
  #:encode (list (rex/r dst src) (byte #x0F) (byte #xAF) (/r dst src))
  (lambda (cpu dst src)
    (interpret-imul-2 cpu dst src 32)))

; REX.W 0F AF /r
(define-insn imul-r64-r/m64 (dst src)
  #:decode [((rex.w/r b) (byte #x0F) (byte #xAF) (/r reg r/m))
            (list (gpr64 b reg) (gpr64 b r/m))]
  #:encode (list (rex.w/r dst src) (byte #x0F) (byte #xAF) (/r dst src))
  (lambda (cpu dst src)
    (interpret-imul-2 cpu dst src 64)))

; REX.W 6B /r ib
(define-insn imul-r64-r/m64-imm8 (dst src imm8)
  #:decode [((rex.w/r b) (byte #x6B) (/r reg r/m) i0)
            (list (gpr64 b reg) (gpr64 b r/m) (decode-imm i0))]
  #:encode (list (rex.w/r dst src) (byte #x6B) (/r dst src) (encode-imm imm8))
  (lambda (cpu dst src imm8)
    (interpret-imul-3 cpu dst src (sign-extend imm8 (bitvector 64)) 64)))

; REX.W 69 /r id
(define-insn imul-r64-r/m64-imm32 (dst src imm32)
  #:decode [((rex.w/r b) (byte #x69) (/r reg r/m) i0 i1 i2 i3)
            (list (gpr64 b reg) (gpr64 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r dst src) (byte #x6B) (/r dst src) (encode-imm imm32))
  (lambda (cpu dst src imm32)
    (interpret-imul-3 cpu dst src (sign-extend imm32 (bitvector 64)) 64)))
