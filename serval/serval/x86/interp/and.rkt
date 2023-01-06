#lang rosette

(require
  "common.rkt")

(provide
  and-eax-imm32
  and-rax-imm32
  and-r/m8-imm8
  and-r/m32-imm32
  and-r/m64-imm32
  and-r/m32-imm8
  and-r/m64-imm8
  and-r/m8-r8
  and-r/m16-r16
  and-r/m32-r32
  and-r/m64-r64
  and-r32-r/m32
  and-r64-r/m64)

(define (interpret-and cpu dst v2)
  (define v1 (cpu-gpr-ref cpu dst))
  (define result (bvand v1 v2))
  (cpu-gpr-set! cpu dst result)
  (cpu-pf+zf+sf-set! cpu result)
  (cpu-flag-clear! cpu 'CF 'OF 'AF))

; 80 /4 ib
(define-insn and-r/m8-imm8 (dst imm8)
  #:decode [((byte #x80) (/4 r/m) i0)
            (list (gpr8-no-rex r/m) (decode-imm i0))]
  #:encode (list (byte #x80) (/4 dst) (encode-imm imm8))
  (lambda (cpu dst imm8)
    (interpret-and cpu dst imm8)))

; 24 ib
(define-insn and-al-imm8 (imm8)
  #:decode [((byte #x24) i0)
            (list (decode-imm i0))]
  #:encode (list (byte #x24) (encode-imm imm8))
  (lambda (cpu imm8)
    (interpret-and cpu al imm8)))

; 25 id
(define-insn and-eax-imm32 (imm32)
  #:decode [((byte #x25) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (byte #x25) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-and cpu eax imm32)))

; REX.W + 25 id
(define-insn and-rax-imm32 (imm32)
  #:decode [((rex.w/r) (byte #x25) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r) (byte #x25) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-and cpu rax (sign-extend imm32 (bitvector 64)))))

; 81 /4 id
(define-insn and-r/m32-imm32 (dst imm32)
  #:decode [((byte #x81) (/4 r/m) i0 i1 i2 i3)
            (list (gpr32-no-rex r/m) (decode-imm i0 i1 i2 i3))]
           [((rex/r b) (byte #x81) (/4 r/m) i0 i1 i2 i3)
            (list (gpr32 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex/r dst) (byte #x81) (/4 dst) (encode-imm imm32))
  (lambda (cpu dst imm32)
    (interpret-and cpu dst imm32)))

; REX.W + 81 /4 id
(define-insn and-r/m64-imm32 (dst imm32)
  #:decode [((rex.w/r b) (byte #x81) (/4 r/m) i0 i1 i2 i3)
            (list (gpr64 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r dst) (byte #x81) (/4 dst) (encode-imm imm32))
  (lambda (cpu dst imm32)
    (interpret-and cpu dst (sign-extend imm32 (bitvector 64)))))

; 83 /4 ib
(define-insn and-r/m32-imm8 (dst imm8)
  #:decode [((byte #x83) (/4 r/m) i0)
            (list (gpr32-no-rex r/m) i0)]
           [((rex/r b) (byte #x83) (/4 r/m) i0)
            (list (gpr32 b r/m) i0)]
  #:encode (list (rex/r dst) (byte #x83) (/4 dst) (encode-imm imm8))
  (lambda (cpu dst imm8)
    (interpret-and cpu dst (sign-extend imm8 (bitvector 32)))))

; REX.W + 83 /4 ib
(define-insn and-r/m64-imm8 (dst imm8)
  #:decode [((rex.w/r b) (byte #x83) (/4 r/m) i0)
            (list (gpr64 b r/m) i0)]
  #:encode (list (rex.w/r dst) (byte #x83) (/4 dst) (encode-imm imm8))
  (lambda (cpu dst imm32)
    (interpret-and cpu dst (sign-extend imm8 (bitvector 64)))))

; 20 /r
(define-insn and-r/m8-r8 (dst src)
  #:decode [((byte #x20) (/r reg r/m))
            (list (gpr8-no-rex r/m) (gpr8-no-rex reg))]
           [((rex/r r b) (byte #x20) (/r reg r/m))
            (list (gpr8 b r/m) (gpr8 r reg))]
  #:encode (list (rex/r src dst) (byte #x20) (/r src dst))
  (lambda (cpu dst src)
    (interpret-and cpu dst (cpu-gpr-ref cpu src))))

; 21 /r
(define-insn and-r/m16-r16 (dst src)
  #:decode [((byte #x21) (/r reg r/m))
            (list (gpr16-no-rex r/m) (gpr16-no-rex reg))]
           [((rex/r r b) (byte #x21) (/r reg r/m))
            (list (gpr16 b r/m) (gpr16 r reg))]
  #:encode (list (rex/r src dst) (byte #x21) (/r src dst))
  (lambda (cpu dst src)
    (interpret-and cpu dst (cpu-gpr-ref cpu src))))

; 21 /r
(define-insn and-r/m32-r32 (dst src)
  #:decode [((byte #x21) (/r reg r/m))
            (list (gpr32-no-rex r/m) (gpr32-no-rex reg))]
           [((rex/r r b) (byte #x21) (/r reg r/m))
            (list (gpr32 b r/m) (gpr32 r reg))]
  #:encode (list (rex/r src dst) (byte #x21) (/r src dst))
  (lambda (cpu dst src)
    (interpret-and cpu dst (cpu-gpr-ref cpu src))))

; REX.W + 21 /r
(define-insn and-r/m64-r64 (dst src)
  #:decode [((rex.w/r r b) (byte #x21) (/r reg r/m))
            (list (gpr64 b r/m) (gpr64 r reg))]
  #:encode (list (rex.w/r src dst) (byte #x21) (/r src dst))
  (lambda (cpu dst src)
    (interpret-and cpu dst (cpu-gpr-ref cpu src))))

; 23 /r
(define-insn and-r32-r/m32 (dst src)
  #:decode [((byte #x23) (/r reg r/m))
            (list (gpr32-no-rex reg) (gpr32-no-rex r/m))]
           [((rex/r r b) (byte #x23) (/r reg r/m))
            (list (gpr32 r reg) (gpr32 b r/m))]
  #:encode (list (rex/r dst src) (byte #x23) (/r dst src))
  (lambda (cpu dst src)
    (interpret-and cpu dst (cpu-gpr-ref cpu src))))

; REX.W + 23 /r
(define-insn and-r64-r/m64 (dst src)
  #:decode [((rex.w/r r b) (byte #x23) (/r reg r/m))
            (list (gpr64 r reg) (gpr64 b r/m))]
           [((rex.w r (== (bv #b0 1)) b) (byte #x23) (modr/m.01 reg r/m) disp8)
            (list (gpr64 r reg) (register-indirect (gpr64 b r/m) disp8 64))]
  #:encode (if (register-indirect? src)
               (match-let ([(list ext mod num disp) (register-encode src)])
                 (list
                  (rex.w dst (bv #b0 1) src)
                  (byte #x23)
                  (modr/m.01 dst num)
                  disp))
               (list (rex.w/r dst src) (byte #x23) (/r dst src)))
  (lambda (cpu dst src)
    (interpret-and cpu dst (cpu-gpr-ref cpu src))))
