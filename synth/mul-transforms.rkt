#lang rosette

(require 
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

(provide (all-defined-out))

; --------------- IMUL ----------------

(define spec-imul32
  (list (imul-r/m32 ecx)))

(define spec-imul32-rr
  (list (imul-r32-r/m32 eax ecx)))

(define spec-imul64
  (list (imul-r/m64 rcx)))

(define spec-imul64-rr
  (list (imul-r64-r/m64 rax rcx)))

(define attempt-imul64-rri8
  (list
    (mov-r64-imm64 rax (bv 0 64))
    (add-r/m64-imm8 rax (comp-simp:imm8))
    (imul-r64-r/m64 rax rcx)))

(define spec-imul64-rri8
  (list (imul-r64-r/m64-imm8 rax rcx (comp-simp:imm8))))

(define attempt-imul64-rri32
  (list
    (mov-r64-imm64 rax (bv 0 64))
    (add-r/m64-imm32 rax (comp-simp:imm32))
    (imul-r64-r/m64 rax rcx)))

(define spec-imul64-rri32
  (list (imul-r64-r/m64-imm32 rax rcx (comp-simp:imm32))))


; ---------------- MUL ----------------

; Dest rdx:rax, src rcx
(define regs-mul
  (list rdx rax rcx))

; (define attempt-mul16
;   (list
;   ; (mul-r/m16 cx)
;    (mov-r/m64-r64 r11 rax) ; save rax to r11
;    (mov-r/m64-imm32 r10 (bv 0 32)) ; zero r10
;    (mov-r/m16-r16 r10w cx) ; use r10 as operand
;    (mov-r/m32-imm32 eax (bv 0 32)) ; zero rax
;    (mov-r/m16-r16 ax r11w) ; move lower 16 bits back as operand
;    (mul-r/m32 r10d) ; multiply
;    (mov-r/m16-r16 r11w ax) ; move lower product to r11
;    (shr-r/m32-imm8 eax (bv 16 8)) ; shift over upper product
;    (mov-r/m16-r16 dx ax) ; move upper product to dx
;    (mov-r/m64-r64 rax r11) ; set rax to correct result
;   ))

; (define spec-mul16
;   (list
;    (mul-r/m16 cx)))

(define attempt-mul32
  (list
  ; safe to use rdx for scratch; it'll be overwritten later regardless
   (mov-r64-imm64 rdx (bv (expt 2 63) 64))
   ; set r10 = ecx + 2^63, r11 = eax + 2^63 (equivalent to ecx/eax - 2^63)
   (mov-r/m32-r32 r10d ecx)
   (sub-r/m64-r64 r10 rdx)
   (mov-r/m32-r32 r11d eax)
   (sub-r/m64-r64 r11 rdx)
   ; replace rax with eax + 2^63
   (mov-r/m64-r64 rax r11)
   ; perform 64-bit mul of modified operands. This gives us
   ; (eax + 2^63) * (ecx + 2^63)
   ; = eax * ecx + eax * 2^63 + ecx * 2^63 + 2^126
   (mul-r/m64 r10)
   ; product is in rax. subtract shifted r11, r10 to get expected value
   ; (equivalent to subtracting eax * 2^63 and ecx * 2^63)
   (shl-r/m64-imm8 r10 (bv 63 8))
   (mov-r/m8-imm8 r10b (bv 1 8))
   (shl-r/m64-imm8 r11 (bv 63 8))
   (mov-r/m8-imm8 r11b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m64-r64 rax r10)
   (sub-r/m64-r64 rax r11)
   (mov-r/m8-r8 al dl)
   ; set edx to contain upper 32 bits of the product (from rax)
   (mov-r/m64-r64 rdx rax)
   (mov-r/m16-imm16 dx (bv 1 16)) ; mask lower bits for safe shift
   (shr-r/m64-imm8 rdx (bv 32 8))
   ; clear upper 32 bits of rax
   (mov-r/m32-r32 eax eax)
  ))

(define spec-mul32
  (list
   (mul-r/m32 ecx)))

(define attempt-mul16
  (list
   ; save arguments (ax and cx)
   (mov-r/m16-r16 r10w cx)
   (mov-r/m16-r16 r11w ax)

   ; calculate first term: x[7:] * y[7:]
    ; prepare arguments
   (mov-r/m16-imm16 dx (bv (expt 2 15) 16))
   (mov-r/m16-imm16 cx (bv 0 16)) 
   (mov-r/m8-r8 cl r10b)
   (sub-r/m16-r16 cx dx)

   (mov-r/m16-imm16 ax (bv 0 16)) 
   (mov-r/m8-r8 al r11b) 
   (sub-r/m16-r16 ax dx)
   (mov-r/m16-r16 r12w ax)
    ; perform safe mul
   (mul-r/m16 cx)
    ; revert mask in result
   (shl-r/m16-imm8 cx (bv 15 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m16-imm8 r12w (bv 15 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m16-r16 ax cx)
   (sub-r/m16-r16 ax r12w)
   (mov-r/m8-r8 al dl)
    ; save result
   (mov-r/m16-r16 r13w ax)

   ; calculate 2nd term: x[7:] * y[:8]
    ; prepare arguments
   (mov-r/m16-imm16 dx (bv (expt 2 15) 16))
   (mov-r/m16-imm16 cx (bv 0 16)) 
   (mov-r/m8-r8 cl r10b)
   (sub-r/m16-r16 cx dx)

   (mov-r/m16-r16 ax r11w)
   (mov-r/m8-imm8 al (bv 1 8))
   (shr-r/m16-imm8 ax (bv 8 8))
   (sub-r/m16-r16 ax dx)
   (mov-r/m16-r16 r12w ax)
    ; perform safe mul
   (mul-r/m16 cx)
    ; revert mask in result
   (shl-r/m16-imm8 cx (bv 15 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m16-imm8 r12w (bv 15 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m16-r16 ax cx)
   (sub-r/m16-r16 ax r12w)
   (mov-r/m8-r8 al dl)
    ; save result
    ; use PUSH in actual transform
   (mov-r/m16-r16 r14w ax)

   ; calculate 3rd term: x[:8] * y[7:]
    ; prepare arguments
   (mov-r/m16-imm16 dx (bv (expt 2 15) 16))
   (mov-r/m16-r16 cx r10w)
   (mov-r/m8-imm8 cl (bv 1 8))
   (shr-r/m16-imm8 cx (bv 8 8))
   (sub-r/m16-r16 cx dx)

   (mov-r/m16-imm16 ax (bv 0 16)) 
   (mov-r/m8-r8 al r11b) 
   (sub-r/m16-r16 ax dx)
   (mov-r/m16-r16 r12w ax)
    ; perform safe mul
   (mul-r/m16 cx)
    ; revert mask in result
   (shl-r/m16-imm8 cx (bv 15 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m16-imm8 r12w (bv 15 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m16-r16 ax cx)
   (sub-r/m16-r16 ax r12w)
   (mov-r/m8-r8 al dl)
    ; save result
    ; use PUSH in actual transform
   (mov-r/m16-r16 r15w ax)

   ; calculate 4th term: x[:8] * y[:8]
    ; prepare arguments
   (mov-r/m16-imm16 dx (bv (expt 2 15) 16))
   (mov-r/m16-r16 cx r10w)
   (mov-r/m8-imm8 cl (bv 1 8))
   (shr-r/m16-imm8 cx (bv 8 8))
   (sub-r/m16-r16 cx dx)

   (mov-r/m16-r16 ax r11w)
   (mov-r/m8-imm8 al (bv 1 8))
   (shr-r/m16-imm8 ax (bv 8 8))
   (sub-r/m16-r16 ax dx)
   (mov-r/m16-r16 r12w ax)
    ; perform safe mul
   (mul-r/m16 cx)
    ; revert mask in result
   (shl-r/m16-imm8 cx (bv 15 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m16-imm8 r12w (bv 15 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m16-r16 ax cx)
   (sub-r/m16-r16 ax r12w)
   (mov-r/m8-r8 al dl)
    ; ax contains result

   ; recombine terms
    ; put 4th term (upper 16 bits) in dl
   (mov-r/m16-r16 dx ax)
    ; put 1st term (lower 16 bits) in al
   (mov-r/m16-r16 ax r13w)
    ; safely add 3rd term (middle 16 bits)
      (mov-r/m16-r16 cx r15w) ; use POP in real transform
      (mov-r/m16-imm16 r11w (bv 0 16))
      (shld-r/m16-r16-imm8 r11w cx (bv 8 8))
      ; now r11w has the upper 8 bits of 3rd term in its lower half
      (shl-r/m16-imm8 cx (bv 8 8))
      (shr-r/m16-imm8 cx (bv 8 8))
      (mov-r/m16-imm16 r13w (bv (expt 2 7) 16))
      (sub-r/m16-r16 cx r13w)
      (sub-r/m16-r16 cx r13w)
      (shl-r/m16-imm8 cx (bv 8 8))
      ; now rcx has the lower 32 bits of 3rd term in its upper half
      ; mask lower bits in cx, ax so we can add safely
      (mov-r/m8-r8 r12b al)
      (mov-r/m8-imm8 al (bv 1 8))
      (mov-r/m8-imm8 cl (bv 1 8))
      ; perform addition
      (add-r/m16-r16 ax cx)
      (adc-r/m16-r16 dx r11w)
      (mov-r/m8-r8 al r12b)
    ; safely add 2nd term (middle 64 bits)
      (mov-r/m16-r16 cx r14w) ; use POP in real transform
      (mov-r/m16-imm16 r11w (bv 0 16))
      (shld-r/m16-r16-imm8 r11w cx (bv 8 8))
      ; now r11 has the upper 32 bits of 3rd term in its lower half
      (shl-r/m16-imm8 cx (bv 8 8))
      (shr-r/m16-imm8 cx (bv 8 8))
      (mov-r/m16-imm16 r13w (bv (expt 2 7) 16))
      (sub-r/m16-r16 cx r13w)
      (sub-r/m16-r16 cx r13w)
      (shl-r/m16-imm8 cx (bv 8 8))
      ; now rcx has the lower 32 bits of 3rd term in its upper half
      ; mask lower bits in rcx, rax so we can add safely
      (mov-r/m8-r8 r12b al)
      (mov-r/m8-imm8 al (bv 1 8))
      (mov-r/m8-imm8 cl (bv 1 8))
      ; perform addition
      (add-r/m16-r16 ax cx)
      (adc-r/m16-r16 dx r11w)
      (mov-r/m8-r8 al r12b)
    ; product should be correct now

   ; restore rcx
   (mov-r/m16-r16 cx r10w)
  ))

(define spec-mul16
  (list
   (mul-r/m16 cx)))

(define attempt-mul16-p123
  (list
   ; save arguments (ax and cx)
   (mov-r/m16-r16 r10w cx)
   (mov-r/m16-r16 r11w ax)

   ; calculate first term: x[7:] * y[7:]
    ; prepare arguments
   (mov-r/m16-imm16 dx (bv (expt 2 15) 16))
   (mov-r/m16-imm16 cx (bv 0 16)) 
   (mov-r/m8-r8 cl r10b)
   (sub-r/m16-r16 cx dx)

   (mov-r/m16-imm16 ax (bv 0 16)) 
   (mov-r/m8-r8 al r11b) 
   (sub-r/m16-r16 ax dx)
   (mov-r/m16-r16 r12w ax)
    ; perform safe mul
   (mul-r/m16 cx)
    ; revert mask in result
   (shl-r/m16-imm8 cx (bv 15 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m16-imm8 r12w (bv 15 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m16-r16 ax cx)
   (sub-r/m16-r16 ax r12w)
   (mov-r/m8-r8 al dl)
    ; save result
   (mov-r/m16-r16 r13w ax)

   ; calculate 2nd term: x[31:] * y[:32]
    ; prepare arguments
   (mov-r/m16-imm16 dx (bv (expt 2 15) 16))
   (mov-r/m16-imm16 cx (bv 0 16)) 
   (mov-r/m8-r8 cl r10b)
   (sub-r/m16-r16 cx dx)

   (mov-r/m16-r16 ax r11w)
   (mov-r/m8-imm8 al (bv 1 8))
   (shr-r/m16-imm8 ax (bv 8 8))
   (sub-r/m16-r16 ax dx)
   (mov-r/m16-r16 r12w ax)
    ; perform safe mul
   (mul-r/m16 cx)
    ; revert mask in result
   (shl-r/m16-imm8 cx (bv 15 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m16-imm8 r12w (bv 15 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m16-r16 ax cx)
   (sub-r/m16-r16 ax r12w)
   (mov-r/m8-r8 al dl)
    ; save result
    ; use PUSH in actual transform
   (mov-r/m16-r16 r14w ax)

   ; calculate 3rd term: x[:8] * y[7:]
    ; prepare arguments
   (mov-r/m16-imm16 dx (bv (expt 2 15) 16))
   (mov-r/m16-r16 cx r10w)
   (mov-r/m8-imm8 cl (bv 1 8))
   (shr-r/m16-imm8 cx (bv 8 8))
   (sub-r/m16-r16 cx dx)

   (mov-r/m16-imm16 ax (bv 0 16)) 
   (mov-r/m8-r8 al r11b) 
   (sub-r/m16-r16 ax dx)
   (mov-r/m16-r16 r12w ax)
    ; perform safe mul
   (mul-r/m16 cx)
    ; revert mask in result
   (shl-r/m16-imm8 cx (bv 15 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m16-imm8 r12w (bv 15 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m16-r16 ax cx)
   (sub-r/m16-r16 ax r12w)
   (mov-r/m8-r8 al dl)
    ; save result
    ; use PUSH in actual transform
   (mov-r/m16-r16 r15w ax)

   (mov-r/m16-imm16 dx (bv 0 16))
    ; put 1st term (lower 16 bits) in al
   (mov-r/m16-r16 ax r13w)
    ; safely add 3rd term (middle 16 bits)
      (mov-r/m16-r16 cx r15w) ; use POP in real transform
      (mov-r/m16-imm16 r11w (bv 0 16))
      (shld-r/m16-r16-imm8 r11w cx (bv 8 8))
      ; now r11w has the upper 8 bits of 3rd term in its lower half
      (shl-r/m16-imm8 cx (bv 8 8))
      (shr-r/m16-imm8 cx (bv 8 8))
      (mov-r/m16-imm16 r13w (bv (expt 2 7) 16))
      (sub-r/m16-r16 cx r13w)
      (sub-r/m16-r16 cx r13w)
      (shl-r/m16-imm8 cx (bv 8 8))
      ; now rcx has the lower 32 bits of 3rd term in its upper half
      ; mask lower bits in cx, ax so we can add safely
      (mov-r/m8-r8 r12b al)
      (mov-r/m8-imm8 al (bv 1 8))
      (mov-r/m8-imm8 cl (bv 1 8))
      ; perform addition
      (add-r/m16-r16 ax cx)
      (adc-r/m16-r16 dx r11w)
      (mov-r/m8-r8 al r12b)
    ; safely add 2nd term (middle 64 bits)
      (mov-r/m16-r16 cx r14w) ; use POP in real transform
      (mov-r/m16-imm16 r11w (bv 0 16))
      (shld-r/m16-r16-imm8 r11w cx (bv 8 8))
      ; now r11 has the upper 32 bits of 3rd term in its lower half
      (shl-r/m16-imm8 cx (bv 8 8))
      (shr-r/m16-imm8 cx (bv 8 8))
      (mov-r/m16-imm16 r13w (bv (expt 2 7) 16))
      (sub-r/m16-r16 cx r13w)
      (sub-r/m16-r16 cx r13w)
      (shl-r/m16-imm8 cx (bv 8 8))
      ; now rcx has the lower 32 bits of 3rd term in its upper half
      ; mask lower bits in rcx, rax so we can add safely
      (mov-r/m8-r8 r12b al)
      (mov-r/m8-imm8 al (bv 1 8))
      (mov-r/m8-imm8 cl (bv 1 8))
      ; perform addition
      (add-r/m16-r16 ax cx)
      (adc-r/m16-r16 dx r11w)
      (mov-r/m8-r8 al r12b)
    ; product should be correct now

   ; restore rcx
   (mov-r/m16-r16 cx r10w)
  ))

(define spec-mul16-p123
  (list
   (mul-r/m16 cx)))

(define attempt-mul16-p12
  (list
   ; save arguments (ax and cx)
   (mov-r/m16-r16 r10w cx)
   (mov-r/m16-r16 r11w ax)

   ; calculate first term: x[7:] * y[7:]
    ; prepare arguments
   (mov-r/m16-imm16 dx (bv (expt 2 15) 16))
   (mov-r/m16-imm16 cx (bv 0 16)) 
   (mov-r/m8-r8 cl r10b)
   (sub-r/m16-r16 cx dx)

   (mov-r/m16-imm16 ax (bv 0 16)) 
   (mov-r/m8-r8 al r11b) 
   (sub-r/m16-r16 ax dx)
   (mov-r/m16-r16 r12w ax)
    ; perform safe mul
   (mul-r/m16 cx)
    ; revert mask in result
   (shl-r/m16-imm8 cx (bv 15 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m16-imm8 r12w (bv 15 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m16-r16 ax cx)
   (sub-r/m16-r16 ax r12w)
   (mov-r/m8-r8 al dl)
    ; save result
   (mov-r/m16-r16 r13w ax)

   ; calculate 2nd term: x[31:] * y[:32]
    ; prepare arguments
   (mov-r/m16-imm16 dx (bv (expt 2 15) 16))
   (mov-r/m16-imm16 cx (bv 0 16)) 
   (mov-r/m8-r8 cl r10b)
   (sub-r/m16-r16 cx dx)

   (mov-r/m16-r16 ax r11w)
   (mov-r/m8-imm8 al (bv 1 8))
   (shr-r/m16-imm8 ax (bv 8 8))
   (sub-r/m16-r16 ax dx)
   (mov-r/m16-r16 r12w ax)
    ; perform safe mul
   (mul-r/m16 cx)
    ; revert mask in result
   (shl-r/m16-imm8 cx (bv 15 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m16-imm8 r12w (bv 15 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m16-r16 ax cx)
   (sub-r/m16-r16 ax r12w)
   (mov-r/m8-r8 al dl)
    ; save result
    ; use PUSH in actual transform
   (mov-r/m16-r16 r14w ax)

   (mov-r/m16-imm16 dx (bv 0 16))
    ; put 1st term (lower 16 bits) in al
   (mov-r/m16-r16 ax r13w)
    ; safely add 2nd term (middle 64 bits)
      (mov-r/m16-r16 cx r14w) ; use POP in real transform
      (mov-r/m16-imm16 r11w (bv 0 16))
      (shld-r/m16-r16-imm8 r11w cx (bv 8 8))
      ; now r11 has the upper 32 bits of 3rd term in its lower half
      (shl-r/m16-imm8 cx (bv 8 8))
      (shr-r/m16-imm8 cx (bv 8 8))
      (mov-r/m16-imm16 r13w (bv (expt 2 7) 16))
      (sub-r/m16-r16 cx r13w)
      (sub-r/m16-r16 cx r13w)
      (shl-r/m16-imm8 cx (bv 8 8))
      ; now rcx has the lower 32 bits of 3rd term in its upper half
      ; mask lower bits in rcx, rax so we can add safely
      (mov-r/m8-r8 r12b al)
      (mov-r/m8-imm8 al (bv 1 8))
      (mov-r/m8-imm8 cl (bv 1 8))
      ; perform addition
      (add-r/m16-r16 ax cx)
      (adc-r/m16-r16 dx r11w)
      (mov-r/m8-r8 al r12b)
    ; product should be correct now

   ; restore rcx
   (mov-r/m16-r16 cx r10w)
   (shl-r/m16-imm8 cx (bv 8 8))
   (shr-r/m16-imm8 cx (bv 8 8))
  ))

(define spec-mul16-p12
  (list
   (shl-r/m16-imm8 cx (bv 8 8))
   (shr-r/m16-imm8 cx (bv 8 8))
   (mul-r/m16 cx)))

(define attempt-mul16-p1
  (list
   ; save arguments (ax and cx)
   (mov-r/m16-r16 r10w cx)
   (mov-r/m16-r16 r11w ax)

   ; calculate first term: x[7:] * y[7:]
    ; prepare arguments
   (mov-r/m16-imm16 dx (bv (expt 2 15) 16))
   (mov-r/m16-imm16 cx (bv 0 16)) 
   (mov-r/m8-r8 cl r10b)
   (sub-r/m16-r16 cx dx)

   (mov-r/m16-imm16 ax (bv 0 16)) 
   (mov-r/m8-r8 al r11b) 
   (sub-r/m16-r16 ax dx)
   (mov-r/m16-r16 r12w ax)
    ; perform safe mul
   (mul-r/m16 cx)
    ; revert mask in result
   (shl-r/m16-imm8 cx (bv 15 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m16-imm8 r12w (bv 15 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m16-r16 ax cx)
   (sub-r/m16-r16 ax r12w)
   (mov-r/m8-r8 al dl)
    ; save result
   (mov-r/m16-r16 r13w ax)

   ; restore rcx
   (mov-r/m16-r16 cx r10w)
   (shl-r/m16-imm8 cx (bv 8 8))
   (shr-r/m16-imm8 cx (bv 8 8))
   (mov-r/m16-imm16 dx (bv 0 16))
  ))

(define spec-mul16-p1
  (list
   (shl-r/m16-imm8 ax (bv 8 8))
   (shr-r/m16-imm8 ax (bv 8 8))
   (shl-r/m16-imm8 cx (bv 8 8))
   (shr-r/m16-imm8 cx (bv 8 8))
   (mul-r/m16 cx)))

(define attempt-mul16-p2
  (list
   ; save arguments (ax and cx)
   (mov-r/m16-r16 r10w cx)
   (mov-r/m16-r16 r11w ax)

   ; calculate 2nd term: x[31:] * y[:32]
    ; prepare arguments
   (mov-r/m16-imm16 dx (bv (expt 2 15) 16))
   (mov-r/m16-imm16 cx (bv 0 16)) 
   (mov-r/m8-r8 cl r10b)
   (sub-r/m16-r16 cx dx)

   (mov-r/m16-r16 ax r11w)
   (mov-r/m8-imm8 al (bv 1 8))
   (shr-r/m16-imm8 ax (bv 8 8))
   (sub-r/m16-r16 ax dx)
   (mov-r/m16-r16 r12w ax)
    ; perform safe mul
   (mul-r/m16 cx)
    ; revert mask in result
   (shl-r/m16-imm8 cx (bv 15 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m16-imm8 r12w (bv 15 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m16-r16 ax cx)
   (sub-r/m16-r16 ax r12w)
   (mov-r/m8-r8 al dl)

   ; restore rcx
   (mov-r/m16-r16 cx r10w)
   (shl-r/m16-imm8 cx (bv 8 8))
   (shr-r/m16-imm8 cx (bv 8 8))
   (mov-r/m16-imm16 dx (bv 0 16))
  ))

(define spec-mul16-p2
  (list
   (shr-r/m16-imm8 ax (bv 8 8))
   (shl-r/m16-imm8 cx (bv 8 8))
   (shr-r/m16-imm8 cx (bv 8 8))
   (mul-r/m16 cx)))

(define attempt-mul16-p3
  (list
   ; save arguments (ax and cx)
   (mov-r/m16-r16 r10w cx)
   (mov-r/m16-r16 r11w ax)

   ; calculate 3rd term: x[:8] * y[7:]
    ; prepare arguments
   (mov-r/m16-imm16 dx (bv (expt 2 15) 16))
   (mov-r/m16-r16 cx r10w)
   (mov-r/m8-imm8 cl (bv 1 8))
   (shr-r/m16-imm8 cx (bv 8 8))
   (sub-r/m16-r16 cx dx)

   (mov-r/m16-imm16 ax (bv 0 16)) 
   (mov-r/m8-r8 al r11b) 
   (sub-r/m16-r16 ax dx)
   (mov-r/m16-r16 r12w ax)
    ; perform safe mul
   (mul-r/m16 cx)
    ; revert mask in result
   (shl-r/m16-imm8 cx (bv 15 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m16-imm8 r12w (bv 15 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m16-r16 ax cx)
   (sub-r/m16-r16 ax r12w)
   (mov-r/m8-r8 al dl)

   ; restore rcx
   (mov-r/m16-r16 cx r10w)
   (shr-r/m16-imm8 cx (bv 8 8))
   (mov-r/m16-imm16 dx (bv 0 16))
  ))

(define spec-mul16-p3
  (list
   (shl-r/m16-imm8 ax (bv 8 8))
   (shr-r/m16-imm8 ax (bv 8 8))
   (shr-r/m16-imm8 cx (bv 8 8))
   (mul-r/m16 cx)))

(define attempt-mul16-p4
  (list
   ; save arguments (ax and cx)
   (mov-r/m16-r16 r10w cx)
   (mov-r/m16-r16 r11w ax)

   ; calculate 4th term: x[:8] * y[:8]
    ; prepare arguments
   (mov-r/m16-imm16 dx (bv (expt 2 15) 16))
   (mov-r/m16-r16 cx r10w)
   (mov-r/m8-imm8 cl (bv 1 8))
   (shr-r/m16-imm8 cx (bv 8 8))
   (sub-r/m16-r16 cx dx)

   (mov-r/m16-r16 ax r11w)
   (mov-r/m8-imm8 al (bv 1 8))
   (shr-r/m16-imm8 ax (bv 8 8))
   (sub-r/m16-r16 ax dx)
   (mov-r/m16-r16 r12w ax)
    ; perform safe mul
   (mul-r/m16 cx)
    ; revert mask in result
   (shl-r/m16-imm8 cx (bv 15 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m16-imm8 r12w (bv 15 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m16-r16 ax cx)
   (sub-r/m16-r16 ax r12w)
   (mov-r/m8-r8 al dl)
    ; rax contains result

   ; restore rcx
   (mov-r/m16-r16 cx r10w)
   (shr-r/m16-imm8 cx (bv 8 8))
   (mov-r/m16-imm16 dx (bv 0 16))
  ))

(define spec-mul16-p4
  (list
   (shr-r/m16-imm8 ax (bv 8 8))
   (shr-r/m16-imm8 cx (bv 8 8))
   (mul-r/m16 cx)))

(define attempt-mul16-p5
  (list
   (mov-r/m16-r16 r10w cx)

    ; recombine terms
    ; put 4th term (upper 16 bits) in dl
   (mov-r/m16-r16 dx ax)
    ; put 1st term (lower 16 bits) in al
   (mov-r/m16-r16 ax r13w)
    ; safely add 3rd term (middle 16 bits)
      (mov-r/m16-r16 cx r15w) ; use POP in real transform
      (mov-r/m16-imm16 r11w (bv 0 16))
      (shld-r/m16-r16-imm8 r11w cx (bv 8 8))
      ; now r11w has the upper 8 bits of 3rd term in its lower half
      (shl-r/m16-imm8 cx (bv 8 8))
      (shr-r/m16-imm8 cx (bv 8 8))
      (mov-r/m16-imm16 r13w (bv (expt 2 7) 16))
      (sub-r/m16-r16 cx r13w)
      (sub-r/m16-r16 cx r13w)
      (shl-r/m16-imm8 cx (bv 8 8))
      ; now rcx has the lower 32 bits of 3rd term in its upper half
      ; mask lower bits in cx, ax so we can add safely
      (mov-r/m8-r8 r12b al)
      (mov-r/m8-imm8 al (bv 1 8))
      (mov-r/m8-imm8 cl (bv 1 8))
      ; perform addition
      (add-r/m16-r16 ax cx)
      (adc-r/m16-r16 dx r11w)
      (mov-r/m8-r8 al r12b)
    ; safely add 2nd term (middle 64 bits)
      (mov-r/m16-r16 cx r14w) ; use POP in real transform
      (mov-r/m16-imm16 r11w (bv 0 16))
      (shld-r/m16-r16-imm8 r11w cx (bv 8 8))
      ; now r11 has the upper 32 bits of 3rd term in its lower half
      (shl-r/m16-imm8 cx (bv 8 8))
      (shr-r/m16-imm8 cx (bv 8 8))
      (mov-r/m16-imm16 r13w (bv (expt 2 7) 16))
      (sub-r/m16-r16 cx r13w)
      (sub-r/m16-r16 cx r13w)
      (shl-r/m16-imm8 cx (bv 8 8))
      ; now rcx has the lower 32 bits of 3rd term in its upper half
      ; mask lower bits in rcx, rax so we can add safely
      (mov-r/m8-r8 r12b al)
      (mov-r/m8-imm8 al (bv 1 8))
      (mov-r/m8-imm8 cl (bv 1 8))
      ; perform addition
      (add-r/m16-r16 ax cx)
      (adc-r/m16-r16 dx r11w)
      (mov-r/m8-r8 al r12b)
    ; product should be correct now

   (mov-r/m16-r16 cx r10w)
  ))

(define spec-mul16-p5
  (list
   (mov-r/m16-r16 dx ax)
   (mov-r/m16-r16 ax r13w)

   (mov-r/m16-r16 r11w r15w)
   (shr-r/m16-imm8 r11w (bv 8 8))
   (shl-r/m16-imm8 r15w (bv 8 8))
   (add-r/m16-r16 ax r15w)
   (adc-r/m16-r16 dx r11w)

   (mov-r/m16-imm16 r11w (bv 0 16))
   (shld-r/m16-r16-imm8 r11w r14w (bv 8 8))
   (shl-r/m16-imm8 r14w (bv 8 8))
   (add-r/m16-r16 ax r14w)
   (adc-r/m16-r16 dx r11w)
  ))


(define attempt-mul64
  (list
   ; save arguments (rax and rcx)
   (mov-r/m64-r64 r10 rcx)
   (mov-r/m64-r64 r11 rax)

   ; calculate first term: x[31:] * y[31:]
    ; prepare arguments
   (mov-r64-imm64 rdx (bv (expt 2 63) 64))
   (mov-r/m32-r32 ecx r10d)
   (sub-r/m64-r64 rcx rdx)

   (mov-r/m32-r32 eax r11d) 
   (sub-r/m64-r64 rax rdx)
   (mov-r/m64-r64 r12 rax)
    ; perform safe mul
   (mul-r/m64 rcx)
    ; revert mask in result
   (shl-r/m64-imm8 rcx (bv 63 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m64-imm8 r12 (bv 63 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m64-r64 rax rcx)
   (sub-r/m64-r64 rax r12)
   (mov-r/m8-r8 al dl)
    ; save result
   (mov-r/m64-r64 r13 rax)

   ; calculate 2nd term: x[31:] * y[:32]
    ; prepare arguments
   (mov-r64-imm64 rdx (bv (expt 2 63) 64))
   (mov-r/m32-r32 ecx r10d)
   (sub-r/m64-r64 rcx rdx)

   (mov-r/m64-r64 rax r11)
   (mov-r/m8-imm8 al (bv 1 8))
   (shr-r/m64-imm8 rax (bv 32 8))
   (sub-r/m64-r64 rax rdx)
   (mov-r/m64-r64 r12 rax)
    ; perform safe mul
   (mul-r/m64 rcx)
    ; revert mask in result
   (shl-r/m64-imm8 rcx (bv 63 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m64-imm8 r12 (bv 63 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m64-r64 rax rcx)
   (sub-r/m64-r64 rax r12)
   (mov-r/m8-r8 al dl)
    ; save result
    ; use PUSH in actual transform
   (mov-r/m64-r64 r14 rax)

   ; calculate 3rd term: x[:32] * y[31:]
    ; prepare arguments
   (mov-r64-imm64 rdx (bv (expt 2 63) 64))
   (mov-r/m64-r64 rcx r10)
   (mov-r/m8-imm8 cl (bv 1 8))
   (shr-r/m64-imm8 rcx (bv 32 8))
   (sub-r/m64-r64 rcx rdx)

   (mov-r/m32-r32 eax r11d)
   (sub-r/m64-r64 rax rdx)
   (mov-r/m64-r64 r12 rax)
    ; perform safe mul
   (mul-r/m64 rcx)
    ; revert mask in result
   (shl-r/m64-imm8 rcx (bv 63 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m64-imm8 r12 (bv 63 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m64-r64 rax rcx)
   (sub-r/m64-r64 rax r12)
   (mov-r/m8-r8 al dl)
    ; save result
    ; use PUSH in actual transform
   (mov-r/m64-r64 r15 rax)

   ; calculate 4th term: x[:32] * y[:32]
    ; prepare arguments
   (mov-r64-imm64 rdx (bv (expt 2 63) 64))
   (mov-r/m64-r64 rcx r10)
   (mov-r/m8-imm8 cl (bv 1 8))
   (shr-r/m64-imm8 rcx (bv 32 8))
   (sub-r/m64-r64 rcx rdx)

   (mov-r/m64-r64 rax r11)
   (mov-r/m8-imm8 al (bv 1 8))
   (shr-r/m64-imm8 rax (bv 32 8))
   (sub-r/m64-r64 rax rdx)
   (mov-r/m64-r64 r12 rax)
    ; perform safe mul
   (mul-r/m64 rcx)
    ; revert mask in result
   (shl-r/m64-imm8 rcx (bv 63 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m64-imm8 r12 (bv 63 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m64-r64 rax rcx)
   (sub-r/m64-r64 rax r12)
   (mov-r/m8-r8 al dl)
    ; rax contains result

   ; recombine terms
    ; put 4th term (upper 64 bits) in rdx
   (mov-r/m64-r64 rdx rax)
    ; put 1st term (lower 64 bits) in rax
   (mov-r/m64-r64 rax r13)
    ; safely add 3rd term (middle 64 bits)
      (mov-r/m64-r64 rcx r15) ; use POP in real transform
      (mov-r/m64-imm32 r11 (bv 0 32))
      (shld-r/m64-r64-imm8 r11 rcx (bv 32 8))
      ; now r11 has the upper 32 bits of 3rd term in its lower half
      (mov-r/m32-r32 ecx ecx)
      (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
      (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
      (shl-r/m64-imm8 rcx (bv 32 8))
      ; now rcx has the lower 32 bits of 3rd term in its upper half
      ; mask lower bits in rcx, rax so we can add safely
      (mov-r/m8-r8 r12b al)
      (mov-r/m8-imm8 al (bv 1 8))
      (mov-r/m8-imm8 cl (bv 1 8))
      ; perform addition
      (add-r/m64-r64 rax rcx)
      (adc-r/m64-r64 rdx r11)
      (mov-r/m8-r8 al r12b)
    ; safely add 2nd term (middle 64 bits)
      (mov-r/m64-r64 rcx r14) ; use POP in real transform
      (mov-r/m64-imm32 r11 (bv 0 32))
      (shld-r/m64-r64-imm8 r11 rcx (bv 32 8))
      ; now r11 has the upper 32 bits of 3rd term in its lower half
      (mov-r/m32-r32 ecx ecx)
      (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
      (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
      (shl-r/m64-imm8 rcx (bv 32 8))
      ; now rcx has the lower 32 bits of 3rd term in its upper half
      ; mask lower bits in rcx, rax so we can add safely
      (mov-r/m8-r8 r12b al)
      (mov-r/m8-imm8 al (bv 1 8))
      (mov-r/m8-imm8 cl (bv 1 8))
      ; perform addition
      (add-r/m64-r64 rax rcx)
      (adc-r/m64-r64 rdx r11)
      (mov-r/m8-r8 al r12b)
    ; product should be correct now

   ; restore rcx
   (mov-r/m64-r64 rcx r10)
  ))

(define spec-mul64
  (list
   (mul-r/m64 rcx)))

(define attempt-mul64-p1
  (list
   ; save arguments (rax and rcx)
   (mov-r/m64-r64 r10 rcx)
   (mov-r/m64-r64 r11 rax)

   ; calculate first term: x[31:] * y[31:]
    ; prepare arguments
   (mov-r64-imm64 rdx (bv (expt 2 63) 64))
   (mov-r/m32-r32 ecx r10d)
   (sub-r/m64-r64 rcx rdx)

   (mov-r/m32-r32 eax r11d) 
   (sub-r/m64-r64 rax rdx)
   (mov-r/m64-r64 r12 rax)
    ; perform safe mul
   (mul-r/m64 rcx)
    ; revert mask in result
   (shl-r/m64-imm8 rcx (bv 63 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m64-imm8 r12 (bv 63 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m64-r64 rax rcx)
   (sub-r/m64-r64 rax r12)
   (mov-r/m8-r8 al dl)

   ; restore rcx
   (mov-r/m64-r64 rcx r10)
   (mov-r/m32-r32 ecx ecx)
   (mov-r/m32-imm32 edx (bv 0 32))
  ))

(define spec-mul64-p1
  (list
   (mov-r/m32-r32 eax eax)
   (mov-r/m32-r32 ecx ecx)
   (mul-r/m64 rcx)))

(define attempt-mul64-p2
  (list
   ; save arguments (rax and rcx)
   (mov-r/m64-r64 r10 rcx)
   (mov-r/m64-r64 r11 rax)

   ; calculate 2nd term: x[31:] * y[:32]
    ; prepare arguments
   (mov-r64-imm64 rdx (bv (expt 2 63) 64))
   (mov-r/m32-r32 ecx r10d)
   (sub-r/m64-r64 rcx rdx)

   (mov-r/m64-r64 rax r11)
   (mov-r/m8-imm8 al (bv 1 8))
   (shr-r/m64-imm8 rax (bv 32 8))
   (sub-r/m64-r64 rax rdx)
   (mov-r/m64-r64 r12 rax)
    ; perform safe mul
   (mul-r/m64 rcx)
    ; revert mask in result
   (shl-r/m64-imm8 rcx (bv 63 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m64-imm8 r12 (bv 63 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m64-r64 rax rcx)
   (sub-r/m64-r64 rax r12)
   (mov-r/m8-r8 al dl)

   ; restore rcx
   (mov-r/m64-r64 rcx r10)
   (mov-r/m32-r32 ecx ecx)
   (mov-r/m32-imm32 edx (bv 0 32))
  ))

(define spec-mul64-p2
  (list
   (shr-r/m64-imm8 rax (bv 32 8))
   (mov-r/m32-r32 ecx ecx)
   (mul-r/m64 rcx)))

(define attempt-mul64-p3
  (list
   ; save arguments (rax and rcx)
   (mov-r/m64-r64 r10 rcx)
   (mov-r/m64-r64 r11 rax)

   ; calculate 3rd term: x[:32] * y[31:]
    ; prepare arguments
   (mov-r64-imm64 rdx (bv (expt 2 63) 64))
   (mov-r/m64-r64 rcx r10)
   (mov-r/m8-imm8 cl (bv 1 8))
   (shr-r/m64-imm8 rcx (bv 32 8))
   (sub-r/m64-r64 rcx rdx)

   (mov-r/m32-r32 eax r11d)
   (sub-r/m64-r64 rax rdx)
   (mov-r/m64-r64 r12 rax)
    ; perform safe mul
   (mul-r/m64 rcx)
    ; revert mask in result
   (shl-r/m64-imm8 rcx (bv 63 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m64-imm8 r12 (bv 63 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m64-r64 rax rcx)
   (sub-r/m64-r64 rax r12)
   (mov-r/m8-r8 al dl)

   ; restore rcx
   (mov-r/m64-r64 rcx r10)
   (shr-r/m64-imm8 rcx (bv 32 8))
   (mov-r/m32-imm32 edx (bv 0 32))
  ))

(define spec-mul64-p3
  (list
   (mov-r/m32-r32 eax eax)
   (shr-r/m64-imm8 rcx (bv 32 8))
   (mul-r/m64 rcx)))

(define attempt-mul64-p4
  (list
   ; save arguments (rax and rcx)
   (mov-r/m64-r64 r10 rcx)
   (mov-r/m64-r64 r11 rax)

   ; calculate 4th term: x[:32] * y[:32]
    ; prepare arguments
   (mov-r64-imm64 rdx (bv (expt 2 63) 64))
   (mov-r/m64-r64 rcx r10)
   (mov-r/m8-imm8 cl (bv 1 8))
   (shr-r/m64-imm8 rcx (bv 32 8))
   (sub-r/m64-r64 rcx rdx)

   (mov-r/m64-r64 rax r11)
   (mov-r/m8-imm8 al (bv 1 8))
   (shr-r/m64-imm8 rax (bv 32 8))
   (sub-r/m64-r64 rax rdx)
   (mov-r/m64-r64 r12 rax)
    ; perform safe mul
   (mul-r/m64 rcx)
    ; revert mask in result
   (shl-r/m64-imm8 rcx (bv 63 8))
   (mov-r/m8-imm8 cl (bv 1 8))
   (shl-r/m64-imm8 r12 (bv 63 8))
   (mov-r/m8-imm8 r12b (bv 1 8))
   (mov-r/m8-r8 dl al)
   (mov-r/m8-imm8 al (bv 2 8))
   (sub-r/m64-r64 rax rcx)
   (sub-r/m64-r64 rax r12)
   (mov-r/m8-r8 al dl)
    ; rax contains result

   ; restore rcx
   (mov-r/m64-r64 rcx r10)
   (shr-r/m64-imm8 rcx (bv 32 8))
   (mov-r/m32-imm32 edx (bv 0 32))
  ))

(define spec-mul64-p4
  (list
   (shr-r/m64-imm8 rax (bv 32 8))
   (shr-r/m64-imm8 rcx (bv 32 8))
   (mul-r/m64 rcx)))

(define attempt-mul64-p5
  (list
   (mov-r/m64-r64 r10 rcx)

    ; recombine terms
    ; put 4th term (upper 64 bits) in rdx
   (mov-r/m64-r64 rdx rax)
    ; put 1st term (lower 64 bits) in rax
   (mov-r/m64-r64 rax r13)
    ; safely add 3rd term (middle 64 bits)
      (mov-r/m64-r64 rcx r15) ; use POP in real transform
      (mov-r/m64-imm32 r11 (bv 0 32))
      (shld-r/m64-r64-imm8 r11 rcx (bv 32 8))
      ; now r11 has the upper 32 bits of 3rd term in its lower half
      (mov-r/m32-r32 ecx ecx)
      (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
      (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
      (shl-r/m64-imm8 rcx (bv 32 8))
      ; now rcx has the lower 32 bits of 3rd term in its upper half
      ; mask lower bits in rcx, rax so we can add safely
      (mov-r/m8-r8 r12b al)
      (mov-r/m8-imm8 al (bv 1 8))
      (mov-r/m8-imm8 cl (bv 1 8))
      ; perform addition
      (add-r/m64-r64 rax rcx)
      (adc-r/m64-r64 rdx r11)
      (mov-r/m8-r8 al r12b)
    ; safely add 2nd term (middle 64 bits)
      (mov-r/m64-r64 rcx r14) ; use POP in real transform
      (mov-r/m64-imm32 r11 (bv 0 32))
      (shld-r/m64-r64-imm8 r11 rcx (bv 32 8))
      ; now r11 has the upper 32 bits of 3rd term in its lower half
      (mov-r/m32-r32 ecx ecx)
      (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
      (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
      (shl-r/m64-imm8 rcx (bv 32 8))
      ; now rcx has the lower 32 bits of 3rd term in its upper half
      ; mask lower bits in rcx, rax so we can add safely
      (mov-r/m8-r8 r12b al)
      (mov-r/m8-imm8 al (bv 1 8))
      (mov-r/m8-imm8 cl (bv 1 8))
      ; perform addition
      (add-r/m64-r64 rax rcx)
      (adc-r/m64-r64 rdx r11)
      (mov-r/m8-r8 al r12b)
    ; product should be correct now

   (mov-r/m64-r64 rcx r10)
  ))

(define spec-mul64-p5
  (list
   (mov-r/m64-r64 rdx rax)
   (mov-r/m64-r64 rax r13)

   (mov-r/m64-r64 r11 r15)
   (shr-r/m64-imm8 r11 (bv 32 8))
   (shl-r/m64-imm8 r15 (bv 32 8))
   (add-r/m64-r64 rax r15)
   (adc-r/m64-r64 rdx r11)

   (mov-r/m64-r64 r11 r14)
   (shr-r/m64-imm8 r11 (bv 32 8))
   (shl-r/m64-imm8 r14 (bv 32 8))
   (add-r/m64-r64 rax r14)
   (adc-r/m64-r64 rdx r11)
  ))
