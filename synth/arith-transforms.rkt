#lang rosette

(require 
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

(provide (all-defined-out))

; ---------------- SUB ----------------

; Dest rcx, src rax
(define regs-sub
  (list rax rcx))

(define attempt-sub8
  (list
   (mov-r/m64-r64 r11 rax) ; save rax
   (mov-r/m64-r64 r10 rcx) ; use temp register in place of rcx
   (mov-r/m32-r32 eax eax) ; force rax to be nonzero
   (sub-r/m64-imm32 rax (bv (expt 2 31) 32))
   (sub-r/m64-imm32 rax (bv (expt 2 31) 32))
   (sub-r/m64-r64 r10 rax) ; perform sub
   (mov-r/m8-r8 cl r10b) ; move difference to lower bits of rcx
   (mov-r/m64-r64 rax r11))) ; restore rax

(define spec-sub8
  (list
   (sub-r/m8-r8 cl al)))

(define attempt-sub16
  (list
   (mov-r/m64-r64 r11 rax) ; save rax
   (mov-r/m64-r64 r10 rcx) ; use temp register in place of rcx
   (mov-r/m32-r32 eax eax) ; force rax to be nonzero
   (sub-r/m64-imm32 rax (bv (expt 2 31) 32))
   (sub-r/m64-imm32 rax (bv (expt 2 31) 32))
   (sub-r/m64-r64 r10 rax) ; perform sub
   (mov-r/m16-r16 cx r10w) ; move difference to lower bits of rcx
   (mov-r/m64-r64 rax r11))) ; restore rax

(define spec-sub16
  (list
   (sub-r/m16-r16 cx ax)))

(define attempt-sub32
  (list
   (mov-r/m64-r64 r11 rax) ; save rax
   (mov-r/m32-r32 eax eax) ; zero top 32 bits of eax
   (sub-r/m64-imm32 rax (bv (expt 2 31) 32))
   (sub-r/m64-imm32 rax (bv (expt 2 31) 32))
   (sub-r/m64-r64 rcx rax) ; perform sub
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m64-r64 rax r11))) ; restore rax

(define spec-sub32
  (list
   (sub-r/m32-r32 ecx eax)))

(define attempt-sub64
  (list
   ; split operands 48/16
   (mov-r64-imm64 r10 (bv (expt 2 48) 64))
   (mov-r/m16-r16 r10w cx) ; split lower 16 bits of rcx into r10
   (mov-r/m16-imm16 cx (bv 1 16))  ; mask out lower 16 bits of rcx
   (mov-r64-imm64 r11 (bv (expt 2 48) 64))
   (mov-r/m16-r16 r11w ax) ; split lower 16 bits of rax into r11
   (mov-r/m16-imm16 ax (bv 1 16))  ; mask out lower 16 bits of rax
   ; rotate into position
   (rol-r/m64-imm8 r10 (bv 16 8))
   (rol-r/m64-imm8 r11 (bv 16 8))
   (ror-r/m64-imm8 rcx (bv 16 8))
   (ror-r/m64-imm8 rax (bv 16 8))
   ; sub parts
   (sub-r/m32-r32 r10d r11d)
   (sbb-r/m64-r64 rcx rax)
   ; rotate back
   (ror-r/m64-imm8 r10 (bv 16 8))
   (ror-r/m64-imm8 r11 (bv 16 8))
   (rol-r/m64-imm8 rcx (bv 16 8))
   (rol-r/m64-imm8 rax (bv 16 8))
   ; recombine and restore rax
   (mov-r/m16-r16 cx r10w)
   (mov-r/m16-r16 ax r11w)
  ))

(define spec-sub64
  (list
   (sub-r/m64-r64 rcx rax)))

; ---------------- ADD ----------------

; Dest rcx, src rax
(define regs-add
  (list rax rcx))

; Check CF for add64 only
(define flags-add64
  (list 'CF 'ZF))

(define attempt-add8
  (list
   (mov-r/m64-imm32 r10 (bv (expt 2 31) 32))
   (mov-r/m8-r8 r10b cl) ; split lower 8 bits of rcx into r10
   (mov-r/m64-imm32 r11 (bv (expt 2 31) 32))
   (mov-r/m8-r8 r11b al) ; split lower 8 bits of rax into r11
   (add-r/m64-r64 r10 r11) ; perform addition
   (mov-r/m8-r8 cl r10b) ; recombine result
   (mov-r/m8-r8 al r11b) ; restore rax
  ))

(define spec-add8
  (list
   (add-r/m8-r8 cl al)))

(define attempt-add16
  (list
   (mov-r/m64-imm32 r10 (bv (expt 2 31) 32))
   (mov-r/m16-r16 r10w cx) ; split lower 16 bits of rcx into r10
   (mov-r/m64-imm32 r11 (bv (expt 2 31) 32))
   (mov-r/m16-r16 r11w ax) ; split lower 16 bits of rax into r11
   (add-r/m64-r64 r10 r11) ; perform addition
   (mov-r/m16-r16 cx r10w) ; recombine result
   (mov-r/m16-r16 ax r11w) ; restore rax
  ))

(define spec-add16
  (list
   (add-r/m16-r16 cx ax)))
  
(define attempt-add32
  (list
   (mov-r/m64-r64 r11 rax) ; save rax
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of rcx
   (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
   (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
   (mov-r/m32-r32 eax eax) ; zero top 32 bits of rax
   (sub-r/m64-imm32 rax (bv (expt 2 31) 32))
   (sub-r/m64-imm32 rax (bv (expt 2 31) 32))
   (add-r/m64-r64 rcx rax) ; perform add
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m64-r64 rax r11))) ; restore rax

(define spec-add32
  (list
   (add-r/m32-r32 ecx eax)))
  
(define attempt-add64
  (list
   ; split operands 48/16
   (mov-r64-imm64 r10 (bv (expt 2 48) 64))
   (mov-r/m16-r16 r10w cx) ; split lower 16 bits of rcx into r10
   (mov-r/m16-imm16 cx (bv 1 16))  ; mask out lower 16 bits of rcx
   (mov-r64-imm64 r11 (bv (expt 2 48) 64))
   (mov-r/m16-r16 r11w ax) ; split lower 16 bits of rax into r11
   (mov-r/m16-imm16 ax (bv 1 16))  ; mask out lower 16 bits of rax
   ; rotate into position
   (rol-r/m64-imm8 r10 (bv 16 8))
   (rol-r/m64-imm8 r11 (bv 16 8))
   (ror-r/m64-imm8 rcx (bv 16 8))
   (ror-r/m64-imm8 rax (bv 16 8))
   ; add parts
   (add-r/m32-r32 r10d r11d)
   (adc-r/m64-r64 rcx rax)
   ; rotate back
   (ror-r/m64-imm8 r11 (bv 16 8))
   (rol-r/m64-imm8 rax (bv 16 8))
   (ror-r/m64-imm8 r10 (bv 16 8))
   (rcl-r/m64-imm8 rcx (bv 16 8))
   ; recombine
   (mov-r/m16-r16 cx r10w)
   ; set flags
   (setc al)
   (cmp-r/m64-imm8 rcx (bv 0 8))
   (bt-r/m64-imm8 rax (bv 0 8))
   ; restore rax
   (mov-r/m16-r16 ax r11w)
  ))

(define spec-add64
  (list
   (add-r/m64-r64 rcx rax)))

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
