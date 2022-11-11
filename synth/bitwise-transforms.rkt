#lang rosette

(require 
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

(provide (all-defined-out))

; ---------- Bitwise AND ----------

(define attempt-and32 ; and32
  (list
   (mov-r/m64-r64 r10 rax) ; save rax
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m32-r32 eax eax) ; zero top 32 bits of eax
   (mov-r64-imm64 r11 (bv (expt 2 33) 64))
   (sub-r/m64-r64 rax r11)
   (sub-r/m64-r64 rcx r11)
   (and-r/m64-r64 rcx rax) ; perform and
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m64-r64 rax r10))) ; restore rax

(define spec-and32    ; and32
  (list
   (and-r/m32-r32 ecx eax)))

(define attempt-and64
  (list
    (mov-r/m64-imm32 r10 (bv (expt 2 16) 32))
    (mov-r/m16-r16 r10w cx) ; split lower 16 bits of rcx into r10
    (mov-r/m16-imm16 cx (bv 1 16))  ; mask out lower 16 bits of rcx
    (mov-r/m64-imm32 r11 (bv (expt 2 16) 32))
    (mov-r/m16-r16 r11w ax) ; split lower 16 bits of rax into r11
    (mov-r/m16-imm16 ax (bv 1 16))  ; mask out lower 16 bits of rax
    (and-r/m64-r64 rcx rax) ; AND upper 48 bits
    (and-r/m64-r64 r10 r11) ; AND lower 16 bits
    (mov-r/m16-r16 cx r10w) ; recombine lower 16 bits of result
    (mov-r/m16-r16 ax r11w) ; restore rax
   ))

(define spec-and64
  (list
   (and-r/m64-r64 rcx rax)))

; ---------- Bitwise OR ----------

(define attempt-or32
  (list
   (mov-r/m64-r64 r10 rax) ; save rax
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m32-r32 eax eax) ; zero top 32 bits of eax
   (mov-r64-imm64 r11 (bv (expt 2 33) 64))
   (sub-r/m64-r64 rax r11)
   (sub-r/m64-r64 rcx r11)
   (or-r/m64-r64 rcx rax) ; perform and
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m64-r64 rax r10))) ; restore rax

(define spec-or32
  (list
   (or-r/m32-r32 ecx eax)))

(define attempt-or64
  (list
    (mov-r/m64-imm32 r10 (bv (expt 2 16) 32))
    (mov-r/m16-r16 r10w cx) ; split lower 16 bits of rcx into r10
    (mov-r/m16-imm16 cx (bv 1 16))  ; mask out lower 16 bits of rcx
    (mov-r/m64-imm32 r11 (bv (expt 2 16) 32))
    (mov-r/m16-r16 r11w ax) ; split lower 16 bits of rax into r11
    (mov-r/m16-imm16 ax (bv 1 16))  ; mask out lower 16 bits of rax
    (or-r/m64-r64 rcx rax) ; AND upper 48 bits
    (or-r/m64-r64 r10 r11) ; AND lower 16 bits
    (mov-r/m16-r16 cx r10w) ; recombine lower 16 bits of result
    (mov-r/m16-r16 ax r11w) ; restore rax
  ))

(define spec-or64
  (list
   (or-r/m64-r64 rcx rax)))

; ---------- Bitwise XOR ----------

(define attempt-xor32
  (list
   (mov-r/m64-r64 r10 rax) ; save rax
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m32-r32 eax eax) ; zero top 32 bits of eax
   (mov-r64-imm64 r11 (bv (expt 2 33) 64))
   (sub-r/m64-r64 rax r11)
   (sub-r/m64-r64 rcx r11)
   (xor-r/m64-r64 rcx rax) ; perform and
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m64-r64 rax r10))) ; restore rax

(define spec-xor32
  (list
   (xor-r/m32-r32 ecx eax)))

(define attempt-xor64
  (list
    (mov-r/m64-imm32 r10 (bv (expt 2 16) 32))
    (mov-r/m16-r16 r10w cx) ; split lower 16 bits of rcx into r10
    (mov-r/m16-imm16 cx (bv 1 16))  ; mask out lower 16 bits of rcx
    (mov-r/m64-imm32 r11 (bv (expt 2 16) 32))
    (mov-r/m16-r16 r11w ax) ; split lower 16 bits of rax into r11
    (mov-r/m16-imm16 ax (bv 1 16))  ; mask out lower 16 bits of rax
    (xor-r/m64-r64 rcx rax) ; AND upper 48 bits
    (xor-r/m64-r64 r10 r11) ; AND lower 16 bits
    (mov-r/m16-r16 cx r10w) ; recombine lower 16 bits of result
    (mov-r/m16-r16 ax r11w) ; restore rax
  ))

(define spec-xor64
  (list
   (xor-r/m64-r64 rcx rax)))

; ---------- Bitwise LSHIFT ----------
; TODO account for shifting by 0

(define attempt-lshift64
  (list
   ; split up rax
   (mov-r64-imm64 r10 (bv (expt 2 63) 64))
   (mov-r/m64-r64 r11 r10)
   (mov-r/m16-r16 r11w ax) ; r11 contains lower 16 bits
   (mov-r/m16-imm16 ax (bv 1 16)) ; rax contains upper 48 bits
   ; perform shifts
   (shl-r/m64-cl r11)
   (shl-r/m64-cl rax)
   ; mutate r11 to make adding safe
   (rol-r/m64-cl r10)
   (sub-r/m64-r64 r11 r10)
   ; recombine
   (add-r/m64-r64 rax r11)
   (sub-r/m64-r64 rax r10)
  ))

(define spec-lshift64
  (list
   (shl-r/m64-cl rax)))

(define attempt-lshift32
  (list
   (mov-r/m32-r32 eax eax)
   (mov-r64-imm64 r11 (bv (expt 2 32) 64))
   (sub-r/m64-r64 rax r11)
   ; TODO mask cl to 5 LSB
   (shl-r/m64-cl rax)
   (mov-r/m32-r32 eax eax)
  ))

(define spec-lshift32
  (list
   (shl-r/m32-cl eax)))

; ---------- Bitwise RSHIFT ----------
; TODO account for shifting by 0

(define attempt-rshift64
  (list
   ; split up rax
   (mov-r64-imm64 r10 (bv (expt 2 63) 64))
   (mov-r/m64-r64 r11 r10)
   (mov-r/m16-r16 r11w ax) ; r11 contains lower 16 bits
   (mov-r/m16-imm16 ax (bv 1 16)) ; rax contains upper 48 bits
   ; perform shifts
   (shr-r/m64-cl r11)
   (shr-r/m64-cl rax)
   ; mutate rax to make adding safe
   (shr-r/m64-cl r10)
   (shl-r/m64-1 r10)
   (sub-r/m64-r64 rax r10)
   ; recombine
   (add-r/m64-r64 rax r11)
   (shr-r/m64-1 r10)
   (add-r/m64-r64 rax r10)
  ))

(define spec-rshift64
  (list
   (shr-r/m64-cl rax)))

(define attempt-rshift32
  (list
   (mov-r/m32-r32 eax eax)
   (mov-r64-imm64 r11 (bv (expt 2 63) 64))
   (sub-r/m64-r64 rax r11)
   ; TODO mask cl to 5 LSB
   (shr-r/m64-cl rax)
   (mov-r/m32-r32 eax eax)
  ))

(define spec-rshift32
  (list
   (shr-r/m32-cl eax)))

; ---------- Bitwise ARSHIFT ----------
; TODO account for shifting by 0

(define attempt-arshift64
  (list
   ; split up rax
   (mov-r64-imm64 r10 (bv (expt 2 63) 64))
   (mov-r/m64-r64 r11 r10)
   (mov-r/m16-r16 r11w ax) ; r11 contains lower 16 bits
   (mov-r/m16-imm16 ax (bv 1 16)) ; rax contains upper 48 bits
   ; perform shifts
   (sar-r/m64-cl r11)
   (sar-r/m64-cl rax)
   ; mutate rax to make adding safe
   (shr-r/m64-cl r10)
   (shl-r/m64-1 r10)
   (sub-r/m64-r64 rax r10)
   ; recombine
   (add-r/m64-r64 rax r11)
   (add-r/m64-r64 rax r10)
   (shr-r/m64-1 r10)
   (add-r/m64-r64 rax r10)
  ))

(define spec-arshift64
  (list
   (sar-r/m64-cl rax)))

(define attempt-arshift32
  (list
   (mov-r/m32-r32 eax eax)
   (mov-r64-imm64 r11 (bv (expt 2 63) 64))
   (sub-r/m64-r64 rax r11)
   ; TODO mask cl to 5 LSB
   (rol-r/m64-imm8 rax (bv 32 8))
   (sar-r/m64-cl rax)
   (ror-r/m64-imm8 rax (bv 32 8))
   (mov-r/m32-r32 eax eax)
  ))

(define spec-arshift32
  (list
   (sar-r/m32-cl eax)))
