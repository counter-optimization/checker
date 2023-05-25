#lang rosette

(require 
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

(provide (all-defined-out))

; ---------- Bitwise AND ----------

; Dest rcx, src rax
(define regs-and
  (list rax rcx))

(define attempt-and8
  (list
    (mov-r/m64-imm32 r10 (bv (expt 2 16) 32))
    (mov-r/m8-r8 r10b cl) ; split lower 8 bits of rcx into r10
    (mov-r/m64-imm32 r11 (bv (expt 2 16) 32))
    (mov-r/m8-r8 r11b al) ; split lower 8 bits of rax into r11
    (and-r/m64-r64 r10 r11) ; perform AND
    (mov-r/m8-r8 cl r10b) ; recombine result
    (mov-r/m8-r8 al r11b) ; restore rax
   ))

(define spec-and8
  (list
   (and-r/m8-r8 cl al)))

(define attempt-and16
  (list
    (mov-r/m64-imm32 r10 (bv (expt 2 16) 32))
    (mov-r/m16-r16 r10w cx) ; split lower 16 bits of rcx into r10
    (mov-r/m64-imm32 r11 (bv (expt 2 16) 32))
    (mov-r/m16-r16 r11w ax) ; split lower 16 bits of rax into r11
    (and-r/m64-r64 r10 r11) ; perform AND
    (mov-r/m16-r16 cx r10w) ; recombine result
    (mov-r/m16-r16 ax r11w) ; restore rax
   ))

(define spec-and16
  (list
   (and-r/m16-r16 cx ax)))

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

(define attempt-and32-cf-zf ; and32
  (list
   (mov-r/m64-r64 r10 rax) ; save rax
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m32-r32 eax eax) ; zero top 32 bits of eax
   (mov-r64-imm64 r11 (bv (expt 2 33) 64))
   (sub-r/m64-r64 rax r11)
   (sub-r/m64-r64 rcx r11)
   (and-r/m64-r64 rcx rax) ; perform and
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m64-r64 rax r10) ; restore rax
   (cmp-r/m32-imm8 ecx (bv 0 8)) ; set ZF
   (clc) ; clear CF
 ))

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

(define attempt-and64-cf-zf
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
    (cmp-r/m64-imm8 rcx (bv 0 8)) ; set ZF
    (clc) ; clear CF
   ))

(define spec-and64
  (list
   (and-r/m64-r64 rcx rax)))

; ---------- Bitwise OR ----------

; Dest rcx, src rax
(define regs-or
  (list rax rcx))

(define attempt-or8
  (list
    (mov-r/m64-imm32 r10 (bv (expt 2 16) 32))
    (mov-r/m8-r8 r10b cl) ; split lower 8 bits of rcx into r10
    (mov-r/m64-imm32 r11 (bv (expt 2 16) 32))
    (mov-r/m8-r8 r11b al) ; split lower 8 bits of rax into r11
    (or-r/m64-r64 r10 r11) ; perform OR
    (mov-r/m8-r8 cl r10b) ; recombine result
    (mov-r/m8-r8 al r11b) ; restore rax
   ))

(define spec-or8
  (list
   (or-r/m8-r8 cl al)))

(define attempt-or16
  (list
    (mov-r/m64-imm32 r10 (bv (expt 2 16) 32))
    (mov-r/m16-r16 r10w cx) ; split lower 16 bits of rcx into r10
    (mov-r/m64-imm32 r11 (bv (expt 2 16) 32))
    (mov-r/m16-r16 r11w ax) ; split lower 16 bits of rax into r11
    (or-r/m64-r64 r10 r11) ; perform OR
    (mov-r/m16-r16 cx r10w) ; recombine result
    (mov-r/m16-r16 ax r11w) ; restore rax
   ))

(define spec-or16
  (list
   (or-r/m16-r16 cx ax)))

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

; Dest rcx, src rax
(define regs-xor
  (list rax rcx))

(define attempt-xor8
  (list
    (mov-r/m64-imm32 r10 (bv (expt 2 16) 32))
    (mov-r/m8-r8 r10b cl) ; split lower 8 bits of rcx into r10
    (mov-r/m64-imm32 r11 (bv (expt 2 16) 32))
    (mov-r/m8-r8 r11b al) ; split lower 8 bits of rax into r11
    (xor-r/m64-r64 r10 r11) ; perform XOR
    (mov-r/m8-r8 cl r10b) ; recombine result
    (mov-r/m8-r8 al r11b) ; restore rax
   ))

(define spec-xor8
  (list
   (xor-r/m8-r8 cl al)))

(define attempt-xor16
  (list
    (mov-r/m64-imm32 r10 (bv (expt 2 16) 32))
    (mov-r/m16-r16 r10w cx) ; split lower 16 bits of rcx into r10
    (mov-r/m64-imm32 r11 (bv (expt 2 16) 32))
    (mov-r/m16-r16 r11w ax) ; split lower 16 bits of rax into r11
    (xor-r/m64-r64 r10 r11) ; perform XOR
    (mov-r/m16-r16 cx r10w) ; recombine result
    (mov-r/m16-r16 ax r11w) ; restore rax
   ))

(define spec-xor16
  (list
   (xor-r/m16-r16 cx ax)))

(define attempt-xor32
  (list
   (mov-r/m64-r64 r10 rax) ; save rax
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m32-r32 eax eax) ; zero top 32 bits of eax
   (mov-r64-imm64 r11 (bv (expt 2 33) 64))
   (sub-r/m64-r64 rax r11)
   (sub-r/m64-r64 rcx r11)
   (xor-r/m64-r64 rcx rax) ; perform xor
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m64-r64 rax r10))) ; restore rax

(define spec-xor32
  (list
   (xor-r/m32-r32 ecx eax)))

(define attempt-xor32-imm8
  (list
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r64-imm64 r11 (bv (expt 2 33) 64))
   (sub-r/m64-r64 rcx r11)
   (xor-r/m64-imm8 rcx (bv 25 8)) ; perform xor
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
  ))

(define spec-xor32-imm8
  (list
   (xor-r/m32-imm8 ecx (bv 25 8))))

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
