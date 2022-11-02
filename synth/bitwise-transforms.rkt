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
   )) ; restore rax

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
