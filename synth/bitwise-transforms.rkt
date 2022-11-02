#lang rosette

(require 
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

(provide (all-defined-out))

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
