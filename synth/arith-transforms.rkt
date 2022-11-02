#lang rosette

(require 
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

(provide (all-defined-out))

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

(define attempt-add32
  (list
   (mov-r/m64-r64 r10 rax) ; save rax
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m32-r32 eax eax) ; zero top 32 bits of eax
   (mov-r64-imm64 r11 (bv (expt 2 32) 64))
   (sub-r/m64-r64 rax r11)
   (sub-r/m64-r64 rcx r11)
   (add-r/m64-r64 rcx rax) ; perform add
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m64-r64 rax r10))) ; restore rax

(define spec-add32
  (list
   (add-r/m32-r32 ecx eax)))
