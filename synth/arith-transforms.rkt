#lang rosette

(require 
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

(provide (all-defined-out))

(define attempt-sub32
  (list
   (sub-r/m32-imm32 eax (bv (expt 2 31) 32))
   (sub-r/m32-imm32 eax (bv (expt 2 31) 32))
   (setz dl)
   (sub-r/m64-imm32 rdx (bv (sub1 (expt 2 32)) 32))
   (sub-r/m64-imm32 rdx (bv 1 32))
   (sub-r/m64-r64 rax rdx)
   (sub-r/m64-r64 rcx rax)
   (sub-r/m64-r64 rcx rdx)))

(define spec-sub32
  (list
   (sub-r/m32-r32 ecx eax)))

(define attempt-add32
  (list
   (mov-r64-imm64 rdx (bv (expt 2 32) 64))
   (sub-r/m64-r64 rax rdx)
   (sub-r/m64-r64 rcx rdx)
   (add-r/m64-r64 rcx rax)
   (mov-r64-imm64 rdx (bv (- (expt 2 33)) 64))
   (sub-r/m64-r64 rcx rdx)))

(define spec-add32
  (list
   (add-r/m32-r32 ecx eax)))
