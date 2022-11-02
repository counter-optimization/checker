#lang rosette

(require 
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

(provide (all-defined-out))

(define attempt-and32 ; and32
  (list
   (mov-r64-imm64 rdx (bv (expt 2 32) 64))
   (sub-r/m64-r64 rax rdx)
   (sub-r/m64-r64 rcx rdx)
   (and-r/m64-r64 rcx rax)))

(define spec-and32    ; and32
  (list
   (and-r/m32-r32 ecx eax)))
