#lang rosette

(require 
  rosette/lib/synthax
  rosette/lib/match
  "../serval/serval/x86.rkt"
  (prefix-in comp-simp: "synth-comp-simp-defenses-macrod.rkt"))

(provide (all-defined-out))

; ---------- Bitwise SHL ----------

; Dest rax, src rcx
(define regs-shl
  (list rax rcx))

(define attempt-shl8-path-sensitivable
  (list
   (mov-r/m64-r64 r13 rcx) ; TODO: replace with push
   (mov-r64-imm64 r12 (bv (expt 2 31) 64))
   (mov-r/m8-r8 r12b cl)
   (mov-r/m32-r32 ecx r12d)
   (mov-r64-imm64 r12 (bv 0 64))
   (and-r/m64-imm8 rcx (bv #x1F 8))
   (cmp-r/m64-imm8 rcx (bv 0 8))
   (setz r12b)
   ; save rax, set rcx if shift amt = 0
   (cmovz-r64-r64 r10 rax)
   (cmovz-r64-r64 rcx r12)
      ; perform safe shift
      (mov-r64-imm64 r11 (bv (expt 2 63) 64))
      (mov-r/m8-r8 r11b al)
      (and-r/m8-imm8 cl (bv (- (expt 2 5) 1) 8))
      (shl-r/m64-cl r11)
      (mov-r/m8-r8 al r11b)
   ; restore rax if r12 =/ 0
   (cmp-r/m32-imm8 r12d (bv 0 8))
   (cmovne-r64-r64 rax r10)
   ; restore rcx
   (mov-r/m64-r64 rcx r13))) ; TODO: replace with pop

(define attempt-shl8
  (list
   ; save rcx
   (mov-r/m64-r64 r13 rcx) ; TODO: replace with push
   ; move and shift cl
   (mov-r/m32-imm32 r10d (bv (expt 2 31) 32))
   (mov-r/m8-r8 r10b cl)
   (shl-r/m64-imm8 r10 (bv (- 32 5) 8))
   ; test for masked cl bits = 0
   (mov-r64-imm64 r12 (bv 0 64))
   (cmp-r/m32-imm8 r10d (bv 0 8))
   (setz r12b)
   ; save rax, set rcx if shift amt = 0
   (cmovz-r64-r64 r10 rax)
   (cmovz-r64-r64 rcx r12)
      ; perform safe shift
      (mov-r64-imm64 r11 (bv (expt 2 63) 64))
      (mov-r/m8-r8 r11b al)
      (and-r/m8-imm8 cl (bv (- (expt 2 5) 1) 8))
      (shl-r/m64-cl r11)
      (mov-r/m8-r8 al r11b)
   ; restore rax if r12 =/ 0
   (cmp-r/m32-imm8 r12d (bv 0 8))
   (cmovne-r64-r64 rax r10)
   ; restore rcx
   (mov-r/m64-r64 rcx r13) ; TODO: replace with pop
  ))

(define spec-shl8
  (list
   (shl-r/m8-cl al)))

(define attempt-shl16
  (list
   ; save rcx
   (mov-r/m64-r64 r13 rcx) ; TODO: replace with push
   ; move and shift cl
   (mov-r/m32-imm32 r10d (bv (expt 2 31) 32))
   (mov-r/m8-r8 r10b cl)
   (shl-r/m64-imm8 r10 (bv (- 32 5) 8))
   ; test for masked cl bits = 0
   (mov-r64-imm64 r12 (bv 0 64))
   (cmp-r/m32-imm8 r10d (bv 0 8))
   (setz r12b)
   ; save rax, set rcx if shift amt = 0
   (cmovz-r64-r64 r10 rax)
   (cmovz-r64-r64 rcx r12)
      ; perform safe shift
      (mov-r64-imm64 r11 (bv (expt 2 63) 64))
      (mov-r/m16-r16 r11w ax)
      (and-r/m8-imm8 cl (bv (- (expt 2 5) 1) 8))
      (shl-r/m64-cl r11)
      (mov-r/m16-r16 ax r11w)
   ; restore rax if r12 =/ 0
   (cmp-r/m32-imm8 r12d (bv 0 8))
   (cmovne-r64-r64 rax r10)
   ; restore rcx
   (mov-r/m64-r64 rcx r13) ; TODO: replace with pop
  ))

(define spec-shl16
  (list
   (shl-r/m16-cl ax)))

(define attempt-shl32-path-sensitivable
  (list
  ; save rcx
   (mov-r/m64-r64 r12 rcx) ; TODO: replace with push
   (mov-r64-imm64 r13 (bv (expt 2 31) 64))
   (mov-r/m8-r8 r13b cl)
   (mov-r/m32-r32 ecx r13d)
   (mov-r64-imm64 r13 (bv 0 64))
   (mov-r64-imm64 r11 (bv 0 64))
   (and-r/m64-imm8 rcx (bv #x1F 8))
   (cmp-r/m64-imm8 rcx (bv 0 8))
   (setz r11b)
   ; save eax, set rcx if shift amt = 0
   (cmovz-r32-r32 r10d eax)
   (cmovz-r64-r64 rcx r11)
      ; perform safe shift
      (mov-r/m32-r32 eax eax)
      (sub-r/m64-imm32 rax (bv (expt 2 31) 64))
      (sub-r/m64-imm32 rax (bv (expt 2 31) 64))
      (and-r/m8-imm8 cl (bv (- (expt 2 5) 1) 8))
      (shl-r/m64-cl rax)
      (mov-r/m32-r32 eax eax)
   ; restore eax if r11 =/ 0
   (cmp-r/m32-imm8 r11d (bv 0 8))
   (cmovne-r32-r32 eax r10d)
   ; restore rcx
   (mov-r/m64-r64 rcx r12) ; TODO: replace with pop
  ))

(define attempt-shl32
  (list
   ; save rcx
   (mov-r/m64-r64 r12 rcx) ; TODO: replace with push
   ; move and shift cl
   (mov-r/m32-imm32 r10d (bv (expt 2 31) 32))
   (mov-r/m8-r8 r10b cl)
   (shl-r/m64-imm8 r10 (bv (- 32 5) 8))
   ; test for masked cl bits = 0
   (mov-r64-imm64 r11 (bv 0 64))
   (cmp-r/m32-imm8 r10d (bv 0 8))
   (setz r11b)
   ; save eax, set rcx if shift amt = 0
   (cmovz-r32-r32 r10d eax)
   (cmovz-r64-r64 rcx r11)
      ; perform safe shift
      (mov-r/m32-r32 eax eax)
      (sub-r/m64-imm32 rax (bv (expt 2 31) 64))
      (sub-r/m64-imm32 rax (bv (expt 2 31) 64))
      (and-r/m8-imm8 cl (bv (- (expt 2 5) 1) 8))
      (shl-r/m64-cl rax)
      (mov-r/m32-r32 eax eax)
   ; restore eax if r11 =/ 0
   (cmp-r/m32-imm8 r11d (bv 0 8))
   (cmovne-r32-r32 eax r10d)
   ; restore rcx
   (mov-r/m64-r64 rcx r12) ; TODO: replace with pop
  ))

(define spec-shl32
  (list
   (shl-r/m32-cl eax)))

(define attempt-shl64
  (list
   ; save rcx
   (mov-r/m64-r64 r14 rcx) ; TODO: replace with push
   ; move and shift cl
   (mov-r/m32-imm32 r13d (bv (expt 2 31) 32))
   (mov-r/m8-r8 r13b cl)
   (shl-r/m64-imm8 r13 (bv (- 32 6) 8))
   ; test for masked cl bits = 0
   (mov-r64-imm64 r12 (bv 0 64))
   (cmp-r/m32-imm8 r13d (bv 0 8))
   (setz r12b)
   ; save rax, set rcx if shift amt = 0
   (cmovz-r64-r64 r13 rax)
   (cmovz-r64-r64 rcx r12)
   ; perform safe shift
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
   ; restore eax if r12 =/ 0
   (cmp-r/m32-imm8 r12d (bv 0 8))
   (cmovne-r64-r64 rax r13)
   ; restore rcx
   (mov-r/m64-r64 rcx r14) ; TODO: replace with pop
  ))

(define spec-shl64
  (list
   (shl-r/m64-cl rax)))

(define attempt-shl64-imm
  (list
    ; split up rax
    (mov-r64-imm64 r10 (bv (expt 2 63) 64))
    (mov-r/m64-r64 r11 r10)
    (mov-r/m16-r16 r11w ax) ; r11 contains lower 16 bits
    (mov-r/m16-imm16 ax (bv 1 16)) ; rax contains upper 48 bits
    ; perform shifts
    (shl-r/m64-imm8 r11 (comp-simp:imm8-nz6))
    (shl-r/m64-imm8 rax (comp-simp:imm8-nz6))
    ; mutate r11 to make adding safe
    (rol-r/m64-imm8 r10 (comp-simp:imm8-nz6))
    (sub-r/m64-r64 r11 r10)
    ; recombine
    (add-r/m64-r64 rax r11)
    (sub-r/m64-r64 rax r10)
  ))

; CF- and ZF-correct
(define attempt-shl64-imm-cf
  (list
    ; split up rax
    (mov-r64-imm64 r10 (bv (expt 2 63) 64))
    (mov-r/m64-r64 r11 r10)
    (mov-r/m16-r16 r11w ax) ; r11 contains lower 16 bits
    (mov-r/m16-imm16 ax (bv 1 16)) ; rax contains upper 48 bits
    ; perform shifts
    (shl-r/m64-imm8 r11 (comp-simp:imm8-nz6))
    (shl-r/m64-imm8 rax (comp-simp:imm8-nz6))
    (setc r12b) ; save CF into arbitrary scratch reg
    ; mutate r11 to make adding safe
    (rol-r/m64-imm8 r10 (comp-simp:imm8-nz6))
    (sub-r/m64-r64 r11 r10)
    ; recombine
    (add-r/m64-r64 rax r11)
    (sub-r/m64-r64 rax r10)
    (bt-r/m64-imm8 r12 (bv 0 8)) ; restore CF
  ))

(define spec-shl64-imm
  (list
   (shl-r/m64-imm8 rax (comp-simp:imm8-nz6))))

; ---------- Bitwise SHR ----------

; Dest rax, src rcx
(define regs-shr
  (list rax rcx))

(define attempt-shr8
  (list
   ; save rcx
   (mov-r/m64-r64 r13 rcx) ; TODO: replace with push
   ; move and shift cl
   (mov-r/m32-imm32 r10d (bv (expt 2 31) 32))
   (mov-r/m8-r8 r10b cl)
   (shl-r/m64-imm8 r10 (bv (- 32 5) 8))
   ; test for masked cl bits = 0
   (mov-r64-imm64 r12 (bv 0 64))
   (cmp-r/m32-imm8 r10d (bv 0 8))
   (setz r12b)
   ; save rax, set rcx if shift amt = 0
   (cmovz-r64-r64 r10 rax)
   (cmovz-r64-r64 rcx r12)
      ; perform safe shift
      (mov-r64-imm64 r11 (bv (expt 2 63) 64))
      (mov-r/m8-r8 r11b al)
      (and-r/m8-imm8 cl (bv (- (expt 2 5) 1) 8))
      (shr-r/m64-cl r11)
      (mov-r/m8-r8 al r11b)
   ; restore rax if r12 =/ 0
   (cmp-r/m32-imm8 r12d (bv 0 8))
   (cmovne-r64-r64 rax r10)
   ; restore rcx
   (mov-r/m64-r64 rcx r13) ; TODO: replace with pop
  ))

(define spec-shr8
  (list
   (shr-r/m8-cl al)))

(define attempt-shr16
  (list
   ; save rcx
   (mov-r/m64-r64 r13 rcx) ; TODO: replace with push
   ; move and shift cl
   (mov-r/m32-imm32 r10d (bv (expt 2 31) 32))
   (mov-r/m8-r8 r10b cl)
   (shl-r/m64-imm8 r10 (bv (- 32 5) 8))
   ; test for masked cl bits = 0
   (mov-r64-imm64 r12 (bv 0 64))
   (cmp-r/m32-imm8 r10d (bv 0 8))
   (setz r12b)
   ; save rax, set rcx if shift amt = 0
   (cmovz-r64-r64 r10 rax)
   (cmovz-r64-r64 rcx r12)
      ; perform safe shift
      (mov-r64-imm64 r11 (bv (expt 2 63) 64))
      (mov-r/m16-r16 r11w ax)
      (and-r/m8-imm8 cl (bv (- (expt 2 5) 1) 8))
      (shr-r/m64-cl r11)
      (mov-r/m16-r16 ax r11w)
   ; restore rax if r12 =/ 0
   (cmp-r/m32-imm8 r12d (bv 0 8))
   (cmovne-r64-r64 rax r10)
   ; restore rcx
   (mov-r/m64-r64 rcx r13) ; TODO: replace with pop
  ))

(define spec-shr16
  (list
   (shr-r/m16-cl ax)))

(define attempt-shr32-path-sensitivable
  (list
   ; save rcx
   (mov-r/m64-r64 r13 rcx) ; TODO: replace with push
   (mov-r64-imm64 r12 (bv (expt 2 31) 64))
   (mov-r/m8-r8 r12b cl)
   (mov-r/m32-r32 ecx r12d)
   (mov-r64-imm64 r12 (bv 0 64))
   (and-r/m64-imm8 rcx (bv #x1F 8))
   (cmp-r/m64-imm8 rcx (bv 0 8))
   (setz r12b)
   ; save eax, set rcx if shift amt = 0
   (cmovz-r32-r32 r10d eax)
   (cmovz-r64-r64 rcx r12)
      ; perform safe shift
      (mov-r/m32-r32 eax eax)
      (mov-r64-imm64 r11 (bv (expt 2 63) 64))
      (sub-r/m64-r64 rax r11)
      (and-r/m8-imm8 cl (bv (- (expt 2 5) 1) 8))
      (shr-r/m64-cl rax)
      (mov-r/m32-r32 eax eax)
   ; restore eax if r12 =/ 0
   (cmp-r/m32-imm8 r12d (bv 0 8))
   (cmovne-r32-r32 eax r10d)
   ; restore rcx
   (mov-r/m64-r64 rcx r13) ; TODO: replace with pop
  ))

(define attempt-shr32
  (list
   ; save rcx
   (mov-r/m64-r64 r13 rcx) ; TODO: replace with push
   ; move and shift cl
   (mov-r/m32-imm32 r10d (bv (expt 2 31) 32))
   (mov-r/m8-r8 r10b cl)
   (shl-r/m64-imm8 r10 (bv (- 32 5) 8))
   ; test for masked cl bits = 0
   (mov-r64-imm64 r12 (bv 0 64))
   (cmp-r/m32-imm8 r10d (bv 0 8))
   (setz r12b)
   ; save eax, set rcx if shift amt = 0
   (cmovz-r32-r32 r10d eax)
   (cmovz-r64-r64 rcx r12)
      ; perform safe shift
      (mov-r/m32-r32 eax eax)
      (mov-r64-imm64 r11 (bv (expt 2 63) 64))
      (sub-r/m64-r64 rax r11)
      (and-r/m8-imm8 cl (bv (- (expt 2 5) 1) 8))
      (shr-r/m64-cl rax)
      (mov-r/m32-r32 eax eax)
   ; restore eax if r12 =/ 0
   (cmp-r/m32-imm8 r12d (bv 0 8))
   (cmovne-r32-r32 eax r10d)
   ; restore rcx
   (mov-r/m64-r64 rcx r13) ; TODO: replace with pop
  ))

(define spec-shr32
  (list
   (shr-r/m32-cl eax)))

(define attempt-shr64
  (list
   ; save rcx
   (mov-r/m64-r64 r14 rcx) ; TODO: replace with push
   ; move and shift cl
   (mov-r/m32-imm32 r13d (bv (expt 2 31) 32))
   (mov-r/m8-r8 r13b cl)
   (shl-r/m64-imm8 r13 (bv (- 32 6) 8))
   ; test for masked cl bits = 0
   (mov-r64-imm64 r12 (bv 0 64))
   (cmp-r/m32-imm8 r13d (bv 0 8))
   (setz r12b)
   ; save rax, set rcx if shift amt = 0
   (cmovz-r64-r64 r13 rax)
   (cmovz-r64-r64 rcx r12)
   ; perform safe shift
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
   ; restore eax if r12 =/ 0
   (cmp-r/m32-imm8 r12d (bv 0 8))
   (cmovne-r64-r64 rax r13)
   ; restore rcx
   (mov-r/m64-r64 rcx r14) ; TODO: replace with pop
  ))

(define spec-shr64
  (list
   (shr-r/m64-cl rax)))

; CF- and ZF-correct
; Does not apply when imm = 0
(define attempt-shr64-imm-cf
  (list
    ; split up rax
    (mov-r64-imm64 r10 (bv (expt 2 63) 64))
    (mov-r/m64-r64 r11 r10)
    (mov-r/m16-r16 r11w ax) ; r11 contains lower 16 bits
    (mov-r/m16-imm16 ax (bv 1 16)) ; rax contains upper 48 bits
    ; perform shifts
    (if (bvule (extract 5 0 (comp-simp:imm8-nz6)) (bv 16 6))
      (shr-r/m64-imm8 rax (comp-simp:imm8-nz6))
      (shr-r/m64-imm8 r11 (comp-simp:imm8-nz6))
    )
    (if (bvule (extract 5 0 (comp-simp:imm8-nz6)) (bv 16 6))
      (shr-r/m64-imm8 r11 (comp-simp:imm8-nz6))
      (shr-r/m64-imm8 rax (comp-simp:imm8-nz6))
    )
    (setc r12b) ; save CF into arbitrary scratch reg
    ; mutate r11 and rax to make adding safe
    (shr-r/m64-imm8 r10 (comp-simp:imm8-nz6))
    ; (add-r/m64-r64 r11 r10)
    (shl-r/m64-1 r10)
    (sub-r/m64-r64 rax r10)
    ; recombine
    (add-r/m64-r64 rax r11)
    (shr-r/m64-1 r10)
    (add-r/m64-r64 rax r10)
    (bt-r/m64-imm8 r12 (bv 0 8)) ; restore CF
  ))

(define spec-shr64-imm
  (list
   (shr-r/m64-imm8 rax (comp-simp:imm8-nz6))))

; ---------- Bitwise SAR ----------

; Dest rax, src rcx
(define regs-sar
  (list rax rcx))

(define attempt-sar8
  (list
   ; save rcx
   (mov-r/m64-r64 r13 rcx) ; TODO: replace with push
   ; move and shift cl
   (mov-r/m32-imm32 r10d (bv (expt 2 31) 32))
   (mov-r/m8-r8 r10b cl)
   (shl-r/m64-imm8 r10 (bv (- 32 5) 8))
   ; test for masked cl bits = 0
   (mov-r64-imm64 r12 (bv 0 64))
   (cmp-r/m32-imm8 r10d (bv 0 8))
   (setz r12b)
   ; save rax, set rcx if shift amt = 0
   (cmovz-r64-r64 r10 rax)
   (cmovz-r64-r64 rcx r12)
      ; perform safe shift
      (mov-r64-imm64 r11 (bv (expt 2 63) 64))
      (mov-r/m8-r8 r11b al)
      (and-r/m8-imm8 cl (bv (- (expt 2 5) 1) 8))
      (rol-r/m64-imm8 r11 (bv 56 8))
      (sar-r/m64-cl r11)
      (ror-r/m64-imm8 r11 (bv 56 8))
      (mov-r/m8-r8 al r11b)
   ; restore rax if r12 =/ 0
   (cmp-r/m32-imm8 r12d (bv 0 8))
   (cmovne-r64-r64 rax r10)
   ; restore rcx
   (mov-r/m64-r64 rcx r13) ; TODO: replace with pop
  ))

(define spec-sar8
  (list
   (sar-r/m8-cl al)))

(define attempt-sar16
  (list
   ; save rcx
   (mov-r/m64-r64 r13 rcx) ; TODO: replace with push
   ; move and shift cl
   (mov-r/m32-imm32 r10d (bv (expt 2 31) 32))
   (mov-r/m8-r8 r10b cl)
   (shl-r/m64-imm8 r10 (bv (- 32 5) 8))
   ; test for masked cl bits = 0
   (mov-r64-imm64 r12 (bv 0 64))
   (cmp-r/m32-imm8 r10d (bv 0 8))
   (setz r12b)
   ; save rax, set rcx if shift amt = 0
   (cmovz-r64-r64 r10 rax)
   (cmovz-r64-r64 rcx r12)
      ; perform safe shift
      (mov-r64-imm64 r11 (bv (expt 2 63) 64))
      (mov-r/m16-r16 r11w ax)
      (and-r/m8-imm8 cl (bv (- (expt 2 5) 1) 8))
      (rol-r/m64-imm8 r11 (bv 48 8))
      (sar-r/m64-cl r11)
      (ror-r/m64-imm8 r11 (bv 48 8))
      (mov-r/m16-r16 ax r11w)
   ; restore rax if r12 =/ 0
   (cmp-r/m32-imm8 r12d (bv 0 8))
   (cmovne-r64-r64 rax r10)
   ; restore rcx
   (mov-r/m64-r64 rcx r13) ; TODO: replace with pop
  ))

(define spec-sar16
  (list
   (sar-r/m16-cl ax)))

(define attempt-sar32
  (list
   ; save rcx
   (mov-r/m64-r64 r13 rcx) ; TODO: replace with push
   ; move and shift cl
   (mov-r/m32-imm32 r10d (bv (expt 2 31) 32))
   (mov-r/m8-r8 r10b cl)
   (shl-r/m64-imm8 r10 (bv (- 32 5) 8))
   ; test for masked cl bits = 0
   (mov-r64-imm64 r12 (bv 0 64))
   (cmp-r/m32-imm8 r10d (bv 0 8))
   (setz r12b)
   ; save eax, set rcx if shift amt = 0
   (cmovz-r32-r32 r10d eax)
   (cmovz-r64-r64 rcx r12)
      ; perform safe shift
      (mov-r/m32-r32 eax eax)
      (mov-r64-imm64 r11 (bv (expt 2 63) 64))
      (sub-r/m64-r64 rax r11)
      (and-r/m8-imm8 cl (bv (- (expt 2 5) 1) 8))
      (rol-r/m64-imm8 rax (bv 32 8))
      (sar-r/m64-cl rax)
      (ror-r/m64-imm8 rax (bv 32 8))
      (mov-r/m32-r32 eax eax)
   ; restore eax if r12 =/ 0
   (cmp-r/m32-imm8 r12d (bv 0 8))
   (cmovne-r32-r32 eax r10d)
   ; restore rcx
   (mov-r/m64-r64 rcx r13) ; TODO: replace with pop
  ))

(define spec-sar32
  (list
   (sar-r/m32-cl eax)))

(define attempt-sar64
  (list
   ; save rcx
   (mov-r/m64-r64 r14 rcx) ; TODO: replace with push
   ; move and shift cl
   (mov-r/m32-imm32 r13d (bv (expt 2 31) 32))
   (mov-r/m8-r8 r13b cl)
   (shl-r/m64-imm8 r13 (bv (- 32 6) 8))
   ; test for masked cl bits = 0
   (mov-r64-imm64 r12 (bv 0 64))
   (cmp-r/m32-imm8 r13d (bv 0 8))
   (setz r12b)
   ; save rax, set rcx if shift amt = 0
   (cmovz-r64-r64 r13 rax)
   (cmovz-r64-r64 rcx r12)
   ; perform safe shift
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
   ; restore eax if r12 =/ 0
   (cmp-r/m32-imm8 r12d (bv 0 8))
   (cmovne-r64-r64 rax r13)
   ; restore rcx
   (mov-r/m64-r64 rcx r14) ; TODO: replace with pop
  ))

(define spec-sar64
  (list
   (sar-r/m64-cl rax)))
