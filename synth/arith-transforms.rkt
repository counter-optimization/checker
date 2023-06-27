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

(define attempt-sub64-cf-zf
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
   (ror-r/m64-imm8 r11 (bv 16 8))
   (ror-r/m64-imm8 r10 (bv 16 8))
   (rol-r/m64-imm8 rax (bv 16 8))
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

; Sets CF
(define attempt-add8-imm8
  (list
   (mov-r/m64-imm32 r11 (bv 0 32))
   (mov-r/m8-imm8 r11b (comp-simp:imm8)) ; move immediate into register
   (mov-r/m64-imm32 r10 (bv (expt 2 31) 32))
   (mov-r/m8-r8 r10b cl) ; split lower 8 bits of rcx into r10
   (add-r/m64-r64 r10 r11) ; perform addition
   (bt-r/m64-imm8 r10 (bv 8 8))
   (mov-r/m8-r8 cl r10b) ; recombine result
  ))

(define spec-add8-imm8
  (list
   (add-r/m8-imm8 cl (comp-simp:imm8))))

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
  
; Sets CF correctly
(define attempt-add32-cf
  (list
   (mov-r/m64-r64 r11 rax) ; save rax
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of rcx
   (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
   (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
   (mov-r/m32-r32 eax eax) ; zero top 32 bits of rax
   (sub-r/m64-imm32 rax (bv (expt 2 31) 32))
   (sub-r/m64-imm32 rax (bv (expt 2 31) 32))
   (add-r/m64-r64 rcx rax) ; perform add
   (bt-r/m64-imm8 rcx (bv 32 8)) ; set CF
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m64-r64 rax r11) ; restore rax
 ))

; Sets CF and ZF correctly
(define attempt-add32-cf-zf
  (list
   (mov-r/m64-r64 r11 rax) ; save rax
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of rcx
   (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
   (sub-r/m64-imm32 rcx (bv (expt 2 31) 32))
   (mov-r/m32-r32 eax eax) ; zero top 32 bits of rax
   (sub-r/m64-imm32 rax (bv (expt 2 31) 32))
   (sub-r/m64-imm32 rax (bv (expt 2 31) 32))
   (add-r/m64-r64 rcx rax) ; perform add
   (cmp-r/m32-imm8 ecx (bv 0 8)) ; set ZF
   (bt-r/m64-imm8 rcx (bv 32 8)) ; set CF
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
   (mov-r/m64-r64 rax r11) ; restore rax
 ))

(define spec-add32
  (list
   (add-r/m32-r32 ecx eax)))
  
(define attempt-add32-imm8-cf-zf
  (list
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of rcx
   (mov-r64-imm64 r11 (bv (expt 2 33) 64)) ; mask upper bits
   (sub-r/m64-r64 rcx r11)
   (mov-r/m64-imm32 r11 (bv 0 32)) ; move immediate to register
   (add-r/m32-imm8 r11d (comp-simp:imm8))
   (add-r/m64-r64 rcx r11) ; perform add
   (cmp-r/m32-imm8 ecx (bv 0 8)) ; set ZF
   (bt-r/m64-imm8 rcx (bv 32 8)) ; set CF
   (mov-r/m32-r32 ecx ecx) ; zero top 32 bits of ecx
 ))

(define spec-add32-imm8
  (list
   (add-r/m32-imm8 ecx (comp-simp:imm8))))
  
; Sets CF correctly
(define attempt-add64
  (list
   (mov-r64-imm64 r10 (bv (expt 2 48) 64)) ; split operands 48/16
   (mov-r/m16-r16 r10w cx) ; split lower 16 bits of rcx into r10
   (mov-r/m16-imm16 cx (bv 1 16))  ; mask out lower 16 bits of rcx
   (mov-r64-imm64 r11 (bv (expt 2 48) 64))
   (mov-r/m16-r16 r11w ax) ; split lower 16 bits of rax into r11
   (mov-r/m16-imm16 ax (bv 1 16))  ; mask out lower 16 bits of rax
   (rol-r/m64-imm8 r10 (bv 16 8)) ; rotate into position
   (rol-r/m64-imm8 r11 (bv 16 8))
   (ror-r/m64-imm8 rcx (bv 16 8))
   (ror-r/m64-imm8 rax (bv 16 8))
   (add-r/m32-r32 r10d r11d) ; add parts
   (adc-r/m64-r64 rcx rax)
   (ror-r/m64-imm8 r11 (bv 16 8)) ; rotate back
   (rol-r/m64-imm8 rax (bv 16 8))
   (ror-r/m64-imm8 r10 (bv 16 8))
   (rcl-r/m64-imm8 rcx (bv 16 8))
   (mov-r/m16-r16 cx r10w) ; recombine
   (mov-r/m16-r16 ax r11w) ; restore rax
  ))
  
; Sets CF and ZF correctly
(define attempt-add64-zf
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

; Sets CF, ZF
(define attempt-add64-imm8
  (list
   (mov-r/m64-imm32 r12 (bv 0 32)) ; move immediate to register
   (add-r/m64-imm8 r12 (comp-simp:imm8))
   ; split operands 48/16
   (mov-r64-imm64 r10 (bv (expt 2 48) 64))
   (mov-r/m16-r16 r10w cx) ; split lower 16 bits of rcx into r10
   (mov-r/m16-imm16 cx (bv 1 16))  ; mask out lower 16 bits of rcx
   (mov-r64-imm64 r11 (bv (expt 2 48) 64))
   (mov-r/m16-r16 r11w r12w) ; split lower 16 bits of r12 into r11
   (mov-r/m16-imm16 r12w (bv 1 16))  ; mask out lower 16 bits of rax
   ; rotate into position
   (rol-r/m64-imm8 r10 (bv 16 8))
   (rol-r/m64-imm8 r11 (bv 16 8))
   (ror-r/m64-imm8 rcx (bv 16 8))
   (ror-r/m64-imm8 r12 (bv 16 8))
   ; add parts
   (add-r/m32-r32 r10d r11d)
   (adc-r/m64-r64 rcx r12)
   ; rotate back
   (ror-r/m64-imm8 r11 (bv 16 8))
   (rol-r/m64-imm8 r12 (bv 16 8))
   (ror-r/m64-imm8 r10 (bv 16 8))
   (rcl-r/m64-imm8 rcx (bv 16 8))
   ; recombine
   (mov-r/m16-r16 cx r10w)
   ; set flags
   (setc r12b)
   (cmp-r/m64-imm8 rcx (bv 0 8))
   (bt-r/m64-imm8 r12 (bv 0 8))
  ))

(define spec-add64-imm8
  (list
   (add-r/m64-imm8 rcx (comp-simp:imm8))))


; Sets CF, ZF
(define attempt-add64-imm32
  (list
   (mov-r/m64-imm32 r12 (bv 0 32)) ; move immediate to register
   (add-r/m64-imm32 r12 (comp-simp:imm32))
   ; split operands 48/16
   (mov-r64-imm64 r10 (bv (expt 2 48) 64))
   (mov-r/m16-r16 r10w cx) ; split lower 16 bits of rcx into r10
   (mov-r/m16-imm16 cx (bv 1 16))  ; mask out lower 16 bits of rcx
   (mov-r64-imm64 r11 (bv (expt 2 48) 64))
   (mov-r/m16-r16 r11w r12w) ; split lower 16 bits of r12 into r11
   (mov-r/m16-imm16 r12w (bv 1 16))  ; mask out lower 16 bits of rax
   ; rotate into position
   (rol-r/m64-imm8 r10 (bv 16 8))
   (rol-r/m64-imm8 r11 (bv 16 8))
   (ror-r/m64-imm8 rcx (bv 16 8))
   (ror-r/m64-imm8 r12 (bv 16 8))
   ; add parts
   (add-r/m32-r32 r10d r11d)
   (adc-r/m64-r64 rcx r12)
   ; rotate back
   (ror-r/m64-imm8 r11 (bv 16 8))
   (rol-r/m64-imm8 r12 (bv 16 8))
   (ror-r/m64-imm8 r10 (bv 16 8))
   (rcl-r/m64-imm8 rcx (bv 16 8))
   ; recombine
   (mov-r/m16-r16 cx r10w)
   ; set flags
   (setc r12b)
   (cmp-r/m64-imm8 rcx (bv 0 8))
   (bt-r/m64-imm8 r12 (bv 0 8))
  ))

(define spec-add64-imm32
  (list
   (add-r/m64-imm32 rcx (comp-simp:imm32))))
  
