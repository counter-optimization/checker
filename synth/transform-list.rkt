#lang rosette

(require
  "arith-transforms.rkt"
  "bitwise-transforms.rkt"
  "shift-transforms.rkt"
  "mul-transforms.rkt")

(provide (all-defined-out))

(define (comp-simp-transform insn)
  (case insn 
    [("ADD8rr")              (list (cons attempt-add8 spec-add8))]
    [("ADD8ri")              (list (cons attempt-add8-imm8 spec-add8-imm8))]
    [("ADD16rr")             (list (cons attempt-add16 spec-add16))]
    [("ADD32rr" "ADD32rm")   (list (cons attempt-add32 spec-add32)
                                   (cons attempt-add32-cf spec-add32)
                                   (cons attempt-add32-cf-zf spec-add32))]
    [("ADD32ri8")            (list (cons attempt-add32-imm8-zf attempt-add32-imm8-cf-zf))]
    [("ADD64rr" "ADD64rm" "ADD64mr")      (list (cons attempt-add64-zf spec-add64))]
    [("ADD64i32" "ADD64ri32" "ADD64mi32") (list (cons attempt-add64-imm32 spec-add64-imm32))]
    [("ADD64ri8" "ADD64mi8") (list (cons attempt-add64-imm8 spec-add64-imm8))]
 
    [("SUB8rr")              (list (cons attempt-sub8 spec-sub8))]
    [("SUB16rr")             (list (cons attempt-sub16 spec-sub16))]
    [("SUB32rr")             (list (cons attempt-sub32 spec-sub32))]
    [("SUB64rr" "SUB64rm")   (list (cons attempt-sub64 spec-sub64)
                                   (cons attempt-sub64-cf-zf spec-sub64))]
     
    [("MUL32r")              (list (cons attempt-mul32 spec-mul32))]
    [("MUL64r" "MUL64m")     (list (cons attempt-mul64-p1 spec-mul64-p1)
                                   (cons attempt-mul64-p2 spec-mul64-p2)
                                   (cons attempt-mul64-p3 spec-mul64-p3)
                                   (cons attempt-mul64-p4 spec-mul64-p4)
                                   (cons attempt-mul64-p5 spec-mul64-p5))]

    [("IMUL32rr" "IMUL32rm") (list (cons attempt-imul32-rr spec-imul32-rr) 
                                   (cons attempt-imul32-rr-cf spec-imul32-rr))]
    [("IMUL32rri8")          (list (cons attempt-imul32-rri8))]
    [("IMUL64rr" "IMUL64rm") (list (cons attempt-imul64-rr-p1 spec-imul64-rr-p1)
                                   (cons attempt-imul64-rr-p2 spec-imul64-rr-p2)
                                   (cons attempt-imul64-rr-p3 spec-imul64-rr-p3)
                                   (cons attempt-imul64-rr-p4 spec-imul64-rr-p4)
                                   (cons attempt-imul64-rr-p5 spec-imul64-rr-p5))]
    [("IMUL64rri8")          (list (cons attempt-imul64-rri8-p1 spec-imul64-rri8-p1)
                                   (cons attempt-imul64-rri8-p2 spec-imul64-rri8-p2)
                                   (cons attempt-imul64-rri8-p3 spec-imul64-rri8-p3))]

    [("AND8rr")              (list (cons attempt-and8 spec-and8))]
    [("AND16rr")             (list (cons attempt-and16 spec-and16))]
    [("AND32rr")             (list (cons attempt-and32 spec-and32)
                                   (cons attempt-and32-cf-zf spec-and32))]
    [("AND32ri8")            (list (cons attempt-and32-imm8-cf-zf spec-and32-imm8))]
    [("AND64rr" "AND64rm")   (list (cons attempt-and64 spec-and64)
                                   (cons attempt-and64-cf-zf spec-and64))]
  
    [("OR8rr" "OR8rm")       (list (cons attempt-or8 spec-or8))]
    [("OR16rr" "OR16rm")     (list (cons attempt-or16 spec-or16))]
    [("OR32rr" "OR32rm")     (list (cons attempt-or32 spec-or32))]
    [("OR64rr" "OR64rm")     (list (cons attempt-or64 spec-or64)
                                   (cons attempt-or64-cf-zf spec-or64))]
  
    [("XOR8rr" "XOR8rm")     (list (cons attempt-xor8 spec-xor8))]
    [("XOR16rr" "XOR16rm")   (list (cons attempt-xor16 spec-xor16))]
    [("XOR32rr" "XOR32rm")   (list (cons attempt-xor32 spec-xor32))]
    [("XOR32ri8")            (list (cons attempt-xor32-imm8 spec-xor32-imm8))]
    [("XOR64rr" "XOR64rm")   (list (cons attempt-xor64 spec-xor64))]
  
    [("SHL8rCL")             (list (cons attempt-shl8 spec-shl8)
                                   (cons attempt-shl8-path-sensitivable spec-shl8))]
    [("SHL16rCL")            (list (cons attempt-shl16 spec-shl16))]
    [("SHL32rCL")            (list (cons attempt-shl32 spec-shl32)
                                   (cons attempt-shl32-path-sensitivable spec-shl32))]
    [("SHL64ri")             (list (cons attempt-shl64-imm spec-shl64-imm)
                                   (cons attempt-shl64-imm-cf spec-shl64-imm))]
          
    [("SHR8rCL")             (list (cons attempt-shr8 spec-shr8))]
    [("SHR16rCL")            (list (cons attempt-shr16 spec-shr16))]
    [("SHR32rCL")            (list (cons attempt-shr32 spec-shr32)
                                   (cons attempt-shr32-path-sensitivable spec-shr32))]
    [("SHR64rCL")            (list (cons attempt-shr64 spec-shr64))]
    [("SHR64ri" "SHR64r1")   (list (cons attempt-shr64-imm-cf spec-shr64-imm))]
  
    [("SAR8rCL")             (list (cons attempt-sar8 spec-sar8))]
    [("SAR16rCL")            (list (cons attempt-sar16 spec-sar16))]
    [("SAR32rCL")            (list (cons attempt-sar32 spec-sar32))]
    [("SAR32ri" "SAR32r1")   (list (cons attempt-sar32-imm8 spec-sar32-imm8))]
    [("SAR64rCL")            (list (cons attempt-sar64 spec-sar64))]

    [else 'none]))
