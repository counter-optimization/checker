#lang racket

(struct Statement () #:transparent)
(struct Store Statement (ptr idx value) #:transparent)
(struct Load Statement (value ptr idx) #:transparent)
(struct Get Statement (var) #:transparent)
(struct Put Statement (var value) #:transparent)
(struct DeclVar Statement (var) #:transparent)

(struct Binop (op1 op2) #:transparent)
(struct Add Binop () #:transparent)
(struct Lt Binop () #:transparent)
(struct Gt Binop () #:transparent)
(struct Lte Binop () #:transparent)
(struct Gte Binop () #:transparent)
(struct Eq Binop () #:transparent)
(struct Sub Binop () #:transparent)
(struct Mul Binop () #:transparent)
(struct Div Binop () #:transparent)
(struct Bvand Binop () #:transparent)
(struct Bvxor Binop () #:transparent)
(struct Bvor Binop () #:transparent)
(struct And Binop () #:transparent)
(struct Or Binop () #:transparent)
(struct RShift Binop () #:transparent)
(struct LShift Binop () #:transparent)
(struct Cast Binop () #:transparent) ; op1 is type, op2 is expr
(struct Modulo Binop () #:transparent)
(struct Index Binop () #:transparent) ; op1 is ptr, op2 is idx

(struct Unop (op1) #:transparent)

(struct Value (val) #:transparent)
(struct Variable (type name) #:transparent)

(struct Function (name arg-list insn-list) #:transparent)
(struct ApplyFunction (name arg-list) #:transparent)

(struct Program (func-list) #:transparent)


;; lexing

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define-empty-tokens op-tokens (+ - / * > >= < <= != == ! AND OR = COMMA >> << % & ^
                                FOR IF ELSE COLON LBRACE RBRACE LPAREN RPAREN
                                LSQUARE RSQUARE
                                NUMTYPE STRINGTYPE RETURN STRINGITERTYPE NUMITERTYPE
                                CONCAT APPEND STR_LENGTH EQUALS SPLIT ITER_CONCAT ITER_LENGTH 
                                FIRST STON NTOS SEMICOLON PIPE EOF
                                STATIC INLINE VOID UNSIGNED
                                U8P U16P U32P U64P U128P
                                U8 U16 U32 U64 U128))

(define-tokens val-tokens (VAR NUM STR COMMENT))

(define-lex-abbrevs
    (lower (:/ "a" "z"))
    (upper (:/ "A" "Z"))
    (digit (:/ "0" "9"))
    (hexdigit (:or (:/ "a" "f") (:/ "A" "F") digit))
    (single-line-comment (:: "//" (:* any-char) #\newline))
    (multi-line-comment (:: "/*" (:* (:or (:~ "*") (:: (:+ "*") (:& (:~ "*") (:~ "/"))))) (:+ "*") "/"))
    (whitespace (:or #\tab #\space #\newline))
    (operator (:or "+" "-" "/" "*" ">" ">=" "<" "<=" "!=" "==" "!" "=" ">>" "<<" "%" "&" "^"))
    (var-name (:: (:or lower upper) (:* (:or lower upper digit "_" "-"))))
    (const 
      (:or
	      (:: (:? "-") "0x" (:+ hexdigit))
	      (:: (:? "-") (:+ digit)))))

(define c-lexer
  (lexer 
    [(eof) 'EOF]
    [(:or "u8 *" "uint8_t *") 'U8P]
    [(:or "u16 *" "uint16_t *") 'U16P]
    [(:or "u32 *" "uint32_t *") 'U32P]
    [(:or "u64 *" "uint64_t *") 'U64P]
    [(:or "u128 *" "uint128_t *") 'U128P]
    [(:or "u8" "uint8_t") 'U8]
    [(:or "u16" "uint16_t") 'U16]
    [(:or "u32" "uint32_t") 'U32]
    [(:or "u64" "uint64_t") 'U64]
    [(:or "u128" "uint128_t") 'U128]
    ["=" 'EQUALS]
    ["&" '&]
    ["|" 'PIPE]
    [";" 'SEMICOLON]
    ["," 'COMMA]
    ["&&" 'AND]
    ["||" 'OR]
    ["for" 'FOR]
    ["if" 'IF]
    ["else" 'ELSE]
    [":" 'COLON]
    ["{" 'LBRACE]
    ["}" 'RBRACE]
    ["(" 'LPAREN]
    [")" 'RPAREN]
    ["[" 'LSQUARE]
    ["]" 'RSQUARE]
    ["static" 'STATIC]
    ["inline" 'INLINE]
    ["void" 'VOID]
    ["U" 'UNSIGNED]
    [const (token-NUM lexeme)]
    [operator (string->symbol lexeme)]
    [whitespace (c-lexer input-port)]
    [single-line-comment (c-lexer input-port)]
    [multi-line-comment (c-lexer input-port)]
    [var-name (token-VAR lexeme)]))

(define in (open-input-string "
static inline void Hacl_Impl_Curve25519_Field51_fadd(u64 *out, u64 *f1, u64 *f2)
{
  u64 f10 = f1[0U];
  u64 f20 = f2[0U];
  u64 f11 = f1[1U];
  u64 f21 = f2[1U];
  u64 f12 = f1[2U];
  u64 f22 = f2[2U];
  u64 f13 = f1[3U];
  u64 f23 = f2[3U];
  u64 f14 = f1[4U];
  u64 f24 = f2[4U];
  out[0U] = f10 + f20;
  out[1U] = f11 + f21;
  out[2U] = f12 + f22;
  out[3U] = f13 + f23;
  out[4U] = f14 + f24;
}

static inline void Hacl_Impl_Curve25519_Field51_fsub(u64 *out, u64 *f1, u64 *f2)
{
  u64 f10 = f1[0U];
  u64 f20 = f2[0U];
  u64 f11 = f1[1U];
  u64 f21 = f2[1U];
  u64 f12 = f1[2U];
  u64 f22 = f2[2U];
  u64 f13 = f1[3U];
  u64 f23 = f2[3U];
  u64 f14 = f1[4U];
  u64 f24 = f2[4U];
  out[0U] = f10 + (u64)0x3fffffffffff68U - f20;
  out[1U] = f11 + (u64)0x3ffffffffffff8U - f21;
  out[2U] = f12 + (u64)0x3ffffffffffff8U - f22;
  out[3U] = f13 + (u64)0x3ffffffffffff8U - f23;
  out[4U] = f14 + (u64)0x3ffffffffffff8U - f24;
}

static inline void
Hacl_Impl_Curve25519_Field51_fmul(u64 *out, u64 *f1, u64 *f2, uint128_t *uu___)
{
  u64 f10 = f1[0U];
  u64 f11 = f1[1U];
  u64 f12 = f1[2U];
  u64 f13 = f1[3U];
  u64 f14 = f1[4U];
  u64 f20 = f2[0U];
  u64 f21 = f2[1U];
  u64 f22 = f2[2U];
  u64 f23 = f2[3U];
  u64 f24 = f2[4U];
  u64 tmp1 = f21 * (u64)19U;
  u64 tmp2 = f22 * (u64)19U;
  u64 tmp3 = f23 * (u64)19U;
  u64 tmp4 = f24 * (u64)19U;
  uint128_t o00 = (uint128_t)f10 * f20;
  uint128_t o10 = (uint128_t)f10 * f21;
  uint128_t o20 = (uint128_t)f10 * f22;
  uint128_t o30 = (uint128_t)f10 * f23;
  uint128_t o40 = (uint128_t)f10 * f24;
  uint128_t o01 = o00 + (uint128_t)f11 * tmp4;
  uint128_t o11 = o10 + (uint128_t)f11 * f20;
  uint128_t o21 = o20 + (uint128_t)f11 * f21;
  uint128_t o31 = o30 + (uint128_t)f11 * f22;
  uint128_t o41 = o40 + (uint128_t)f11 * f23;
  uint128_t o02 = o01 + (uint128_t)f12 * tmp3;
  uint128_t o12 = o11 + (uint128_t)f12 * tmp4;
  uint128_t o22 = o21 + (uint128_t)f12 * f20;
  uint128_t o32 = o31 + (uint128_t)f12 * f21;
  uint128_t o42 = o41 + (uint128_t)f12 * f22;
  uint128_t o03 = o02 + (uint128_t)f13 * tmp2;
  uint128_t o13 = o12 + (uint128_t)f13 * tmp3;
  uint128_t o23 = o22 + (uint128_t)f13 * tmp4;
  uint128_t o33 = o32 + (uint128_t)f13 * f20;
  uint128_t o43 = o42 + (uint128_t)f13 * f21;
  uint128_t o04 = o03 + (uint128_t)f14 * tmp1;
  uint128_t o14 = o13 + (uint128_t)f14 * tmp2;
  uint128_t o24 = o23 + (uint128_t)f14 * tmp3;
  uint128_t o34 = o33 + (uint128_t)f14 * tmp4;
  uint128_t o44 = o43 + (uint128_t)f14 * f20;
  uint128_t tmp_w0 = o04;
  uint128_t tmp_w1 = o14;
  uint128_t tmp_w2 = o24;
  uint128_t tmp_w3 = o34;
  uint128_t tmp_w4 = o44;
  uint128_t l_ = tmp_w0 + (uint128_t)(u64)0U;
  u64 tmp01 = (uint64_t)l_ & (u64)0x7ffffffffffffU;
  u64 c0 = (uint64_t)(l_ >> (u32)51U);
  uint128_t l_0 = tmp_w1 + (uint128_t)c0;
  u64 tmp11 = (uint64_t)l_0 & (u64)0x7ffffffffffffU;
  u64 c1 = (uint64_t)(l_0 >> (u32)51U);
  uint128_t l_1 = tmp_w2 + (uint128_t)c1;
  u64 tmp21 = (uint64_t)l_1 & (u64)0x7ffffffffffffU;
  u64 c2 = (uint64_t)(l_1 >> (u32)51U);
  uint128_t l_2 = tmp_w3 + (uint128_t)c2;
  u64 tmp31 = (uint64_t)l_2 & (u64)0x7ffffffffffffU;
  u64 c3 = (uint64_t)(l_2 >> (u32)51U);
  uint128_t l_3 = tmp_w4 + (uint128_t)c3;
  u64 tmp41 = (uint64_t)l_3 & (u64)0x7ffffffffffffU;
  u64 c4 = (uint64_t)(l_3 >> (u32)51U);
  u64 l_4 = tmp01 + c4 * (u64)19U;
  u64 tmp0_ = l_4 & (u64)0x7ffffffffffffU;
  u64 c5 = l_4 >> (u32)51U;
  u64 o0 = tmp0_;
  u64 o1 = tmp11 + c5;
  u64 o2 = tmp21;
  u64 o3 = tmp31;
  u64 o4 = tmp41;
  out[0U] = o0;
  out[1U] = o1;
  out[2U] = o2;
  out[3U] = o3;
  out[4U] = o4;
}

static inline void
Hacl_Impl_Curve25519_Field51_fmul2(u64 *out, u64 *f1, u64 *f2, uint128_t *uu___)
{
  u64 f10 = f1[0U];
  u64 f11 = f1[1U];
  u64 f12 = f1[2U];
  u64 f13 = f1[3U];
  u64 f14 = f1[4U];
  u64 f20 = f2[0U];
  u64 f21 = f2[1U];
  u64 f22 = f2[2U];
  u64 f23 = f2[3U];
  u64 f24 = f2[4U];
  u64 f30 = f1[5U];
  u64 f31 = f1[6U];
  u64 f32 = f1[7U];
  u64 f33 = f1[8U];
  u64 f34 = f1[9U];
  u64 f40 = f2[5U];
  u64 f41 = f2[6U];
  u64 f42 = f2[7U];
  u64 f43 = f2[8U];
  u64 f44 = f2[9U];
  u64 tmp11 = f21 * (u64)19U;
  u64 tmp12 = f22 * (u64)19U;
  u64 tmp13 = f23 * (u64)19U;
  u64 tmp14 = f24 * (u64)19U;
  u64 tmp21 = f41 * (u64)19U;
  u64 tmp22 = f42 * (u64)19U;
  u64 tmp23 = f43 * (u64)19U;
  u64 tmp24 = f44 * (u64)19U;
  uint128_t o00 = (uint128_t)f10 * f20;
  uint128_t o15 = (uint128_t)f10 * f21;
  uint128_t o25 = (uint128_t)f10 * f22;
  uint128_t o30 = (uint128_t)f10 * f23;
  uint128_t o40 = (uint128_t)f10 * f24;
  uint128_t o010 = o00 + (uint128_t)f11 * tmp14;
  uint128_t o110 = o15 + (uint128_t)f11 * f20;
  uint128_t o210 = o25 + (uint128_t)f11 * f21;
  uint128_t o310 = o30 + (uint128_t)f11 * f22;
  uint128_t o410 = o40 + (uint128_t)f11 * f23;
  uint128_t o020 = o010 + (uint128_t)f12 * tmp13;
  uint128_t o120 = o110 + (uint128_t)f12 * tmp14;
  uint128_t o220 = o210 + (uint128_t)f12 * f20;
  uint128_t o320 = o310 + (uint128_t)f12 * f21;
  uint128_t o420 = o410 + (uint128_t)f12 * f22;
  uint128_t o030 = o020 + (uint128_t)f13 * tmp12;
  uint128_t o130 = o120 + (uint128_t)f13 * tmp13;
  uint128_t o230 = o220 + (uint128_t)f13 * tmp14;
  uint128_t o330 = o320 + (uint128_t)f13 * f20;
  uint128_t o430 = o420 + (uint128_t)f13 * f21;
  uint128_t o040 = o030 + (uint128_t)f14 * tmp11;
  uint128_t o140 = o130 + (uint128_t)f14 * tmp12;
  uint128_t o240 = o230 + (uint128_t)f14 * tmp13;
  uint128_t o340 = o330 + (uint128_t)f14 * tmp14;
  uint128_t o440 = o430 + (uint128_t)f14 * f20;
  uint128_t tmp_w10 = o040;
  uint128_t tmp_w11 = o140;
  uint128_t tmp_w12 = o240;
  uint128_t tmp_w13 = o340;
  uint128_t tmp_w14 = o440;
  uint128_t o0 = (uint128_t)f30 * f40;
  uint128_t o1 = (uint128_t)f30 * f41;
  uint128_t o2 = (uint128_t)f30 * f42;
  uint128_t o3 = (uint128_t)f30 * f43;
  uint128_t o4 = (uint128_t)f30 * f44;
  uint128_t o01 = o0 + (uint128_t)f31 * tmp24;
  uint128_t o111 = o1 + (uint128_t)f31 * f40;
  uint128_t o211 = o2 + (uint128_t)f31 * f41;
  uint128_t o31 = o3 + (uint128_t)f31 * f42;
  uint128_t o41 = o4 + (uint128_t)f31 * f43;
  uint128_t o02 = o01 + (uint128_t)f32 * tmp23;
  uint128_t o121 = o111 + (uint128_t)f32 * tmp24;
  uint128_t o221 = o211 + (uint128_t)f32 * f40;
  uint128_t o32 = o31 + (uint128_t)f32 * f41;
  uint128_t o42 = o41 + (uint128_t)f32 * f42;
  uint128_t o03 = o02 + (uint128_t)f33 * tmp22;
  uint128_t o131 = o121 + (uint128_t)f33 * tmp23;
  uint128_t o231 = o221 + (uint128_t)f33 * tmp24;
  uint128_t o33 = o32 + (uint128_t)f33 * f40;
  uint128_t o43 = o42 + (uint128_t)f33 * f41;
  uint128_t o04 = o03 + (uint128_t)f34 * tmp21;
  uint128_t o141 = o131 + (uint128_t)f34 * tmp22;
  uint128_t o241 = o231 + (uint128_t)f34 * tmp23;
  uint128_t o34 = o33 + (uint128_t)f34 * tmp24;
  uint128_t o44 = o43 + (uint128_t)f34 * f40;
  uint128_t tmp_w20 = o04;
  uint128_t tmp_w21 = o141;
  uint128_t tmp_w22 = o241;
  uint128_t tmp_w23 = o34;
  uint128_t tmp_w24 = o44;
  uint128_t l_ = tmp_w10 + (uint128_t)(u64)0U;
  u64 tmp00 = (uint64_t)l_ & (u64)0x7ffffffffffffU;
  u64 c00 = (uint64_t)(l_ >> (u32)51U);
  uint128_t l_0 = tmp_w11 + (uint128_t)c00;
  u64 tmp10 = (uint64_t)l_0 & (u64)0x7ffffffffffffU;
  u64 c10 = (uint64_t)(l_0 >> (u32)51U);
  uint128_t l_1 = tmp_w12 + (uint128_t)c10;
  u64 tmp20 = (uint64_t)l_1 & (u64)0x7ffffffffffffU;
  u64 c20 = (uint64_t)(l_1 >> (u32)51U);
  uint128_t l_2 = tmp_w13 + (uint128_t)c20;
  u64 tmp30 = (uint64_t)l_2 & (u64)0x7ffffffffffffU;
  u64 c30 = (uint64_t)(l_2 >> (u32)51U);
  uint128_t l_3 = tmp_w14 + (uint128_t)c30;
  u64 tmp40 = (uint64_t)l_3 & (u64)0x7ffffffffffffU;
  u64 c40 = (uint64_t)(l_3 >> (u32)51U);
  u64 l_4 = tmp00 + c40 * (u64)19U;
  u64 tmp0_ = l_4 & (u64)0x7ffffffffffffU;
  u64 c50 = l_4 >> (u32)51U;
  u64 o100 = tmp0_;
  u64 o112 = tmp10 + c50;
  u64 o122 = tmp20;
  u64 o132 = tmp30;
  u64 o142 = tmp40;
  uint128_t l_5 = tmp_w20 + (uint128_t)(u64)0U;
  u64 tmp0 = (uint64_t)l_5 & (u64)0x7ffffffffffffU;
  u64 c0 = (uint64_t)(l_5 >> (u32)51U);
  uint128_t l_6 = tmp_w21 + (uint128_t)c0;
  u64 tmp1 = (uint64_t)l_6 & (u64)0x7ffffffffffffU;
  u64 c1 = (uint64_t)(l_6 >> (u32)51U);
  uint128_t l_7 = tmp_w22 + (uint128_t)c1;
  u64 tmp2 = (uint64_t)l_7 & (u64)0x7ffffffffffffU;
  u64 c2 = (uint64_t)(l_7 >> (u32)51U);
  uint128_t l_8 = tmp_w23 + (uint128_t)c2;
  u64 tmp3 = (uint64_t)l_8 & (u64)0x7ffffffffffffU;
  u64 c3 = (uint64_t)(l_8 >> (u32)51U);
  uint128_t l_9 = tmp_w24 + (uint128_t)c3;
  u64 tmp4 = (uint64_t)l_9 & (u64)0x7ffffffffffffU;
  u64 c4 = (uint64_t)(l_9 >> (u32)51U);
  u64 l_10 = tmp0 + c4 * (u64)19U;
  u64 tmp0_0 = l_10 & (u64)0x7ffffffffffffU;
  u64 c5 = l_10 >> (u32)51U;
  u64 o200 = tmp0_0;
  u64 o212 = tmp1 + c5;
  u64 o222 = tmp2;
  u64 o232 = tmp3;
  u64 o242 = tmp4;
  u64 o10 = o100;
  u64 o11 = o112;
  u64 o12 = o122;
  u64 o13 = o132;
  u64 o14 = o142;
  u64 o20 = o200;
  u64 o21 = o212;
  u64 o22 = o222;
  u64 o23 = o232;
  u64 o24 = o242;
  out[0U] = o10;
  out[1U] = o11;
  out[2U] = o12;
  out[3U] = o13;
  out[4U] = o14;
  out[5U] = o20;
  out[6U] = o21;
  out[7U] = o22;
  out[8U] = o23;
  out[9U] = o24;
}

static inline void Hacl_Impl_Curve25519_Field51_fmul1(u64 *out, u64 *f1, u64 f2)
{
  u64 f10 = f1[0U];
  u64 f11 = f1[1U];
  u64 f12 = f1[2U];
  u64 f13 = f1[3U];
  u64 f14 = f1[4U];
  uint128_t tmp_w0 = (uint128_t)f2 * f10;
  uint128_t tmp_w1 = (uint128_t)f2 * f11;
  uint128_t tmp_w2 = (uint128_t)f2 * f12;
  uint128_t tmp_w3 = (uint128_t)f2 * f13;
  uint128_t tmp_w4 = (uint128_t)f2 * f14;
  uint128_t l_ = tmp_w0 + (uint128_t)(u64)0U;
  u64 tmp0 = (uint64_t)l_ & (u64)0x7ffffffffffffU;
  u64 c0 = (uint64_t)(l_ >> (u32)51U);
  uint128_t l_0 = tmp_w1 + (uint128_t)c0;
  u64 tmp1 = (uint64_t)l_0 & (u64)0x7ffffffffffffU;
  u64 c1 = (uint64_t)(l_0 >> (u32)51U);
  uint128_t l_1 = tmp_w2 + (uint128_t)c1;
  u64 tmp2 = (uint64_t)l_1 & (u64)0x7ffffffffffffU;
  u64 c2 = (uint64_t)(l_1 >> (u32)51U);
  uint128_t l_2 = tmp_w3 + (uint128_t)c2;
  u64 tmp3 = (uint64_t)l_2 & (u64)0x7ffffffffffffU;
  u64 c3 = (uint64_t)(l_2 >> (u32)51U);
  uint128_t l_3 = tmp_w4 + (uint128_t)c3;
  u64 tmp4 = (uint64_t)l_3 & (u64)0x7ffffffffffffU;
  u64 c4 = (uint64_t)(l_3 >> (u32)51U);
  u64 l_4 = tmp0 + c4 * (u64)19U;
  u64 tmp0_ = l_4 & (u64)0x7ffffffffffffU;
  u64 c5 = l_4 >> (u32)51U;
  u64 o0 = tmp0_;
  u64 o1 = tmp1 + c5;
  u64 o2 = tmp2;
  u64 o3 = tmp3;
  u64 o4 = tmp4;
  out[0U] = o0;
  out[1U] = o1;
  out[2U] = o2;
  out[3U] = o3;
  out[4U] = o4;
}

static inline void Hacl_Impl_Curve25519_Field51_fsqr(u64 *out, u64 *f, uint128_t *uu___)
{
  u64 f0 = f[0U];
  u64 f1 = f[1U];
  u64 f2 = f[2U];
  u64 f3 = f[3U];
  u64 f4 = f[4U];
  u64 d0 = (u64)2U * f0;
  u64 d1 = (u64)2U * f1;
  u64 d2 = (u64)38U * f2;
  u64 d3 = (u64)19U * f3;
  u64 d419 = (u64)19U * f4;
  u64 d4 = (u64)2U * d419;
  uint128_t s0 = (uint128_t)f0 * f0 + (uint128_t)d4 * f1 + (uint128_t)d2 * f3;
  uint128_t s1 = (uint128_t)d0 * f1 + (uint128_t)d4 * f2 + (uint128_t)d3 * f3;
  uint128_t s2 = (uint128_t)d0 * f2 + (uint128_t)f1 * f1 + (uint128_t)d4 * f3;
  uint128_t s3 = (uint128_t)d0 * f3 + (uint128_t)d1 * f2 + (uint128_t)f4 * d419;
  uint128_t s4 = (uint128_t)d0 * f4 + (uint128_t)d1 * f3 + (uint128_t)f2 * f2;
  uint128_t o00 = s0;
  uint128_t o10 = s1;
  uint128_t o20 = s2;
  uint128_t o30 = s3;
  uint128_t o40 = s4;
  uint128_t l_ = o00 + (uint128_t)(u64)0U;
  u64 tmp0 = (uint64_t)l_ & (u64)0x7ffffffffffffU;
  u64 c0 = (uint64_t)(l_ >> (u32)51U);
  uint128_t l_0 = o10 + (uint128_t)c0;
  u64 tmp1 = (uint64_t)l_0 & (u64)0x7ffffffffffffU;
  u64 c1 = (uint64_t)(l_0 >> (u32)51U);
  uint128_t l_1 = o20 + (uint128_t)c1;
  u64 tmp2 = (uint64_t)l_1 & (u64)0x7ffffffffffffU;
  u64 c2 = (uint64_t)(l_1 >> (u32)51U);
  uint128_t l_2 = o30 + (uint128_t)c2;
  u64 tmp3 = (uint64_t)l_2 & (u64)0x7ffffffffffffU;
  u64 c3 = (uint64_t)(l_2 >> (u32)51U);
  uint128_t l_3 = o40 + (uint128_t)c3;
  u64 tmp4 = (uint64_t)l_3 & (u64)0x7ffffffffffffU;
  u64 c4 = (uint64_t)(l_3 >> (u32)51U);
  u64 l_4 = tmp0 + c4 * (u64)19U;
  u64 tmp0_ = l_4 & (u64)0x7ffffffffffffU;
  u64 c5 = l_4 >> (u32)51U;
  u64 o0 = tmp0_;
  u64 o1 = tmp1 + c5;
  u64 o2 = tmp2;
  u64 o3 = tmp3;
  u64 o4 = tmp4;
  out[0U] = o0;
  out[1U] = o1;
  out[2U] = o2;
  out[3U] = o3;
  out[4U] = o4;
}

static inline void Hacl_Impl_Curve25519_Field51_fsqr2(u64 *out, u64 *f, uint128_t *uu___)
{
  u64 f10 = f[0U];
  u64 f11 = f[1U];
  u64 f12 = f[2U];
  u64 f13 = f[3U];
  u64 f14 = f[4U];
  u64 f20 = f[5U];
  u64 f21 = f[6U];
  u64 f22 = f[7U];
  u64 f23 = f[8U];
  u64 f24 = f[9U];
  u64 d00 = (u64)2U * f10;
  u64 d10 = (u64)2U * f11;
  u64 d20 = (u64)38U * f12;
  u64 d30 = (u64)19U * f13;
  u64 d4190 = (u64)19U * f14;
  u64 d40 = (u64)2U * d4190;
  uint128_t s00 = (uint128_t)f10 * f10 + (uint128_t)d40 * f11 + (uint128_t)d20 * f13;
  uint128_t s10 = (uint128_t)d00 * f11 + (uint128_t)d40 * f12 + (uint128_t)d30 * f13;
  uint128_t s20 = (uint128_t)d00 * f12 + (uint128_t)f11 * f11 + (uint128_t)d40 * f13;
  uint128_t s30 = (uint128_t)d00 * f13 + (uint128_t)d10 * f12 + (uint128_t)f14 * d4190;
  uint128_t s40 = (uint128_t)d00 * f14 + (uint128_t)d10 * f13 + (uint128_t)f12 * f12;
  uint128_t o100 = s00;
  uint128_t o110 = s10;
  uint128_t o120 = s20;
  uint128_t o130 = s30;
  uint128_t o140 = s40;
  u64 d0 = (u64)2U * f20;
  u64 d1 = (u64)2U * f21;
  u64 d2 = (u64)38U * f22;
  u64 d3 = (u64)19U * f23;
  u64 d419 = (u64)19U * f24;
  u64 d4 = (u64)2U * d419;
  uint128_t s0 = (uint128_t)f20 * f20 + (uint128_t)d4 * f21 + (uint128_t)d2 * f23;
  uint128_t s1 = (uint128_t)d0 * f21 + (uint128_t)d4 * f22 + (uint128_t)d3 * f23;
  uint128_t s2 = (uint128_t)d0 * f22 + (uint128_t)f21 * f21 + (uint128_t)d4 * f23;
  uint128_t s3 = (uint128_t)d0 * f23 + (uint128_t)d1 * f22 + (uint128_t)f24 * d419;
  uint128_t s4 = (uint128_t)d0 * f24 + (uint128_t)d1 * f23 + (uint128_t)f22 * f22;
  uint128_t o200 = s0;
  uint128_t o210 = s1;
  uint128_t o220 = s2;
  uint128_t o230 = s3;
  uint128_t o240 = s4;
  uint128_t l_ = o100 + (uint128_t)(u64)0U;
  u64 tmp00 = (uint64_t)l_ & (u64)0x7ffffffffffffU;
  u64 c00 = (uint64_t)(l_ >> (u32)51U);
  uint128_t l_0 = o110 + (uint128_t)c00;
  u64 tmp10 = (uint64_t)l_0 & (u64)0x7ffffffffffffU;
  u64 c10 = (uint64_t)(l_0 >> (u32)51U);
  uint128_t l_1 = o120 + (uint128_t)c10;
  u64 tmp20 = (uint64_t)l_1 & (u64)0x7ffffffffffffU;
  u64 c20 = (uint64_t)(l_1 >> (u32)51U);
  uint128_t l_2 = o130 + (uint128_t)c20;
  u64 tmp30 = (uint64_t)l_2 & (u64)0x7ffffffffffffU;
  u64 c30 = (uint64_t)(l_2 >> (u32)51U);
  uint128_t l_3 = o140 + (uint128_t)c30;
  u64 tmp40 = (uint64_t)l_3 & (u64)0x7ffffffffffffU;
  u64 c40 = (uint64_t)(l_3 >> (u32)51U);
  u64 l_4 = tmp00 + c40 * (u64)19U;
  u64 tmp0_ = l_4 & (u64)0x7ffffffffffffU;
  u64 c50 = l_4 >> (u32)51U;
  u64 o101 = tmp0_;
  u64 o111 = tmp10 + c50;
  u64 o121 = tmp20;
  u64 o131 = tmp30;
  u64 o141 = tmp40;
  uint128_t l_5 = o200 + (uint128_t)(u64)0U;
  u64 tmp0 = (uint64_t)l_5 & (u64)0x7ffffffffffffU;
  u64 c0 = (uint64_t)(l_5 >> (u32)51U);
  uint128_t l_6 = o210 + (uint128_t)c0;
  u64 tmp1 = (uint64_t)l_6 & (u64)0x7ffffffffffffU;
  u64 c1 = (uint64_t)(l_6 >> (u32)51U);
  uint128_t l_7 = o220 + (uint128_t)c1;
  u64 tmp2 = (uint64_t)l_7 & (u64)0x7ffffffffffffU;
  u64 c2 = (uint64_t)(l_7 >> (u32)51U);
  uint128_t l_8 = o230 + (uint128_t)c2;
  u64 tmp3 = (uint64_t)l_8 & (u64)0x7ffffffffffffU;
  u64 c3 = (uint64_t)(l_8 >> (u32)51U);
  uint128_t l_9 = o240 + (uint128_t)c3;
  u64 tmp4 = (uint64_t)l_9 & (u64)0x7ffffffffffffU;
  u64 c4 = (uint64_t)(l_9 >> (u32)51U);
  u64 l_10 = tmp0 + c4 * (u64)19U;
  u64 tmp0_0 = l_10 & (u64)0x7ffffffffffffU;
  u64 c5 = l_10 >> (u32)51U;
  u64 o201 = tmp0_0;
  u64 o211 = tmp1 + c5;
  u64 o221 = tmp2;
  u64 o231 = tmp3;
  u64 o241 = tmp4;
  u64 o10 = o101;
  u64 o11 = o111;
  u64 o12 = o121;
  u64 o13 = o131;
  u64 o14 = o141;
  u64 o20 = o201;
  u64 o21 = o211;
  u64 o22 = o221;
  u64 o23 = o231;
  u64 o24 = o241;
  out[0U] = o10;
  out[1U] = o11;
  out[2U] = o12;
  out[3U] = o13;
  out[4U] = o14;
  out[5U] = o20;
  out[6U] = o21;
  out[7U] = o22;
  out[8U] = o23;
  out[9U] = o24;
}

                                                            
static void point_add_and_double(u64 *q, u64 *p01_tmp1, uint128_t *tmp2)
{
  u64 *nq = p01_tmp1;
  u64 *nq_p1 = p01_tmp1 + (u32)10U;
  u64 *tmp1 = p01_tmp1 + (u32)20U;
  u64 *x1 = q;
  u64 *x2 = nq;
  u64 *z2 = nq + (u32)5U;
  u64 *z3 = nq_p1 + (u32)5U;
  u64 *a = tmp1;
  u64 *b = tmp1 + (u32)5U;
  u64 *ab = tmp1;
  u64 *dc = tmp1 + (u32)10U;
  u64 *x3;
  u64 *z31;
  u64 *d0;
  u64 *c0;
  u64 *a1;
  u64 *b1;
  u64 *d;
  u64 *c;
  u64 *ab1;
  u64 *dc1;
  Hacl_Impl_Curve25519_Field51_fadd(a, x2, z2);
  Hacl_Impl_Curve25519_Field51_fsub(b, x2, z2);
  x3 = nq_p1;
  z31 = nq_p1 + (u32)5U;
  d0 = dc;
  c0 = dc + (u32)5U;
  Hacl_Impl_Curve25519_Field51_fadd(c0, x3, z31);
  Hacl_Impl_Curve25519_Field51_fsub(d0, x3, z31);
  Hacl_Impl_Curve25519_Field51_fmul2(dc, dc, ab, tmp2);
  Hacl_Impl_Curve25519_Field51_fadd(x3, d0, c0);
  Hacl_Impl_Curve25519_Field51_fsub(z31, d0, c0);
  a1 = tmp1;
  b1 = tmp1 + (u32)5U;
  d = tmp1 + (u32)10U;
  c = tmp1 + (u32)15U;
  ab1 = tmp1;
  dc1 = tmp1 + (u32)10U;
  Hacl_Impl_Curve25519_Field51_fsqr2(dc1, ab1, tmp2);
  Hacl_Impl_Curve25519_Field51_fsqr2(nq_p1, nq_p1, tmp2);
  a1[0U] = c[0U];
  a1[1U] = c[1U];
  a1[2U] = c[2U];
  a1[3U] = c[3U];
  a1[4U] = c[4U];
  Hacl_Impl_Curve25519_Field51_fsub(c, d, c);
  Hacl_Impl_Curve25519_Field51_fmul1(b1, c, (u64)121665U);
  Hacl_Impl_Curve25519_Field51_fadd(b1, b1, d);
  Hacl_Impl_Curve25519_Field51_fmul2(nq, dc1, ab1, tmp2);
  Hacl_Impl_Curve25519_Field51_fmul(z3, z3, x1, tmp2);
}"))

(define (run-lexer l str-in-port)
  (define result (l str-in-port))
  (printf "~a\n" result)
  (if (not (eq? result 'EOF))
      (run-lexer l str-in-port)
      empty))

(define (run-parser p l str-in-port)
  (define run-lex (lambda () (l str-in-port)))
  (define result (p run-lex))
  result)

;; PARSER

(define (error-handler tok-ok? tok-name tok-value)
   (displayln
      (format "Error at ~a~a" 
              tok-name (if tok-value 
                           (format " of val ~a" tok-value)
                           ""))))

(define c-parser
  (parser
    (tokens op-tokens val-tokens)
    (error error-handler)
    (start program)
    (end EOF)
    (precs (left COMMA)
           (right =)
           (left OR)
           (left AND)
           (left PIPE)
           (left ^)
           (left &)
           (left == !=)
           (left < <= > >=)
           (left << >>)
           (left - +)
           (left * / %)
           (right !)
           (left APPEND CONCAT EQUALS SPLIT ITER_LENGTH
                 STR_LENGTH STON NTOS ITER_CONCAT)
           (left LPAREN))
    (grammar 
      (program
        [(fundef) (list $1)]
        [(fundef program) (cons $1 $2)])
      (fundef
        [(STATIC INLINE VOID VAR LPAREN argslist RPAREN LBRACE statement-list RBRACE) 
         (Function $4 $6 $9)]
        [(STATIC VOID VAR LPAREN argslist RPAREN LBRACE statement-list RBRACE) 
         (Function $3 $5 $8)])
      (type
        [(U8P) 'U8P]
        [(U16P) 'U16P]
        [(U32P) 'U32P]
        [(U64P) 'U64P]
        [(U128P) 'U128P]
        [(U8) 'U8]
        [(U16) 'U16]
        [(U32) 'U32]
        [(U64) 'U64]
        [(U128) 'U128])
      (arg
        [(type VAR) (Variable $1 $2)])
      (argslist
        [(arg) (list $1)]
        [(arg COMMA argslist) (cons $1 $3)])
      (expr
        [(LPAREN expr RPAREN) $2]
        [(LPAREN type RPAREN expr) (Cast $2 $4)]
        [(expr LSQUARE expr RSQUARE) (Index $1 $3)]
        [(expr < expr) (Lt $1 $3)]
        [(expr > expr) (Gt $1 $3)]
        [(expr <= expr) (Lte $1 $3)]
        [(expr >= expr) (Gte $1 $3)]
        [(expr == expr) (Eq $1 $3)]
        [(expr OR expr) (Or $1 $3)]
        [(expr AND expr) (And $1 $3)]
        [(expr + expr) (Add $1 $3)]
        [(expr - expr) (Sub $1 $3)]
        [(expr * expr) (Mul $1 $3)]
        [(expr / expr) (Div $1 $3)]
        [(expr ^ expr) (Bvxor $1 $3)]
        [(expr PIPE expr) (Bvor $1 $3)]
        [(expr & expr) (Bvand $1 $3)]
        [(expr >> expr) (LShift $1 $3)]
        [(expr << expr) (RShift $1 $3)]
        [(VAR) (Variable #f $1)]
        [(NUM) (Value $1)]
        [(NUM UNSIGNED) (Value $1)])
      (assn
        [(type VAR EQUALS expr) (Put (Variable $1 $2) $4)]
        [(expr EQUALS expr) (Put $1 $3)])
      (func-call-args
        [(expr) (list $1)]
        [(expr COMMA func-call-args) (cons $1 $3)])
      (statement
        [(assn SEMICOLON) $1]
        [(VAR LPAREN func-call-args RPAREN SEMICOLON)
         (ApplyFunction $1 $3)]
        [(type VAR SEMICOLON) (DeclVar (Variable $1 $2))])
      (statement-list
        [(statement) (list $1)]
        [(statement statement-list) (cons $1 $2)]))))

; interpreters

(define (lookup var env)
  (define identifier (if (Variable? var) (Variable-name var) var))
  (define found (assoc identifier env))
  (if found
      (cdr found)
      (raise (format "Couldn't find var ~a in env" var))))

(define (find-func-by-name name funcs)
  (if (empty? funcs)
      (raise (format "Couldn't find func with name ~a in program" name))
      (let* ([cur (car funcs)])
        (if (string=? (Function-name cur) name)
            cur
            (find-func-by-name name (cdr funcs))))))

(define (writer-eval-expr expr env)
  (match expr
    [(Variable type name) (lookup name env)]
    [(Value x) x]
    [(? symbol?) expr] ; probably a type
    [(Index place idx) (format "~a[~a]" 
                               (writer-eval-expr place env)
                               (writer-eval-expr idx env))]
    [(Cast type expr) (format "((~a)~a)" 
                              (writer-eval-expr type env) 
                              (writer-eval-expr expr env))]
    [(Mul left right) 
     (begin
       (define left-eval (writer-eval-expr left env))
       (define right-eval (writer-eval-expr right env))
       (printf "need to check that 1 <> ~a\n" left-eval)
       (printf "need to check that 0 <> ~a\n" left-eval)
       (printf "need to check that 1 <> ~a\n" right-eval)
       (printf "need to check that 0 <> ~a\n" right-eval)
	   (format "(~a * ~a)" left-eval right-eval))]
    [(Div left right)
     (begin 
       (define left-eval (writer-eval-expr left env))
       (define right-eval (writer-eval-expr right env))
       (printf "need to check that 1 <> ~a\n" right-eval)
       (printf "need to check that 0 <> ~a\n" left-eval)
       (format "(~a / ~a)" left-eval right-eval))]
    [(Add left right)
     (begin 
       (define left-eval (writer-eval-expr left env))
       (define right-eval (writer-eval-expr right env))
       (printf "need to check that 0 <> ~a\n" right-eval)
       (printf "need to check that 0 <> ~a\n" left-eval)
       (format "(~a + ~a)" left-eval right-eval))]
    [(Sub left right)
     (begin 
       (define left-eval (writer-eval-expr left env))
       (define right-eval (writer-eval-expr right env))
       (printf "need to check that 0 <> ~a\n" left-eval)
       (format "(~a - ~a)" left-eval right-eval))]
    [(LShift left right) 
     (begin 
       (define left-eval (writer-eval-expr left env))
       (define right-eval (writer-eval-expr right env))
       (printf "need to check that 0 <> ~a\n" right-eval)
       (printf "need to check that 0 <> ~a\n" left-eval)
       (format "(~a << ~a)" left-eval right-eval))]
    [(RShift left right) 
     (begin 
       (define left-eval (writer-eval-expr left env))
       (define right-eval (writer-eval-expr right env))
       (printf "need to check that 0 <> ~a\n" right-eval)
       (printf "need to check that 0 <> ~a\n" left-eval)
       (format "(~a >> ~a)" left-eval right-eval))]
    [(Bvand left right)
     (begin 
       (define left-eval (writer-eval-expr left env))
       (define right-eval (writer-eval-expr right env))
       (printf "need to check that 0 <> ~a\n" right-eval)
       (printf "need to check that 0 <> ~a\n" left-eval)
       (format "(~a & ~a)" left-eval right-eval))]
    [(Bvor left right) 
     (begin 
       (define left-eval (writer-eval-expr left env))
       (define right-eval (writer-eval-expr right env))
       (printf "need to check that 0 <> ~a\n" right-eval)
       (printf "need to check that 0 <> ~a\n" left-eval)
       (format "(~a | ~a)" left-eval right-eval))]
    [(Bvxor left right) 
     (begin 
       (define left-eval (writer-eval-expr left env))
       (define right-eval (writer-eval-expr right env))
       (printf "need to check that 0 <> ~a\n" right-eval)
       (printf "need to check that 0 <> ~a\n" left-eval)
       (format "(~a ^ ~a)" left-eval right-eval))]))

(define (writer fundef prog [env empty] [default-pop-args #t])
  (define insns (Function-insn-list fundef))
  
  (define (add-to-env! var val)
    (set! env (cons (cons var val) env)))
  
  ; populate env with arguments
  (when default-pop-args
    (for ([arg (Function-arg-list fundef)])
      (match-let ([(Variable type name) arg]) 
        (add-to-env! name name))))
  
  ; interpret the fn
  (for ([insn insns])
    (match insn
      [(Put (Variable type name) rhs) 
       (begin
       	(define evald-rhs (writer-eval-expr rhs env))
        (add-to-env! name evald-rhs))]
      [(Put lhs rhs) 
       (begin
       	(define evald-rhs (writer-eval-expr rhs env))
        (define evald-lhs (writer-eval-expr lhs env))
        (add-to-env! evald-lhs evald-rhs))]
      [(DeclVar (Variable type name))
       (begin
       	(add-to-env! name name))]
      [(ApplyFunction name calling-arg-list)
       (begin
         ; get the target function and its args
         (define func-to-call (find-func-by-name name (Program-func-list prog)))
         (define to-call-args (Function-arg-list func-to-call))
         
         ; check arities match of call site and func definition
         (unless (eq? (length calling-arg-list) (length to-call-args))
           (raise (format "Must provide same num of arguments to ~a\n" func-to-call)))
         
         ; save environment for evaluating provided arguments (at call-site)
         (define saved-env env)
         
         ; replace the function def args with provided arguments (from call-site)
         (for ([to-call-arg to-call-args]
               [calling-arg calling-arg-list])
           (match-let ([(Variable type name) to-call-arg])
             (add-to-env! name (writer-eval-expr calling-arg saved-env))))
         
         ; evaluate the function
         (writer func-to-call env #f))]))
  env)

(define (writer-program prog target-fn-name)
  (define entrypoint (find-func-by-name target-fn-name (Program-func-list prog)))
  (writer entrypoint prog))

; running stuff

;(run-lexer c-lexer in)
(define parsed-result (run-parser c-parser c-lexer in))
(define parsed-program (Program parsed-result))
(displayln parsed-program)
(define final-env (writer-program parsed-program "point_add_and_double"))
;(define to-print (cdr (assoc "o10" final-env)))
;(displayln to-print)
;(displayln final-env)