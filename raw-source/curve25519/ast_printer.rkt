#lang racket

(struct Statement () #:transparent)
(struct Store Statement (ptr idx value) #:transparent)
(struct Load Statement (value ptr idx) #:transparent)
(struct Get Statement (var) #:transparent)
(struct Put Statement (var value) #:transparent)

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
	      (:+ (:or digit "-")))))

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

(define in (open-input-string "static inline void
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
  (match result
    [(Function name args insns) (begin
                                  (printf "Function ~a (~a):\n" name args)
                                  (for ([insn insns])
                                    (printf "\t~a\n" insn)))])
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
    (start fundef)
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
      (fundef
        [(STATIC INLINE VOID VAR LPAREN argslist RPAREN LBRACE statement-list RBRACE) 
         (Function $4 $6 $9)])
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
      (statement
        [(assn SEMICOLON) $1])
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

(define (writer-eval-expr expr env)
  (match expr
    [(Variable type name) (lookup name env)]
    [(Value x) x]
    [(? symbol?) expr] ; probably a type
    [(Index place idx) (format "(~a[~a])" 
                               (writer-eval-expr place env)
                               (writer-eval-expr idx env))]
    [(Cast type expr) (format "((~a)~a)" 
                              (writer-eval-expr type env) 
                              (writer-eval-expr expr env))]
    [(Mul left right) (format "(~a * ~a)"
                              (writer-eval-expr left env)
                              (writer-eval-expr right env))]
    [(Div left right) (format "(~a / ~a)"
                              (writer-eval-expr left env)
                              (writer-eval-expr right env))]
    [(Add left right) (format "(~a + ~a)"
                              (writer-eval-expr left env)
                              (writer-eval-expr right env))]
    [(Sub left right) (format "(~a - ~a)"
                              (writer-eval-expr left env)
                              (writer-eval-expr right env))]
    [(LShift left right) (format "(~a << ~a)"
                              (writer-eval-expr left env)
                              (writer-eval-expr right env))]
    [(RShift left right) (format "(~a >> ~a)"
                              (writer-eval-expr left env)
                              (writer-eval-expr right env))]
    [(Bvand left right) (format "(~a & ~a)"
                              (writer-eval-expr left env)
                              (writer-eval-expr right env))]
    [(Bvor left right) (format "(~a | ~a)"
                              (writer-eval-expr left env)
                              (writer-eval-expr right env))]
    [(Bvxor left right) (format "(~a ^ ~a)"
                              (writer-eval-expr left env)
                              (writer-eval-expr right env))]))

(define (writer fundef)
  (define env empty)
  (define insns (Function-insn-list fundef))
  
  (define (add-to-env! var val)
    (set! env (cons (cons var val) env)))
  
  ; populate env with arguments
  (for ([arg (Function-arg-list fundef)])
    (match-let ([(Variable type name) arg]) 
      (add-to-env! name name)))
  
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
        (add-to-env! evald-lhs evald-rhs))]))
  env)

; running stuff

;(run-lexer c-lexer in)
(define parsed-result (run-parser c-parser c-lexer in))
(define final-env (writer parsed-result))
(define to-print (cdr (assoc "o10" final-env)))
(displayln to-print)