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
  uint128_t o44 = o43 + (uint128_t)f34 * f40
  uint128_t tmp_w24 = o44;"))

(define (run-lexer l str-in-port)
  (define result (l str-in-port))
  (printf "~a\n" result)
  (if (not (eq? result 'EOF))
      (run-lexer l str-in-port)
      empty))

(define (run-parser p l str-in-port)
  (define run-lex (lambda () (l str-in-port)))
  (displayln (p run-lex)))

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
        [(STATIC INLINE VOID VAR LPAREN argslist RPAREN LBRACE statement-list) 
         (begin (displayln $4) (Function $4 $6 $9))]
        [(STATIC INLINE VOID VAR LPAREN argslist RPAREN LBRACE statement-list RBRACE) 
         (begin (displayln $4) (Function $4 '() '()))])
      
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
        [(LPAREN type RPAREN expr) (Cast $2 $4)]
        [(VAR) $1]
        [(NUM) $1]
        [(NUM UNSIGNED) $1])
      
      (assn
        [(type VAR EQUALS expr) (Put (Variable $1 $2) $4)]
        [(expr EQUALS expr) (Put $1 $3)])
      
      (statement
        [(assn SEMICOLON) $1])
      
      (statement-list
        [(statement) (list $1)]
        [(statement statement-list) (cons $1 $2)]))))

(run-lexer c-lexer in)
;(run-parser c-parser c-lexer in)