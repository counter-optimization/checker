#lang racket

(struct Statement () #:transparent)
(struct Store Statement (ptr idx value) #:transparent)
(struct Load Statement (value ptr idx) #:transparent)
(struct Get Statement (var) #:transparent)
(struct Put Statement (var value) #:transparent)

(struct Binop (op1 op2) #:transparent)
(struct Add Binop () #:transparent)
(struct Sub Binop () #:transparent)
(struct Mul Binop () #:transparent)
(struct Div Binop () #:transparent)
(struct Bvand Binop () #:transparent)
(struct Bvxor Binop () #:transparent)
(struct Bvor Binop () #:transparent)
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
  u64 f13 = f1[3U];"))

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
           (left == !=)
           (left < <= > >=)
           (left - +)
           (left * /)
           (right !)
           (left APPEND CONCAT EQUALS SPLIT ITER_LENGTH
                 STR_LENGTH STON NTOS ITER_CONCAT)
           (left LPAREN))
    (grammar 
      
      (fundef
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
        [(expr LSQUARE expr RSQUARE) (Index $1 $2)]
        [(VAR) $1]
        [(NUM) $1])
      
      (assn
        [(type VAR EQUALS expr) (Put (Variable $1 $2) $4)]
        [(expr EQUALS expr) (Put $1 $3)])
      
      (statement
        [(assn) $1])
      
      (statement-list
        [(statement) (list $1)]
        [(statement statement-list) (cons $1 $2)]))))

;(run-lexer c-lexer in)
(run-parser c-parser c-lexer in)