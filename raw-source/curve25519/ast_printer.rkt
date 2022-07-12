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

(define in (open-input-string "static void point_add_and_double(u64 *q, u64 *p01_tmp1, uint128_t *tmp2)
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
       (format "(~a << ~a)" left-eval right-eval))]
    [(RShift left right) 
     (begin 
       (define left-eval (writer-eval-expr left env))
       (define right-eval (writer-eval-expr right env))
       (printf "need to check that 0 <> ~a\n" right-eval)
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
;(define final-env (writer parsed-result))
;(define to-print (cdr (assoc "o10" final-env)))
;(displayln to-print)