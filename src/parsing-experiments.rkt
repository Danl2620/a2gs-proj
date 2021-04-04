#lang racket/base

(require
  parser-tools/yacc
  parser-tools/lex
  (prefix-in : parser-tools/lex-sre)
  data/monad
  data/applicative
  megaparsack
  megaparsack/parser-tools/lex
  )


;;(define-tokens simple [IDENTIFIER NUMBER])
;;(define-empty-tokens simple* [OPEN-PAREN CLOSE-PAREN COMMA])

(define-tokens value-tokens (NUMBER IDENTIFIER))
(define-empty-tokens op-tokens (newline = OP CP OB CB CONST
                                        STRUCT ENUM UNION
                                        COLON SEMICOLON
                                        COMMA EQUALS
                                        DOT
                                        + - * / ^ EOF NEG

                                        f32 f64
                                        i8 i16 i32 i64
                                        u8 u16 u32 u64
                                        BOOL))



(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (id-punctuation (:or #\_ #\^))

  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  (digit (:/ "0" "9"))

  (numeric-type (:or "f32" "f64"
                     "i8" "i16" "i32" "i64"
                     "u8" "u16" "u32" "u64"
                     )))


(define zig-lexer
  (lexer-src-pos
   [#\( (token-OP)]
   [#\) (token-CP)]
   [#\, (token-COMMA)]
   [#\{ 'OB]
   [#\} 'CB]
   ["const" 'CONST]
   ["struct" 'STRUCT]
   ["enum" 'ENUM]
   ["union" 'UNION]
   [#\: 'COLON]
   [#\; 'SEMICOLON]
   [#\, 'COMMA]
   [#\= 'EQUALS]
   [#\. 'DOT]
   [(:or "=" "+" "-" "*" "/" "%" "&&"      "==" "!=" ">=" "<=" ">" "<") (string->symbol lexeme)]
  
   [numeric-type (string->symbol lexeme)]
   ["bool" 'BOOL]
   
   [(:+ (:or upper-letter lower-letter id-punctuation))
    (token-IDENTIFIER (string->symbol lexeme))]
   [(:+ digit)
    (token-NUMBER (string->number lexeme))]

   [(:: (:+ digit) #\. (:* digit)) (token-NUMBER (string->number lexeme))]
  
   [(:or whitespace blank iso-control) (void)]
   [(eof) eof]))


(define (lex-zig str)
  (define in (open-input-string str))
  (port-count-lines! in)
  (let loop ([v (zig-lexer in)])
    (cond [(void? (position-token-token v)) (loop (zig-lexer in))]
          [(eof-object? (position-token-token v)) '()]
          [else (cons v (loop (zig-lexer in)))]
          )))


;;(lex-simple "f(1, g(3, 4))")

; some wrappers around tokens that use syntax/p
(define number/p (syntax/p (token/p 'NUMBER)))
(define identifier/p (syntax/p (token/p 'IDENTIFIER)))


; a simple function invokation
(define funcall/p
  (syntax/p
   (do [func <- identifier/p]
     (token/p 'OP)
     [args <- (many/p expression/p #:sep (token/p 'COMMA))]
     (token/p 'CP)
     (pure (list* func args))
     )))

(define lhs/p
  (syntax/p
   (do [prot <- (or/p (do (try/p (token/p 'CONST)) (pure 'const))
                      (do void/p (pure 'mut)))]
     (name <- identifier/p)
     (pure (list 'lhs prot name))
     )))

(define type/p
  (syntax/p
   (or/p (do (token/p '*) [type <- type/p] (pure (list 'pointer type)))
         (do (try/p (token/p 'f32)) (pure 'f32))
         (do (token/p 'f64) (pure 'f64))
         (do (token/p 'i8) (pure 'i8))
         (do (token/p 'i16) (pure 'i16))
         (do (token/p 'i32) (pure 'i32))
         (do (token/p 'i64) (pure 'i64))
         (do (token/p 'u8) (pure 'u8))
         (do (token/p 'u16) (pure 'u16))
         (do (token/p 'u32) (pure 'u32))
         (do (token/p 'u64) (pure 'u64))
         (do (token/p 'BOOL) (pure 'bool))
         (do [name <- identifier/p] (pure (list 'type-ref name)))
         )))

(define field/p
  (syntax/p
   (do [name <- identifier/p]
     (token/p 'COLON)
     [type <- type/p]
     (pure (list name type))
     )))

(define struct/p
  (syntax/p
   (do (token/p 'STRUCT)
     (token/p 'OB)
     [fields <- (many/p field/p #:sep (token/p 'COMMA))]
     (token/p 'CB)
     (token/p 'SEMICOLON)
     (pure (list* 'struct fields))
     )))

(define enum/p
  (syntax/p
   (do (token/p 'ENUM)
     (token/p 'OB)
     [entries <- (many/p identifier/p #:sep (token/p 'COMMA))]
     (token/p 'CB)
     (token/p 'SEMICOLON)
     (pure (list 'enum entries))
     )))

(define union/p
  (syntax/p
   (do (token/p 'UNION)
     (token/p 'OB)
     [entries <- (many/p field/p #:sep (token/p 'COMMA))]
     (token/p 'CB)
     (token/p 'SEMICOLON)
     (pure (list 'union entries))
     )))

(define field-instance/p
  (syntax/p
   (do (token/p 'DOT)
     [name <- identifier/p]
     (token/p 'EQUALS)
     [value <- number/p]
     (pure (list 'assign name value))
     )))

(define struct-instance/p
  (syntax/p
   (do [type <- identifier/p]
     (token/p 'OB)
     [fields <- (many/p field-instance/p #:sep (token/p 'COMMA))]
     (token/p 'CB)
     (token/p 'SEMICOLON)
     (pure (list 'struct-instance (list 'type type) fields))
     )))

(define definition/p
  (syntax/p
   (do [name <- lhs/p]
     (token/p 'EQUALS)
     [value <- (or/p struct/p enum/p union/p struct-instance/p)]
     (pure (list 'assign name value))
     )))

;;(define function/p
;;)

     

; an expression can be a number or a function invokation
(define expression/p
  (or/p number/p
        ;;funcall/p
        definition/p))

(define examples
  (list
   ;"f(1, g(3, 4))"
   "1"
   "2.3"
   ;"sin(0.5)"
   ;;"x=1\n(x + 2 * 3.12) - (1+2)*3"
   "const Point = struct { x: i64, y: f32 };"
   "MPoint = struct { x: i16, y: f32 };"
   "const Type = enum { ok, not_ok };"
   "const PayloadA = union { x: f32, y: i64 };"
   "const PayloadB = union { x: f32, y: *i64 };"
   "const UnionType = union { x: f32, y: i64, p: Point };"

   "const Size = struct {
  w: i16,
  h: i16
};"

   "const RB = struct {
  start: *u8,
  size: Size,
  stride: i16,
  bpp: i8
};"


   "const p = Point { .x = 1234, .y = 0.34 };"

   ;; TODO: parse hex numbers, generalize value expressions
;"const shr-screen-320 = RB {
;  .start = #xe12000,
;  .size = Size { .w =320, .h = 200 },
;  .bpp = 4
;};
;"

   
   ))

(for/list ([str examples])
  (parse-result! (parse-tokens expression/p (lex-zig str)))
  )