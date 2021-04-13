#lang curly-fn racket/base

(require
  racket/string
  parser-tools/yacc
  parser-tools/lex
  (prefix-in : parser-tools/lex-sre)
  data/monad
  data/applicative
  megaparsack
  megaparsack/text
  megaparsack/parser-tools/lex
  )


(define-tokens value-tokens
  (INT-LITERAL
   FLOAT-LITERAL
   IDENTIFIER
   STRING
   INT-CUSTOM-PRECISION))
(define-empty-tokens op-tokens (newline =
                                        OP CP
                                        OB CB
                                        OSB CSB
                                        CONST
                                        STRUCT ENUM UNION FN
                                        IF FOR RETURN
                                        COLON SEMICOLON
                                        COMMA EQUALS
                                        DOT
                                        + - * / ^ EOF NEG
                                        < > <= >= !=

                                        f32 f64
                                        i8 i16 i32 i64
                                        u8 u16 u32 u64
                                        BOOL

                                        ARROW))



(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (hex-digit (char-set "abcdefABCDEF"))
  (id-punctuation (:or #\_ #\^))

  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  (digit (:/ "0" "9"))

  (string0 (:* (:~ #\")))
  
  (numeric-type (:or "f32" "f64"
                     "i8" "i16" "i32" "i64"
                     "u8" "u16" "u32" "u64"
                     ))
  [comment (:: #\/ #\/ (:* (:~ #\newline)) #\newline)]
  )


(define zig-lexer
  (lexer-src-pos
   [#\( (token-OP)]
   [#\) (token-CP)]
   [#\, (token-COMMA)]
   [#\{ 'OB]
   [#\} 'CB]
   (#\[ 'OSB)
   (#\] 'CSB)
   ["const" 'CONST]
   ["struct" 'STRUCT]
   ["enum" 'ENUM]
   ["union" 'UNION]
   ("fn" 'FN)
   ("if" 'IF)
   ("for" 'FOR)
   ("return" 'RETURN)
   [#\: 'COLON]
   [#\; 'SEMICOLON]
   [#\, 'COMMA]
   [#\= 'EQUALS]
   [#\. 'DOT]

   (#\< '<)
   (#\> '>)
   ((:: #\< #\=) '<=)
   ((:: #\> #\=) '>=)
   ((:: #\! #\=) '!=)
   
   ("->" 'ARROW)
   ((:: #\" string0 #\") (token-STRING (string-trim lexeme "\"")))
   [(:or "=" "+" "-" "*" "/" "%" "&&" "==" "!=" ">=" "<=" ">" "<") (string->symbol lexeme)]
  
   [numeric-type (string->symbol lexeme)]

   ;; arbitrary bit integers
   [(:: #\u (:+ digit)) (token-INT-CUSTOM-PRECISION (string->symbol lexeme))]
   [(:: #\i (:+ digit)) (token-INT-CUSTOM-PRECISION (string->symbol lexeme))]
   
   ["bool" 'BOOL]
   
   [(:: (:or upper-letter lower-letter)
        (:* (:or upper-letter lower-letter id-punctuation digit)))
    (token-IDENTIFIER (string->symbol lexeme))]
   
   [(:: (:+ digit) #\. (:* digit))
    (token-FLOAT-LITERAL (string->number lexeme))]

   [(:+ digit)
    (token-INT-LITERAL (string->number lexeme))]

   [(:: #\0 #\x (:+ (:or digit hex-digit)))
    (token-INT-LITERAL (string->number (substring lexeme 2) 16))]

   [(:or whitespace blank iso-control comment) (void)]
   [(eof) eof]))

(define (lex-zig in)
  (port-count-lines! in)
  (let loop ([v (zig-lexer in)])
    (cond [(void? (position-token-token v)) (loop (zig-lexer in))]
          [(eof-object? (position-token-token v)) '()]
          [else (cons v (loop (zig-lexer in)))]
          )))


(define (lex-zig-str str)
  (lex-zig (open-input-string str))
  )

(define (lex-zig-file path)
  (with-input-from-file path
    (lambda ()
      (lex-zig (current-input-port))
      )))


;;(lex-simple "f(1, g(3, 4))")

;; some wrappers around tokens that use syntax/p

(define number/p
  (syntax/p
   (or/p (token/p 'INT-LITERAL)
         (token/p 'FLOAT-LITERAL))))

(define identifier/p
  (syntax/p (token/p 'IDENTIFIER)))

(define operator/p
  (or/p (token/p '<)
        (token/p '>)
        (token/p '<=)
        (token/p '>=)
        (token/p '!=)
        ))




; a simple function invokation
(define funcall/p
  (syntax/p
   (do [func <- identifier/p]
     (token/p 'OP)
     [args <- (many/p expression/p #:sep (token/p 'COMMA))]
     (token/p 'CP)
     (pure (list* func args))
     )))

(struct ast-numeric-type
  (signed?
   size
   integer?
   )
  #:methods gen:custom-write
  [(define (write-proc e port mode)
     (write-string "#<ast-type-number:" port)
     (write-string (or (and (ast-numeric-type-integer? e)
                            (or (and (ast-numeric-type-signed? e) "i")
                                "u")
                            )
                       "f") port)
     (write-string (number->string (ast-numeric-type-size e)) port)
     (write-string ">" port)
     )])

(define-values (dummy i8 i16 i32 i64
                      u8 u16 u32 u64
                      f32)

  (values
   #f
   (ast-numeric-type #t 8 #t)
   (ast-numeric-type #t 16 #t)
   (ast-numeric-type #t 32 #t)
   (ast-numeric-type #t 64 #t)

   (ast-numeric-type #f 8 #t)
   (ast-numeric-type #f 16 #t)
   (ast-numeric-type #f 32 #t)
   (ast-numeric-type #f 64 #t)

   (ast-numeric-type #t 32 #f)
   ))

(define type/p
  (or/p (do (token/p '*) [type <- type/p] (pure (list 'pointer type)))
        (do (try/p (token/p 'f32)) (pure f32))
        ;;(do (token/p 'f64) (pure f64))
        (do (token/p 'i8) (pure i8))
        (do (token/p 'i16) (pure i16))
        (do (token/p 'i32) (pure i32))
        (do (token/p 'i64) (pure i64))
        (do (token/p 'u8)  (pure u8))
        (do (token/p 'u16) (pure u16))
        (do (token/p 'u32) (pure u32))
        (do (token/p 'u64) (pure u64))
        (do [n <- (token/p 'INT-CUSTOM-PRECISION)] (pure n))
        (do (token/p 'BOOL) (pure 'bool))
        (do [name <- identifier/p] (pure (list 'type-ref name)))
        ))

(struct ast-lhs
  (mutable
   name
   type
   )
  #:transparent
  )

(define lhs/p
  (do [mut <- (or/p (do (try/p (token/p 'CONST)) (pure #f))
                    (do void/p (pure #t)))]
    (name <- identifier/p)
    [type <- (or/p (do (token/p 'COLON)
                     [type <- type/p]
                     (pure type))
                   (do void/p (pure #f)))]
    (pure (ast-lhs mut name type))
    ))

(struct ast-struct-field (name type)
  #:transparent)

(define field/p
  (do [name <- identifier/p]
    (token/p 'COLON)
    [type <- type/p]
    (pure (ast-struct-field name type))
    ))

(struct ast-struct
  (fields)
  #:transparent)

(define struct/p
  (do (token/p 'STRUCT)
    (token/p 'OB)
    [fields <- (many/p field/p #:sep (token/p 'COMMA))]
    (token/p 'CB)
    (pure (ast-struct fields))
    ))

(define enum/p
  (syntax/p
   (do (token/p 'ENUM)
     (token/p 'OB)
     [entries <- (many/p identifier/p #:sep (token/p 'COMMA))]
     (token/p 'CB)
     (pure (list 'enum entries))
     )))

(define union/p
  (syntax/p
   (do (token/p 'UNION)
     (token/p 'OB)
     [entries <- (many/p field/p #:sep (token/p 'COMMA))]
     (token/p 'CB)
     (pure (list 'union entries))
     )))

(struct ast-field-initializer
  (name
   value)
  #:transparent)

(define field-instance/p
  (do (token/p 'DOT)
    [name <- identifier/p]
    (token/p 'EQUALS)
    [value <- (or/p number/p struct-instance/p)]
    (pure (ast-field-initializer name value))
    ))

(struct ast-struct-instance
  (type
   field-initializers)
  #:transparent
  )

(define struct-instance/p
  (do [type <- identifier/p]
    (token/p 'OB)
    [field-initializers <- (many/p field-instance/p #:sep (token/p 'COMMA))]
    (token/p 'CB)
    (pure (ast-struct-instance type field-initializers))
    ))

(define value/p
  (or/p (syntax/p number/p)
        (syntax/p identifier/p)
        (syntax/p (token/p 'STRING))
        struct-instance/p
        ))

(struct ast-definition
  (lhs
   value
   )
  #:transparent
  )

(define definition/p
  (do [name <- lhs/p]
    (token/p 'EQUALS)
    [value <- (or/p expression/p struct/p enum/p union/p value/p)]
    (token/p 'SEMICOLON)
    (pure (ast-definition name value))
    ))

(struct ast-function
  (name params type body)
  #:transparent)

(define function/p
  (do (token/p 'FN)
    [name <- identifier/p]
    (token/p 'OP)
    [params <- (many/p field/p #:sep (token/p 'COMMA))]
    (token/p 'CP)
    [type <- (or/p (do (token/p 'ARROW)
                     [type <- type/p]
                     (pure type))
                   (do void/p (pure #f))
                   )]
    (token/p 'OB)
    [body <- (many/p expression/p)]
    (token/p 'CB)
    (pure (ast-function name params type #f))
    ))

(define top-level-definition/p
  (or/p definition/p
        function/p
        ))

(define boolean-expression/p
  (or/p (do value/p
          operator/p
          value/p)
        identifier/p
        ))

(struct ast-if-expression
  (condition body)
  #:transparent
  )

(define if-expression/p
  (do (token/p 'IF)
    (token/p 'OP)
    [condition <- boolean-expression/p]
    (token/p 'CP)
    (token/p 'OB)
    [body <- (many/p expression/p)]
    (token/p 'CB)
    (pure (ast-if-expression condition body))
    ))

(struct ast-for-expression
  (expr body)
  #:transparent
  )

(define for-expression/p
  (do (token/p 'FOR)
    (token/p 'OP)
    [expr <- identifier/p]
    (token/p 'CP)
    (token/p 'OB)
    [body <- (many/p expression/p)]
    (token/p 'CB)
     
    (pure (ast-for-expression expr body))
    ))

(define return-expression/p
  (do (token/p 'RETURN)
    [expr <- expression/p]
    (token/p 'SEMICOLON)
    ))


; an expression can be a number or a function invokation
(define expression/p
  (or/p (try/p funcall/p)
        (try/p if-expression/p)
        (try/p for-expression/p)
        (try/p return-expression/p)
        (try/p definition/p)
        (syntax/p number/p)
        (syntax/p identifier/p)
        ))

(struct ast-module
  (defs)
  #:transparent
  )

(define body/p
  (do [defs <- (many/p top-level-definition/p)]
    (pure (ast-module defs))
    ))

(module+ test
  (define examples
    (list
     "const intval = 1;"
     "const floatval = 2.3;"
     "const hexval: i32 = 0xa7a;"
     "const Distance = 12.34;"
     "const one: i32 = 1;"
     "const str = \"asdf\";"
     "const Point = struct { x: i64, y: f32 };"
     "MPoint = struct { x: i16, y: f32 };"
     "const Type = enum { ok, not_ok };"
     "const PayloadA = union { x: f32, y: i64 };"
     "const PayloadB = union { x: f32, y: *i64 };"
     "const UnionType = union { x: f32, y: i64, p: Point };"

     "const Size = struct {
  w: i16, h: i16
};"

     "const RB = struct {
  start: *u8,
  size: Size,
  stride: i16,
  bpp: i8
};"


     "const p = Point { .x = 1234, .y = 0.34 };"

     ;; TODO:
     ;;   + parse hex numbers,
     ;;   - generalize value expressions more
     ;;   - function definitions
     "const shr_screen_320 = RB {
     .start = 0x12000,
     .size = Size { .w =320, .h = 200 },
     .bpp = 4
   };
   "

   
     ))

  (require racket/pretty)
  #;(pretty-print
     (for/list ([str examples])
       (parse-result! (parse-tokens expression/p (lex-zig-str str)))
       ))

  (pretty-print
   (parse-result! (parse-tokens body/p (lex-zig-file "test.z") "test.z")))
  )