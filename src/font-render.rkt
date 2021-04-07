#lang racket/base

(require
  (for-syntax
   racket/base
   syntax/parse
   ))


(define u8 'u8)
(define u16 'u16)
(define s8 's8)
(define s16 's16)

(define array-> list-ref)
(define-syntax-rule (-> s f)
  (assoc s 'f))

(struct field-info (name name-kw type))

(define-syntax (gs-struct stx)
  (define-syntax-class field
    #:description "type field"
    #:attributes (spec name name-kw)

    (pattern (name:id ~! type)
             #:with name-kw (string->keyword (symbol->string (syntax-e #'name)))
             #:attr spec #'(field-info 'name 'name-kw 'type)
             ))

  (syntax-parse stx
    ([_  f:field ...]
     #'(list (list 'f ...))
     )))


(define-syntax (gs-define stx)
  (syntax-parse stx
    ([_ (name param ...)
        body ...]
     #'(define name (list 'name (list 'param ...)))
     )))

;; types
;;  len+string (from Merlin ASC)
(define gs-string (gs-struct (len u8) (char (array[] s8))))
;;  note array[] is unbounded array, or pointer

;; rectangular buffer, or rb

(define rb
  (gs-struct
   (start u8-ptr)
   (size (tuple s16 s16)) ;; width, height
   (stride s16) ;;  #:default ([0] size)) ;; defaults to width
   (bpp s8) ;; bits pre pixel
   ))

const Size = struct {
  w: i16,
  h: i16
};

const RB = struct {
  start: *u8,
  size: Size,
  stride: i16,
  bpp: i8

  pub fn create(start: *u8, size: Size, bpp: i8) RB
  {
    return RB { .start = start, .size = size, .stride = size.w, .bpp = bpp };
  }
};

(define shr-screen-320
  (rb #:start #xe12000 #:size '(320 200) #:bpp 4))

(define shr-screen-640
  (rb #:start #xe12000 #:size '(640 200) #:bpp 2))

(define vec2 (gs-struct (x s16) (y s16)))

(define font-table
  (gs-struct 
   (character ([] rb))
   (page rb)
   (offset s16) ;; subtract offset from ASCII code to yield index into character array
   ))

(gs-define (get-char font ci) ;; font-table -> s16 -> ()
           (array-> (-> font character) ci)
           )

(gs-define (rb-blit c dest offset) ;; rb -> rb -> offset
           (let row 0)
           (while (< row (tuple (rb-size c) 0))
                  (rb-blit-row c dest row)
                  ;; TODO: check for vertical bounds of dest!
                  ))

(gs-define (draw-string font dest-rb str pos) ;; font-table -> rb -> string -> vec2
           (let index 0)
           (while (< index (string-length str))
                  (let ci (string-ref str index)
                    (let table-idx (- c (font-offset font)))
                    (let c (get-char font ci))
                    (rb-blit c dest-rb offset)
                    ;; TODO: define below SET! functions
                    (!+rb offset (rb-width c))
                    (!+ index 1)
                    )
                  ))
