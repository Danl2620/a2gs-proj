
; TODO
; + turn io addresses into labels
; + turn keyboard io address into labels
; - draw cursor
; - simplify drawchar
; - try 80 column mode
; - look at cowlark's vi for ideas

OMF16          equ   $B3
SPEAKER        equ   $00C030
PRODOS16       equ   $E100A8
PRODOS         equ   $BF00       ;; 8bit mode only!

;; Keyboard IO locations
KBDDATASTROBE  equ   $00C000
KBDDOWNCLEAR   equ   $00C010

;; Display soft switches; they don't take any values. some are only triggered
;; by writes, TXTPAGE1 can be triggered by a read
CLR80COL       equ   $00C000        ; disable 80 column store?
SET80COL       equ   $00C001
CLR80VID       equ   $00C00C        ; disable 80 column hardware?!
SET80VID       equ   $00C00D        ; enable 80 column hardware?!
TXTCLR         equ   $00C050        ; set standard apple ii gfx mode
TXTSET         equ   $00C051        ; select text mode only. "only"?
TXTPAGE1       equ   $00C054        ; select text page 1 (there are 2)
LORES          equ   $00C056        ; select "low res" graphics

;; key codes
KEY_ESCAPE     equ   #$9B
KEY_UP         equ   #$8B
KEY_DOWN       equ   #$8A
KEY_LEFT       equ   #$88
KEY_RIGHT      equ   #$95
KEY_BACKSPACE  equ   #$FF
KEY_RETURN     equ   #$8D

;; char codes
CHAR_BLOCK     equ   #$20
CHAR_SPACE     equ   #$A0
CHAR_COMMA     equ   #$AC
CHAR_CURSOR    equ   #$FF

;; format
COL_LAST       equ   #39
STATUS_POS     equ $0007ED

;; firmware routines
INIT           equ $00fb2f
HOME           equ $00fc58
COUT           equ $00fded
KEYIN          equ $00fd1b

               ;; rel
               ;; typ   OMF16
               org $2000
               dsk   main.l

; CHARACTER_UNDER_CURSOR DS 1

; ensure 16-bit mode (unnecessary?)
               clc
               xce
               rep   #$30

               phk
               plb

; half bank for accumulator, you don't want to toggle them on then back off, right?
               sep   #$30

; toggle the display soft switches

               ;; disable 80 column mode
               stal  CLR80COL
               stal  CLR80VID

               ;; set Apple II page 1 lores text mode
               stal  TXTCLR
               stal  TXTSET
               ldal  TXTPAGE1
               ldal  LORES

; set X to 16 bits, A to 8 bits
               rep   #$30
               sep   #$20


;; START OF PROGRAM

;; main loop
;;  every 1ms do
;;    draw cursor
;;    check for keypress
;;    if changes, update status bar
;;


; show the current char on screen
; read keyboard then hit strobe to reset
               ldx   #0
               ldy   #0
               jsr   drawpos
               ldal  KBDDOWNCLEAR

;;
;  key event loop
;;

:kloop         clc
               ldal  KBDDATASTROBE  ; grab key pressed and strobe bit
               bit   #%1            ; xfer strobe bit to carry bit
               bmi   :kjump

               ;; blink a cursor
                ;;sta >CHARACTER_UNDER_CURSOR
               lda CHAR_BLOCK
               jsr drawchar

               ;;  lda >CHARACTER_UNDER_CURSOR
               ;;  jsr drawchar

               jmp   :kloop


:kjump         jsr   keydown
               jsr   drawpos
               jmp   :kloop

;;
;  switch based on key press
;;

keydown        cmp   KEY_ESCAPE
               beq   EXIT
               cmp   KEY_UP
               beq   up
               cmp   KEY_DOWN
               beq   down
               cmp   KEY_LEFT
               beq   left
               cmp   KEY_RIGHT
               beq   right
               cmp   KEY_BACKSPACE
               beq   backspace
               cmp   KEY_RETURN
               beq   return

               jsr   drawchar
               jmp   colinc

EXIT           jmp   DONE

;;DONE           jsl   PRODOS16
;;               da    $29
;;               adrl  QuitParm



Error          brk

QuitParm       adrl  $0000
               da    $00

finkey         ldal  KBDDOWNCLEAR   ; clear strobe bit
               lda   #0
               rts

up             cpy   #0
               beq   ping
               dey
               jmp   finkey

down           cpy   #22
               beq   ping
               iny
               jmp   finkey

left           cpx   #0
               beq   ping
               dex
               jmp   finkey

right          cpx   COL_LAST
               beq   ping
               inx
               jmp   finkey

return         ldx   #0             ; carriage return
               jmp   down

backspace      lda   CHAR_SPACE
               cpx   #0
               beq   :contbs
               dex
:contbs        jsr   drawchar
               jmp   finkey


colinc         cpx   COL_LAST
               beq   :rolcol
               inx
               jmp   finkey

:rolcol        ldx   #0
               jmp   down

ping           pha
               ldal  SPEAKER
               pla
               jmp   finkey

drawpos        pha
               phx


               ;; draw the status line inverse
               ldx   #$0
               lda   CHAR_BLOCK

drawpos_l      stal  #$0007D0,x
               inx
               cpx   COL_LAST + 1
               bcc   drawpos_l

               ;; draw the parens and comma
               lda   #"("
               stal  STATUS_POS + 1
               lda   CHAR_COMMA
               stal  STATUS_POS + 4
               lda   #")"
               stal  STATUS_POS + 7

               ;; draw x pos
               txa
               jsr   tencount
               adc   #$B0
               stal  STATUS_POS + 3
               txa
               adc   #$B0
               stal  STATUS_POS + 2

               ;; draw y pos
               tya
               jsr   tencount
               adc   #$B0
               stal  STATUS_POS + 6
               txa
               adc   #$B0
               stal  STATUS_POS + 5

               ;; restore x and original keydown char
               plx
               pla
               rts

drawchar       cpy   #0
               beq   :row0
               cpy   #1
               beq   :row1
               cpy   #2
               beq   :row2
               cpy   #3
               beq   :row3
               cpy   #4
               beq   :row4
               cpy   #5
               beq   :row5
               cpy   #6
               beq   :row6
               cpy   #7
               beq   :row7
               cpy   #8
               beq   :row8
               cpy   #9
               beq   :row9
               cpy   #10
               beq   :row10
               cpy   #11
               beq   :row11
               cpy   #12
               beq   :row12
               cpy   #13
               beq   :row13
               cpy   #14
               beq   :row14
               cpy   #15
               beq   :row15
               cpy   #16
               beq   :row16
               cpy   #17
               beq   :row17
               cpy   #18
               beq   :row18
               cpy   #19
               beq   :row19
               cpy   #20
               beq   :row20
               cpy   #21
               beq   :row21
               cpy   #22
               beq   :row22
               rts
:row0          stal  $000400,X
               rts
:row1          stal  $000480,X
               rts
:row2          stal  $000500,X
               rts
:row3          stal  $000580,X
               rts
:row4          stal  $000600,X
               rts
:row5          stal  $000680,X
               rts
:row6          stal  $000700,X
               rts
:row7          stal  $000780,X
               rts
:row8          stal  $000428,X
               rts
:row9          stal  $0004A8,X
               rts
:row10         stal  $000528,X
               rts
:row11         stal  $0005A8,X
               rts
:row12         stal  $000628,X
               rts
:row13         stal  $0006A8,X
               rts
:row14         stal  $000728,X
               rts
:row15         stal  $0007A8,X
               rts
:row16         stal  $000450,X
               rts
:row17         stal  $0004D0,X
               rts
:row18         stal  $000550,X
               rts
:row19         stal  $0005D0,X
               rts
:row20         stal  $000650,X
               rts
:row21         stal  $0006D0,X
               rts
:row22         stal  $000750,X
               rts

;;
;  print a string
;;


* print          ldy   #$01        ;; start 1 past return address
* loop           lda   (1,s),y     ;; get a char to print
*                beq   fix
*                jsr   cout
*                iny
*                bra   loop

* fix            sty   len
*                lda   1,s
*                adc   len
*                sta   1,s

*                lda   2,s
*                adc   #0
*                sta   2,s
*                rts

;;
;  do some BCD
;;

tencount       ldx   #0
:substart      clc
               cmp   #10
               bcc   :subout
               sbc   #10
               inx
               jmp   :substart
:subout        rts

               jsl   PRODOS16


;; back to 8 bit mode (must be last section of file)

DONE           sep #$30  ; set the M bit in the P register to put the processor into 8-bit mode

               JSR  PRODOS        ;Call the MLI ($BF00)
               DFB  $65           ;CALL TYPE = QUIT
               DW   PARMTABLE     ;Pointer to parameter table

PARMTABLE      DFB  4             ;Number of parameters is 4
               DFB  0             ;0 is the only quit type
               DW   0000          ;Pointer reserved for future use
               DFB  0             ;Byte reserved for future use
               DW   0000          ;Pointer reserved for future use
