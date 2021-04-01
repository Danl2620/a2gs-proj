****************************************
* SHRHELLO                             *
*                                      *
*  Dagen Brock <dagenbrock@gmail.com>  *
*  2013-07-21                          *
****************************************

;; Keyboard IO locations
KBDDATASTROBE  equ   $00C000
KBDDOWNCLEAR   equ   $00C010

;; Graphics
PALETTE 			EQU $E19E00	;palettes are stored from $E19E00-FF
NEWVIDEOREG			EQU $00C029	;Turn on SHR mode
DHR_SCREEN			EQU $E12000
DHR_SCBS			EQU $E19D00

VIDEO_SHR			EQU #$80
VIDEO_LINEAR		EQU #$40
VIDEO_DHR_BW		EQU #$20
VIDEO_BANK_LATCH	EQU #$01

COLOR_BLACK			EQU $0000
COLOR_WHITE			EQU $0FFF
COLOR_RED			EQU $0F00
COLOR_GREEN			EQU $00F0
COLOR_BLUE			EQU $000F


	rel	; Compile
	dsk SHRHELLO.l	; Save Name
	mx %00
	phk	; Set Data Bank to Program Bank
	plb	; Always do this first!

Start
	lda #COLOR_WHITE
	ldx #$0000	; palette index $0
	jsr SetPaletteColor

	lda #COLOR_RED
	ldx #$0001	; palette index $1
	jsr SetPaletteColor

	lda #COLOR_GREEN
	ldx #$000F	; palette index $F
	jsr SetPaletteColor

	lda #$0000
	jsr SetSCBs	; set all SCBs to 00 (320 mode, pal 0, no fill, no interrupt)
	jsr GraphicsOn

	lda #$0000	; clear screen to color 0 and turn on SHR graphics
	jsr ClearToColor
	lda #HelloStr
	ldx #60*160+30
	jsr DrawString
	jsr WaitKey

	lda #$1111	; clear screen to color 1
	jsr ClearToColor
	lda #HelloStr
	ldx #60*160+50
	jsr DrawString
	jsr WaitKey
	;; fall thru to Quit()

;; Quit()
Quit
	jsl $E100A8	; Prodos 16 entry point
	da $29	; Quit code
	adrl QuitParm	; address of parameter table
	bcs Error	; never taken

Error	brk	; should never get here

QuitParm
	adrl $0000	; pointer to pathname (not used here)
	da $00	; quit type (absolute quite)

HelloStr	str 'HELLO KANSASFEST'

****************************************
* Turn on SHR mode                     *
****************************************
;; GraphicsOn()
GraphicsOn
	sep #$30	;8-bit mode
	lda VIDEO_SHR + VIDEO_BANK_LATCH ; really prefer a const IOR here
	stal NEWVIDEOREG	;Turn on SHR mode
	rep #$30	;back to 16-bit mode
	rts

****************************************
* A= color values (0RGB)               *
* X= color/palette offset              *
*   (0-F = pal0, 10-1F = pal1, etc.)   *
****************************************
;; SetPaletteColor(u16 color, u8 offset)
SetPaletteColor
	pha	;save accumulator
	txa
	asl	;X*2 = real offset to color table
	tax
	pla
	stal PALETTE,x
	rts	;yup, that's it

****************************************
* A= color values (0RGB)               *
****************************************
;; ClearToColor()
ClearToColor
	ldx #$7D00	;start at top of pixel data! ($2000-9D00)
:clearloop	dex
	dex
	stal DHR_SCREEN,x	;screen location
	bne :clearloop	;loop until we've worked our way down to 0
	rts

;; SetSCBs(u16 val) -- set all $100 SCBs to val
SetSCBs
	ldx #$0100
:scbloop
	dex
	dex
	stal DHR_SCBS,x
	bne :scbloop
	rts

;; WaitKey()
WaitKey
	sep #$30
:wait
	ldal KBDDATASTROBE
	bpl :wait
	stal KBDDOWNCLEAR
	rep #$30
	rts

	use UTIL.MACS
	use FONT	;include our font library
