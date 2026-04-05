.p816
.a8
.i16

.include "defines.inc"
.include "registers.inc"
.include "joypad.inc"

.segment "CODE"

.export LoadTitleScreenAndWaitForInput

.import GetJoypadInputs

TitleScreenSprites:    .incbin "graphics/title-screen-sprites.4bpp"
TitleScreenPalette:    .incbin "graphics/title-screen-palette.pal"
TitleScreenSpriteData: .incbin "data/title-screen-sprites.bin"

; This sets up the video for the title screen, loads the data,
; and puts it on the screen, then waits for the player to press
; A or start.
.proc LoadTitleScreenAndWaitForInput
	lda #$8f                ; force v-blanking
	sta INIDISP
	stz NMITIMEN            ; disable NMI

	jsr LoadTitleScreenSprites

	sep #$20                ; set A to 8-bit

	lda #$07                ; video mode 7
	sta BGMODE
	stz M7SEL               ; wrap tiles, no flipping
	;jsr SetMode7Matrix      ; set up Mode 7 transform parameters

	lda #$62                ; sprites are 16x16 and 32x32, sprite RAM is at $8000 (but word address $4000)
	sta OBJSEL

	lda #$11                ; enable sprites and BG1
	sta TM
	lda #$00                ; no background layers on subscreen
	sta TS
	lda #$00                ; set fixed color for subscreen
	sta CGWSEL
	lda #$81                ; enable color math for BG1, subtract mode
	sta CGADSUB
	lda #$f0                ; set fixed color to white
	sta COLDATA             ; this should fade the background entirely to black

	lda #$0f
	sta INIDISP             ; release forced blanking, set screen to full brightness
	lda #$81
	sta NMITIMEN            ; enable NMI, turn on automatic joypad polling

	rep #$20                ; set A to 16-bit
@Loop:
	wai
	jsr GetJoypadInputs
	lda JoyTrigger1
	and #(BUTTON_A | BUTTON_START)
	beq @Loop

	rts
.endproc

; The "Final Fantasy Superizer" on the title screen is sprites, so that
; they can be rendered over the overworld in Mode 7.  To simplify this,
; we can just bulk load the sprite configuration data from ROM.
.proc LoadTitleScreenSprites
	; First load the sprite graphics.
	sep #$20                      ; A to 8-bit
	rep #$10                      ; X,Y to 16-bit
	stz MDMAEN                    ; reset DMA
	lda #$80                      ; VRAM increment on write to VMDATAH
	sta VMAINC
	ldx #$4000
	stx VMADDL                    ; start at VRAM address $4000
	lda #<VMDATAL                 ; write to VRAM low register
	sta DMA0ADDB
	ldx #TitleScreenSprites
	stx DMA0ADDAL                 ; read from title screen sprite graphics
	lda #BANK_MAIN                ; which is in this bank
	sta DMA0ADDAH
	ldx #$2800                    ; write 10 KB
	stx DMA0AMTL
	lda #$01
	sta DMA0PARAM                 ; configure DMA0 for A->B, inc A address, 2 bytes to 2 registers (VMDATAL/H)
	sta MDMAEN                    ; enable

	; Now load the palette.
	lda #$80
	sta CGADD                 ; start at CGRAM address $80
	ldx #$0000
@PaletteLoop:
	lda TitleScreenPalette, X ; get a byte of palette data
	sta CGDATA                ; write it to CGRAM
	inx
	cpx #$0020                ; length of palette data
	bne @PaletteLoop

	; Finally, copy the sprite data into OAM.
	rep #$20                      ; A to 16-bit
	ldx #$0000
@SpriteDataLoop:
	lda TitleScreenSpriteData, X  ; read
	sta OamMirror, X              ; write
	inx                           ; 2 bytes at a time
	inx
	cpx #$0220                    ; length of sprite data (same as OAM size)
	bne @SpriteDataLoop

	rts
.endproc

