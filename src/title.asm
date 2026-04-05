.p816
.a8
.i16

.include "defines.inc"
.include "registers.inc"
.include "joypad.inc"

.segment "CODE"

TitleScreenSprites:    .incbin "graphics/title-screen-sprites.4bpp"
TitleScreenPalette:    .incbin "graphics/title-screen-palette.pal"
TitleScreenSpriteData: .incbin "data/title-screen-sprites.bin"

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
	stx DMA0ADDAL                 ; read from overworld sprite data
	lda #BANK_MAIN                ; which is in this bank
	sta DMA0ADDAH
	ldx #$2000                    ; write 8 KB
	stx DMA0AMTL
	lda #$01
	sta DMA0PARAM                 ; configure DMA0 for A->B, inc A address, 2 bytes to 2 registers (VMDATAL/H)
	sta MDMAEN                    ; enable

	; Now load the palette.
	lda #$80
	stz CGADD                 ; start at CGRAM address $80
	ldx #$0000
Loop:
	lda TitleScreenPalette, X ; get a byte of palette data
	sta CGDATA                ; write it to CGRAM
	inx
	cpx #$0020                ; length of palette data
	bne Loop

	; Finally, copy the sprite data into OAM.
	rep #$20                      ; A to 16-bit
	ldx #$0000
	lda TitleScreenSpriteData, X  ; read
	sta OamMirror, X              ; write
	inx                           ; 2 bytes at a time
	inx
	cpx #$0220                    ; length of sprite data (same as OAM size)
	rts
.endproc
