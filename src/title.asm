.p816
.a8
.i16

.include "defines.inc"
.include "registers.inc"
.include "joypad.inc"

.segment "CODE"

.export LoadTitleScreenAndWaitForInput
.export DoCharacterSelect

.import GetJoypadInputs

TitleScreenSprites:    .incbin "graphics/title-screen-sprites.4bpp"
TitleScreenPalette:    .incbin "graphics/title-screen-palette.pal"
TitleScreenSpriteData: .incbin "data/title-screen-sprites.bin"

FontChr:               .incbin "graphics/font.2bpp"
HandSprite:            .incbin "graphics/hand.4bpp"
HandPalette:           .incbin "graphics/hand-palette.pal"

FontPalette:
	COLOR  0,  0,  0 ; normally 31/0/31, but this will be the actual background
	COLOR  0,  0, 31
	COLOR 31, 31, 31
	COLOR 16, 16, 16

; This sets up the video for the title screen, loads the data,
; and puts it on the screen, then waits for the player to press
; A or start.
.proc LoadTitleScreenAndWaitForInput
	lda #GAME_MODE_OVERWORLD ; title screen is gonna use the overworld engine
	sta GameMode

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
	lda #$ff                ; set fixed color to white
	sta COLDATA             ; this should fade the background entirely to black

	stz INIDISP             ; release forced blanking, set screen to zero brightness
	lda #$81
	sta NMITIMEN            ; enable NMI, turn on automatic joypad polling

	lda #$01
	pha                     ; save A because we're going to get interrupted
@FadeInLoop:
	wai                     ; wait one frame
	pla
	sta INIDISP             ; gradually increase brightness
	inc
	pha                     ; save A
	cmp #$10                ; until we've reached full brightness
	bne @FadeInLoop
	pla                     ; clear the stack

	rep #$20                ; set A to 16-bit
	lda #%111111111         ; this will count downward, and the upper 5 bits are the fixed color
	pha                     ; save A because we're going to get interrupted
@InputLoop:
	wai
	pla
	beq :+                  ; if we hit 0, we can't get any brighter
	dec
:
	pha
	lsr                     ; divide by 16
	lsr
	lsr
	lsr
	sep #$20                ; set A to 8-bit
	ora #$e0                ; all three color channels
	sta COLDATA             ; set the fixed color
	rep #$20                ; set A back to 16-bit

	jsr GetJoypadInputs
	lda JoyTrigger1
	and #(BUTTON_A | BUTTON_START)
	beq @InputLoop
	pla                     ; clear the stack

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

.proc DoCharacterSelect
	jsr InitializeCharacterSelect

@InputLoop:
	wai
	jsr GetJoypadInputs
	lda JoyTrigger1
	and #(BUTTON_A | BUTTON_START)
	beq @InputLoop

	rts
.endproc

.proc InitializeCharacterSelect
; Dungeon Map VRAM mapping
; 16 KB BG1 CHR       0x0000 word address
; 8 KB BG2 CHR        0x2000 word address
; 4 KB BG3 CHR        0x3000 word address
; 2 KB BG3 tilemap    0x3800 word address
; 8 KB BG1 tilemap    0x4000 word address
; 8 KB BG2 tilemap    0x5000 word address
; 16 KB sprites       0x6000 word address

; BG12NBA = $20
; BG34NBA = $03
; BG1SC   = $43
; BG2SC   = $53
; BG3SC   = $38

	sep #$20                ; A to 8-bit
	lda #GAME_MODE_DUNGEON  ; character select uses mode 1, like the subscreen or a town shop
	sta GameMode

	lda #$8f                ; force v-blanking
	sta INIDISP
	stz NMITIMEN            ; disable NMI

	lda #$31                ; video mode 1, large character size for BG1 and BG2
	sta BGMODE
	lda #$20                ; see above VRAM mapping for details of these registers
	sta BG12NBA
	lda #$03
	sta BG34NBA
	lda #$43
	sta BG1SC
	lda #$53
	sta BG2SC
	lda #$38
	sta BG3SC
	lda #$14                ; enable sprites and BG3
	sta TM
	lda #$00                ; no background layers on subscreen
	sta TS
	stz HDMAEN              ; turn off any HDMA

	; We're going to clear out BG3.
	lda #$80                      ; VRAM increment on write to VMDATAH
	sta VMAINC
	rep #$30                      ; A,X,Y to 16-bit

	; Zero out the BG3 tilemap buffer and copy it to VRAM.
	lda #$0001
	sta BG3TileMapBufferDirty     ; 2-byte write to a 1-byte variable, but the second byte will get overwritten immediately
	ldx #$0000
@BG3Loop:
	stz BG3TileMapBuffer, X
	inx
	inx
	cpx #$0800
	bne @BG3Loop

	; Load the font graphics.
	sep #$20                      ; A to 8-bit
	rep #$10                      ; X,Y to 16-bit
	stz MDMAEN                    ; reset DMA
	lda #$80                      ; VRAM increment on write to VMDATAH
	sta VMAINC
	ldx #$3000
	stx VMADDL                    ; start at VRAM address $3000
	lda #<VMDATAL                 ; write to VRAM low register
	sta DMA0ADDB
	ldx #FontChr
	stx DMA0ADDAL                 ; read from title screen sprite graphics
	lda #BANK_MAIN                ; which is in this bank
	sta DMA0ADDAH
	ldx #$1000                    ; write 4 KB
	stx DMA0AMTL
	lda #$01
	sta DMA0PARAM                 ; configure DMA0 for A->B, inc A address, 2 bytes to 2 registers (VMDATAL/H)
	sta MDMAEN                    ; enable

	; Now load the palettes.
	stz CGADD                 ; start at CGRAM address $80
	ldx #$0000
@FontPaletteLoop:
	lda FontPalette, X        ; get a byte of palette data
	sta CGDATA                ; write it to CGRAM
	inx
	cpx #$0008                ; length of palette data
	bne @FontPaletteLoop

	; Party Select box is from (6, 0) to (25, 5)
	pea 6
	pea 0
	pea 25
	pea 5
	jsr DrawWindow
	plx
	plx
	plx
	plx
	; Character box is from (1, 7) to (13, 23)
	pea 1
	pea 7
	pea 13
	pea 23
	jsr DrawWindow
	plx
	plx
	plx
	plx
	; Blursings box is from (15, 7) to (30, 23)
	pea 15
	pea 7
	pea 30
	pea 23
	jsr DrawWindow
	plx
	plx
	plx
	plx
	; Start Game box is from (19, 25) to (30, 27)
	pea 19
	pea 25
	pea 30
	pea 27
	jsr DrawWindow
	plx
	plx
	plx
	plx

	; Draw some text.
	pea PartySelectString
	pea 8
	pea 0
	jsr DrawText
	plx
	plx
	plx
	pea StatusString
	pea 19
	pea 7
	jsr DrawText
	plx
	plx
	plx
	pea StartGameString
	pea 20
	pea 26
	jsr DrawText
	plx
	plx
	plx

	lda #$0f
	sta INIDISP             ; release forced blanking, set screen to full brightness
	lda #$81
	sta NMITIMEN            ; enable NMI, turn on automatic joypad polling
	rts
.endproc

; Draws a box on the screen, from x1 to x2, y1 to y2, where the arguments are passed
; on the stack in that order.
.proc DrawWindow
	; Stack variables
	x1 = $0B
	y1 = $09
	x2 = $07
	y2 = $05
	; Window characters
	BorderTL = $c9
	BorderT  = $cb
	BorderTR = $bb
	BorderL  = $ca
	BorderR  = $bc
	BorderBL = $cc
	BorderB  = $b9
	BorderBR = $c8

	phd
	tsc
	tcd
	php
	rep #$30                      ; set A,X,Y to 16-bit
	lda y1                        ; calculate offset into BG3 tilemap
	asl
	asl
	asl
	asl
	asl                           ; multiply Y by 32
	clc
	adc x1                        ; we now have the word address, so
	asl                           ; multiply by 2 to get the byte address
	tax                           ; put the address in X
	lda x2
	sec
	sbc x1
	pha                           ; push the tile count
	lda #BorderTL                 ; push the top border tiles
	pha
	lda #BorderT
	pha
	lda #BorderTR
	pha
	jsr DrawWindowRow             ; Draw the top row

	; Now we draw the main part of the window.  The tile count is the same, so we just
	; have to change the tiles.  They've already been pushed onto the stack, and rather
	; than pop them off, we can just set them with stack-relative addressing.
	lda #BorderL
	sta 5, S
	lda #$20 ; space
	sta 3, S
	lda #BorderR
	sta 1, S
	ldy y1
	iny                           ; set Y to the second row
@RowLoop:
	tya
	asl
	asl
	asl
	asl
	asl
	clc
	adc x1
	asl                           ; get the byte address of the beginning of the row again
	tax
	jsr DrawWindowRow
	iny
	cpy y2
	bmi @RowLoop

	lda #BorderBL                 ; Now we do the bottom row.
	sta 5, S
	lda #BorderB
	sta 3, S
	lda #BorderBR
	sta 1, S
	tya                           ; y should already be equal to y2
	asl
	asl
	asl
	asl
	asl
	clc
	adc x1
	asl                           ; get the byte address of the beginning of the row again
	tax
	jsr DrawWindowRow

	; Clean up the stack!
	pla
	pla
	pla
	pla
	plp
	pld
	rts
.endproc

PartySelectString: .asciiz " PARTY  SELECT "
StatusString:      .asciiz " STATUS "
StartGameString:   .asciiz "START GAME"

.proc DrawWindowRow
	TileCount  = $0B
	LeftTile   = $09
	MiddleTile = $07
	RightTile  = $05
	phd
	tsc
	tcd

	lda LeftTile                  ; begin the row
	and $00ff                     ; the tile index is just one byte on the stack, but two bytes in VRAM
	sta BG3TileMapBuffer, X       ; high byte stores palette/priority/flipping, not needed for this
	inx
	inx                           ; 2 bytes
	phy
	ldy #$0001
	lda MiddleTile                ; space
	and $00ff
@TopLoop:
	sta BG3TileMapBuffer, X
	inx
	inx                           ; 2 bytes
	iny                           ; but the coordinate only increased by 1
	cpy TileCount
	bmi @TopLoop
	lda RightTile                 ; complete the row
	and $00ff
	sta BG3TileMapBuffer, X
	ply

	pld
	rts
.endproc

.proc DrawText
	pText  = $09
	xCoord = $07
	yCoord = $05
	phd
	tsc
	tcd
	php
	rep #$20                ; A to 16-bit

	lda yCoord              ; formula is 2 * (y*32 + x), 2 bytes per tile, 32 tiles across
	asl
	asl
	asl
	asl
	asl
	clc
	adc xCoord
	asl
	tax

	ldy #$0000
@Loop:
	lda (pText), Y          ; fetch a character
	and #$00ff              ; we just want one byte of text at a time
	beq @Done               ; null-terminated string
	sta BG3TileMapBuffer, X ; we have to store two bytes for the tile
	iny                     ; next character
	inx                     ; and next tile location
	inx
	bra @Loop

@Done:
	plp
	pld
	rts
.endproc

; Zeroes out VRAM starting from a word address given in the A register and a word count
; given in the X register.  A,X,Y assumed to be 16-bit.
.proc ZeroVRAM
	sta VMADDL
@Loop:
	stz VMDATAL
	dex
	bne @Loop
	rts
.endproc
