.p816
.a8
.i16

.include "defines.inc"
.include "registers.inc"
.include "joypad.inc"

.export LoadTitleScreenAndWaitForInput
.export DoCharacterSelect

.import GetJoypadInputs
.import CopyBG3TileMapBufferToVRAM
.import CopyOamMirrorToOAM
.import DMA2VRAML : far

.segment "TITLEGRAPHICS"

TitleScreenSprites:    .incbin "graphics/title-screen-sprites.4bpp"  ; 16 KB
FontChr:               .incbin "graphics/font.2bpp"                  ; 4 KB
HandSprite:            .incbin "graphics/hand.4bpp"                  ; 128 bytes

.segment "BATTLEGRAPHICS"

BattleSprites:         .incbin "graphics/battle-sprites.4bpp"        ; 24 KB

.segment "CODE"

; Palettes and sprite data are stored with the code so they don't have to be long copies to CGRAM/OAM.
TitleScreenSpriteData: .incbin "data/title-screen-sprites.bin"
TitleScreenPalette:    .incbin "graphics/title-screen-palette.pal"
HandPalette:           .incbin "graphics/hand-palette.pal"
BattleSpritePalettes:  .incbin "graphics/battle-sprite-palettes.pal"

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

	lda #$63                ; sprites are 16x16 and 32x32, sprite RAM is at $C000 (but word address $6000)
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

	LONGCALL DMA2VRAML, BANK_TITLEGRAPHICS, TitleScreenSprites, $6000, $2800

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
	ClassSelections = $1C ; 4 bytes, one per character
	CharacterNames  = $20 ; 32 bytes, 8 per character, fixed-length strings
	ClassPosition   = $1B
	sep #$20             ; A to 8-bit
	stz ClassPosition
	jsr InitializeCharacterSelect

	rep #$20             ; A to 16-bit
	sep #$10             ; X,Y to 8-bit
@InputLoop:
	jsr PlaceHand
	wai
	jsr GetJoypadInputs
@CheckA:
	lda JoyTrigger1
	and #BUTTON_A | BUTTON_START
	bne @Done
@CheckLeft:
	lda JoyTrigger1
	and #BUTTON_LEFT
	beq @CheckRight
	ldx ClassPosition
	dex
	stx ClassPosition
	bpl @InputLoop
	ldx #$05
	stx ClassPosition
	bra @InputLoop
@CheckRight:
	lda JoyTrigger1
	and #BUTTON_RIGHT
	beq @InputLoop
	ldx ClassPosition
	inx
	stx ClassPosition
	cpx #$06
	bne @InputLoop
	ldx #$00
	stx ClassPosition
	bra @InputLoop

@Done:
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
	LONGCALL DMA2VRAML, BANK_TITLEGRAPHICS, FontChr, $3000, $1000

	sep #$20                  ; A to 8-bit
	stz CGADD                 ; start at CGRAM address $00
	ldx #$0000
@FontPaletteLoop:
	lda FontPalette, X        ; get a byte of palette data
	sta CGDATA                ; write it to CGRAM
	inx
	cpx #$0008                ; length of palette data
	bne @FontPaletteLoop

	; Load the sprites.
	LONGCALL DMA2VRAML, BANK_BATTLESPRITES, BattleSprites, $6000, $3000

	LONGCALL DMA2VRAML, BANK_TITLEGRAPHICS, HandSprite, $60E0, $0040
	LONGCALL DMA2VRAML, BANK_TITLEGRAPHICS, HandSprite + $40, $61E0, $0040

	sep #$20                  ; A to 8-bit
	lda #$80
	sta CGADD                 ; start at CGRAM address $80
	ldx #$0000
@BattleSpritePaletteLoop:
	lda BattleSpritePalettes, X ; get a byte of palette data
	sta CGDATA                ; write it to CGRAM
	inx
	cpx #$00C0                ; length of palette data
	bne @BattleSpritePaletteLoop

	ldx #$0000
@HandPaletteLoop:
	lda HandPalette, X
	sta CGDATA
	inx
	cpx #$0020                ; just one hand palette
	bne @HandPaletteLoop

	jsr DrawCharacterSelectScreen
	jsr PlaceHand
	jsr CopyBG3TileMapBufferToVRAM
	jsr CopyOamMirrorToOAM

	lda #$0f
	sta INIDISP             ; release forced blanking, set screen to full brightness
	lda #$81
	sta NMITIMEN            ; enable NMI, turn on automatic joypad polling

	rts
.endproc

.proc DrawCharacterSelectScreen
	CALL DrawWindow,  6,  1, 24,  6
	CALL DrawWindow,  1,  8, 13, 24
	CALL DrawWindow, 15,  8, 30, 24

	CALL DrawText, PartySelectString, 8, 1
	CALL DrawText, StatusString, 19, 8

	; Put character sprites in the party select window.
	sep #$20          ; A 8-bit
	ldy #$0000
	ldx #$0004        ; start at sprite 1, sprite 0 is the hand
	Temp = $00
@ClassLoop:
	; Leftmost class is at tile position 7, 3, which is 56, 21 pixels
	; So each character gets drawn at position 56 + 24*c, 21
	; The bottom half gets drawn at position 56 + 24*c, 37
	tya
	asl
	asl
	asl                   ; 8*c
	sta Temp
	asl                   ; 16*c
	adc Temp              ; 24*c
	adc #$38              ; 24*c + 56
	sta OamMirror, X      ; sprite 1 x coordinate
	sta OamMirror + 4, X  ; sprite 2 x coordinate
	inx
	lda #$15
	sta OamMirror, X      ; sprite 1 y coordinate
	lda #$25
	sta OamMirror + 4, X  ; sprite 2 y coordinate
	inx
	tya
	asl
	asl
	asl
	asl
	asl
	asl                   ; c*64 is the sprite tile index
	sta OamMirror, X      ; sprite 1 tile
	clc
	adc #$20
	sta OamMirror + 4, X  ; sprite 2 tile
	inx
	tya
	asl                   ; bits 1-3 are the palette, equal to c
	ora #$10              ; priority
	cpy #$04
	bmi :+
	ora #$01              ; set the overflow bit of the tile index if c >= 4
:
	sta OamMirror, X      ; sprite 1 properties
	sta OamMirror + 4, X  ; sprite 2 properties
	inx

	inx
	inx
	inx
	inx                   ; move X past sprite 2
	iny
	cpy #$0006
	bne @ClassLoop

	stz OamMirror + $200  ; set the high bit of x to 0 for these 12 sprites
	stz OamMirror + $201
	stz OamMirror + $202
	lda #$54
	sta OamMirror + $203  ; and the 13th sprite

	rts
.endproc

.proc PlaceHand
	ClassPosition   = $1B
	php
	sep #$20                ; A is 8-bit
	; Hand starts at 38, 25, 4 pixels down from the class and with a gap of 2 pixels to the right
	lda ClassPosition
	asl
	adc ClassPosition       ; c * 3
	asl
	asl
	asl                     ; c * 24
	adc #$26
	sta OamMirror
	lda #$19
	sta OamMirror + $01
	lda #$0e
	sta OamMirror + $02
	lda #$1c
	sta OamMirror + $03

	plp
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
	Space    = $20

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
	pea BorderTL                  ; push the top border tiles
	pea BorderT
	pea BorderTR
	jsr DrawWindowRow             ; Draw the top row
	plx
	plx
	plx

	; Now we draw the main part of the window.  The tile count is the same, so we just
	; have to change the tiles.  They've already been pushed onto the stack, and rather
	; than pop them off, we can just set them with stack-relative addressing.
	pea BorderL
	pea Space
	pea BorderR
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

	plx
	plx
	plx
	pea BorderBL                 ; Now we do the bottom row.
	pea BorderB
	pea BorderBR
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
