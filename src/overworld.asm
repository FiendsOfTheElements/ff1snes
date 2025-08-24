.p816
.a8
.i16

.include "defines.inc"
.include "registers.inc"
.include "joypad.inc"

.export LoadOverworld
.export DoOverworldMovement
.export SetMode7Matrix

.import JOYPAD1

.import Sine
.import Cosine

.segment "OVERWORLD"
CompressedOverworld:
.incbin "data/overworld-map.bin"

.segment "CODE"

; for now this can live in the code segment, but we'll probably move it.
OverworldChr:             .incbin "graphics/overworld-chr.8bpp"
OverworldPalette:
	COLOR  0,  0,  0
	COLOR  0, 24,  0
	COLOR 15, 15, 15
	COLOR 31, 31, 31
	COLOR  0,  0,  0
	COLOR  0, 24,  0
	COLOR 31, 15,  0
	COLOR 31, 31, 15
	COLOR  0,  0,  0
	COLOR  0, 24,  0
	COLOR 24, 24, 31
	COLOR 10, 10, 31
	COLOR  0,  0,  0
	COLOR  0, 24,  0
	COLOR 15, 31, 15
	COLOR  0, 24,  0

OverworldTilemaps:        .incbin "data/overworld-tilemaps.bin"
OverworldTilePaletteMaps: .incbin "data/overworld-tile-palette-maps.bin"

OverworldMap = $4000 ; store the decompressed overworld map from $7E:4000-7E:7FFF (64 rows x 256 columns, 4096 bytes)
OverworldMapBank = $7E

.export MAPPOSX  = $1000
.export MAPPOSY  = $1002
MINPOSX  = $0080
MAXPOSX  = $0380
MINPOSY  = $0070
MAXPOSY  = $0390
MAPANGLE = $1004
MAPZOOM  = $1006
MAXZOOM  = $7F   ; zoom value is fixed point, where $0040 is 1.0
MINZOOM  = $20   ; we do this for precision reasons with mode 7 multiply

.proc LoadOverworld
	sep #$20                ; set A to 8-bit
	lda #$8f                ; force v-blanking
	sta INIDISP
	stz NMITIMEN            ; disable NMI

	;jsr LoadOverworldMapData
	jsr LoadOverworldCharacters
	jsr LoadOverworldPalette

	stz MAPANGLE            ; initial rotation (none)
	lda #$0040              ; initial zoom (1x)
	sta MAPZOOM

	sep #$20                ; set A to 8-bit
	lda #$07
	sta BGMODE
	lda #$80
	sta M7SEL               ; use CGRAM color 0 off the edge, no flipping
	stz M7A
	lda #$01
	sta M7A                 ; initialize Mode 7 matrix with identity
	stz M7B
	stz M7B
	stz M7C
	stz M7C
	stz M7D
	sta M7D

	jsr SetMode7Matrix      ; set up Mode 7 transform parameters

	lda #$01                ; enable BG1
	sta TM

	lda #$0f
	sta INIDISP             ; release forced blanking, set screen to full brightness
	lda #$81
	sta NMITIMEN            ; enable NMI, turn on automatic joypad polling

	rts
.endproc

.proc LoadOverworldMapData
	; Here we decompress the map data.  The map is 256x256, but we will only load 64 rows
	; at a time, centered around the player's Y coordinate.  Each map row will go into the
	; "slot" of the actual row mod 64, so as you scroll up and down, the rows behind you
	; get cycled out for rows in front of you.
	TempMaxY = $00             ; Keep track of the maximum Y position
	sep #$30                   ; A,X,Y to 8-bit.  We'll wrap around the world correctly.
	lda MAPPOSY				   ; get the player's Y position
	clc
	adc #$1E                   ; max Y position is 30 rows higher (farther down)
	sta TempMaxY
	clc
	sbc #$3F                   ; min Y position is 63 rows below max.  We go farther up than
	tay                        ; down because we can see farther up due to the perspective.
@Loop:
	phy                        ; Save Y which will definitely be overwritten by decoding
	php                        ; Save the bitness of the registers
	jsr DecompressMapRow       ; Decompress one row
	plp                        ; Restore the bitness before
	ply                        ; restoring Y, otherwise we pop the wrong number of bytes
	iny
	cpy TempMaxY
	bne @Loop
	rts
.endproc

.proc DecompressMapRow
	; Map rows are referenced by a pointer table, 2 bytes per row, and the rows
	; themselves are RLE compressed.  There are 128 possible tiles, so the lower 7 bits
	; represent the tile that appears next.  If the MSB is not set, then the byte simply
	; represents one tile.  If the MSB is set, then the following byte is the number of
	; times to repeat the tile -- a "repeat" value of 1 means the tile appears twice.
	; Rows are terminated by $FF.
	;
	; The value in the Y register is the row we want to decompress.
	; We will decompress it into the (Y mod 64)th row of uncompressed map data.
	TempSrc = $02              ; Keep track of where we're reading from
	sep #$20                   ; Set A to 8-bit
	lda BANK_OVERWORLD         ; Set data bank to overworld source
	pha
	plb
	rep #$30                   ; Set A,X,Y to 16-bit
	phy                        ; remember the actual row
	tya
	and #$003f                 ; row mod 64
	xba                        ; times the row length (256)
	tay                        ; Y now points to the destination row buffer
	pla                        ; restore the actual row
	asl                        ; row# times 2 to get the pointer
	tax
	lda CompressedOverworld, X ; get the pointer
	tax                        ; X now points to the compressed row data

@Loop:
	sep #$20                   ; set A to 8-bit
	lda BANK_OVERWORLD         ; Set data bank to overworld source
	pha
	plb
	lda CompressedOverworld, X
	inx
	cmp #$ff                   ; are we done?
	bne :+
	rts
:
	pha                        ; remember the tile
	cmp #$80                   ; test the MSB
	bcc :+
                               ; if we're here, just one tile
	lda OverworldMapBank       ; set data bank to overworld destination
	pha
	plb
	pla                        ; retrieve the tile
	sta OverworldMap, Y        ; store it
	iny                        ; bump it
	jmp @Loop                  ; loop it
:                              ; if we're here, we repeat the tile
	rep #$20                   ; set A to 16-bit (because X is 16-bit)
	lda CompressedOverworld, X ; get the number of repeats
	inx
	stx TempSrc                ; remember where we were reading from
	and #$00ff                 ; we only want one byte
	tax                        ; X will count down
	sep #$20                   ; A back to 8-bit
	lda OverworldMapBank       ; set data bank to overworld destination
	pha
	plb
	pla                        ; get the tile back
@LoopRepeat:
	sta OverworldMap, Y        ; store it
	iny                        ; bump it
	dex                        ; loop it
	bpl @LoopRepeat
	ldx TempSrc                ; restore where we were reading from
	jmp @Loop
.endproc

.proc LoadOverworldCharacters
	; There are 256 8x8 characters that make up the graphics for the overworld.
	; All we need to do is shove them into VRAM starting at address 0.
	sep #$20                    ; A to 8-bit
	rep #$10                    ; X,Y to 16-bit
	stz MDMAEN                  ; reset DMA
	lda #$80                    ; VRAM increment = 1
	sta VMAINC
	ldx #$0000
	stx VMADDL                  ; start at VRAM address 0
	lda #<VMDATAH               ; write to VRAM
	sta DMA0ADDB
	ldx #OverworldChr
	stx DMA0ADDAL               ; read from overworld CHR data
	stz DMA0ADDAH
	ldx #$4000                  ; write 16 KB (64 bytes * 256 characters)
	stx DMA0AMTL
	stz DMA0PARAM               ; configure DMA0 for A->B, inc A address, 1 byte to 1 register
	lda #$01                    ; DMA0 channel
	sta MDMAEN                  ; enable
	rts
.endproc

.proc LoadOverworldPalette
	rep #$20        ; set A to 16-bit so we can
	lda #$0000      ; clear the high byte
	sep #$20        ; set A to 8-bit
	rep #$10        ; set X,Y to 16-bit

	stz CGADD       ; start at CGRAM address 0
	ldx #$0000
Loop:
	lda OverworldPalette, X ; get a byte of palette data
	sta CGDATA              ; write it to CGRAM
	inx
	cpx #$0020              ; length of palette data
	bne Loop

	rts
.endproc

.proc DoOverworldMovement
	rep #$20                            ; set A to 16-bit
	; check the dpad, if any of the directional buttons are pressed,
	; move the screen accordingly
CheckUpButton:
	lda JOYPAD1                         ; read joypad buttons pressed
	and #BUTTON_UP
	beq CheckDownButton
	lda MAPPOSY
	cmp #MINPOSY
	beq CheckDownButton
	dec
	sta MAPPOSY

CheckDownButton:
	lda JOYPAD1
	and #BUTTON_DOWN
	beq CheckLeftButton
	lda MAPPOSY
	cmp #MAXPOSY
	beq CheckLeftButton
	inc
	sta MAPPOSY

CheckLeftButton:
	lda JOYPAD1
	and #BUTTON_LEFT
	beq CheckRightButton
	lda MAPPOSX
	cmp #MINPOSX
	beq CheckRightButton
	dec
	sta MAPPOSX

CheckRightButton:
	lda JOYPAD1
	and #BUTTON_RIGHT
	beq CheckXButton
	lda MAPPOSX
	cmp #MAXPOSX
	beq CheckXButton
	inc
	sta MAPPOSX

CheckXButton:
	lda JOYPAD1
	and #BUTTON_X
	beq CheckYButton
	lda MAPZOOM
	and #$00FF        ; just read one byte
	cmp #MINZOOM
	beq CheckYButton
	dec
	sta MAPZOOM

CheckYButton:
	lda JOYPAD1
	and #BUTTON_Y
	beq CheckRButton
	lda MAPZOOM
	and #$00FF
	cmp #MAXZOOM
	beq CheckRButton
	inc
	sta MAPZOOM

CheckRButton:
	lda JOYPAD1
	and #BUTTON_R
	beq CheckLButton
	lda MAPANGLE
	dec
	sta MAPANGLE

CheckLButton:
	lda JOYPAD1
	and #BUTTON_L
	beq Done
	lda MAPANGLE
	inc
	sta MAPANGLE

Done:
	rts
.endproc

.proc SetMode7Matrix
	sep #$20          ; set A to 8-bit
	lda MAPPOSX
	sta M7X
	lda MAPPOSX + 1
	sta M7X
	lda MAPPOSY
	sta M7Y
	lda MAPPOSY + 1
	sta M7Y

	TempX = $00
	TempY = $02
	rep #$20          ; set A to 16-bit
	lda MAPPOSX
	sec
	sbc #$80
	sta TempX
	lda MAPPOSY
	sec
	sbc #$70
	sta TempY

	sep #$20          ; set A to 8-bit
	lda TempX
	sta BG1HOFS
	lda TempX + 1
	sta BG1HOFS
	lda TempY
	sta BG1VOFS
	lda TempY + 1
	sta BG1VOFS

	; Now we do trig.
	TempCos    = $02
	TempSin    = $04
	TempNegSin = $06
	rep #$20          ; set A to 16-bit
	lda MAPANGLE
	jsr Cosine        ; get the cosine
	sta TempCos
	sep #$20          ; set A to 8-bit
    lda TempCos       ; multiply -- this will be signed, fixed-point where A is IIII IIII.FFFF FFFF
	sta M7A           ; and B is II.FF FFFF
	lda TempCos + 1
	sta M7A
	lda MAPZOOM
	sta M7B
	lda MPYL          ; we need the result shifted down six times, but it's 24-bit, and we only need
	asl               ; 16 bits of it, so we can shift it up twice and take the two high bytes
	sta TempCos
	lda MPYM
	rol
	sta TempCos + 1
	lda MPYH
	rol
	sta TempCos + 2   ; this will overwrite, but if we do it in the right order, doesn't matter
	lda TempCos
	asl
	lda TempCos + 1
	rol
	sta TempCos
	lda TempCos + 2
	rol
	sta TempCos + 1

	rep #$20          ; set A to 16-bit
	lda MAPANGLE
	jsr Sine          ; get the sine
	sta TempSin
	sep #$20          ; set A to 8-bit
    lda TempSin       ; multiply -- this will be signed, fixed-point where A is IIII IIII.FFFF FFFF
	sta M7A           ; and B is II.FF FFFF
	lda TempSin + 1
	sta M7A
	lda MAPZOOM
	sta M7B
	lda MPYL          ; we need the result shifted down six times, but it's 24-bit, and we only need
	asl               ; 16 bits of it, so we can shift it up twice and take the two high bytes
	sta TempSin
	lda MPYM
	rol
	sta TempSin + 1
	lda MPYH
	rol
	sta TempSin + 2   ; this will overwrite, but if we do it in the right order, doesn't matter
	lda TempSin
	asl
	lda TempSin + 1
	rol
	sta TempSin
	lda TempSin + 2
	rol
	sta TempSin + 1

	rep #$20          ; set A to 16-bit
	lda TempSin       ; get the negative sine
	eor #$FFFF
	inc
	sta TempNegSin

	; Now we've done the trig calculations, set up the matrix.
	rep #$10          ; set X and Y to 16-bit
	sep #$20
	lda TempCos
	sta M7A
	lda TempCos + 1
	sta M7A
	lda TempNegSin
	sta M7B
	lda TempNegSin + 1
	sta M7B
	lda TempSin
	sta M7C
	lda TempSin + 1
	sta M7C
	lda TempCos
	sta M7D
	lda TempCos + 1
	sta M7D

	rts
.endproc
