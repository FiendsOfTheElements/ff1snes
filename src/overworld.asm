.p816
.a8
.i16

.include "defines.inc"
.include "registers.inc"
.include "joypad.inc"

.export LoadOverworld
.export DoOverworldMovement
.export CopyTileMapBufferToVRAM
.export SetMode7Matrix

.import Sine
.import Cosine

.segment "OVERWORLD"
OverworldChr:        .incbin "graphics/overworld-chr.m7"         ; 16 KB
CompressedOverworld: .incbin "data/overworld-map.bin"            ; 16 KB, filling out the entire bank
.segment "OWSPRITE"
OverworldSprites:    .incbin "graphics/overworld-sprites.4bpp"   ; 12 KB

.segment "CODE"

OverworldPalette:
	COLOR  0,  0,  0
	COLOR  0, 21,  0
	COLOR 23, 23, 23
	COLOR 31, 31, 31
	COLOR  0,  0,  0
	COLOR  0, 21,  0
	COLOR 31, 20,  8
	COLOR 31, 28, 21
	COLOR  0,  0,  0
	COLOR  0, 21,  0
	COLOR 20, 28, 31
	COLOR  7, 23, 31
	COLOR  0,  0,  0
	COLOR  0, 21,  0
	COLOR 23, 31,  3
	COLOR  0, 21,  0

OverworldTilemaps: .incbin "data/overworld-tilemaps.bin"

OverworldSpritePalette:
	COLOR  0,  0,  0
	COLOR  0,  0,  0
	COLOR 27,  1, 16
	COLOR 30, 30, 10
	COLOR  0,  0,  0
	COLOR  0,  0,  0
	COLOR  1, 13, 25
	COLOR 30, 30, 10
	COLOR  0,  0,  0
	COLOR  0,  0,  0
	COLOR 27, 19,  1
	COLOR 30, 30, 10
	COLOR  0,  0,  0
	COLOR  0,  0,  0
	COLOR 31, 31, 31
	COLOR 30, 30, 10

DirDown  = 1
DirUp    = 2
DirLeft  = 3
DirRight = 4

MAPPOSX  = $1000
MAPPOSY  = $1002
MAPANGLE = $1004
MAPZOOM  = $1006
MOVEDIR  = $1008
FACEDIR  = $1009

MAXZOOM  = $7F   ; zoom value is fixed point, where $0040 is 1.0
MINZOOM  = $20   ; we do this for precision reasons with mode 7 multiply

.proc LoadOverworld
	sep #$20                ; set A to 8-bit
	lda #$8f                ; force v-blanking
	sta INIDISP
	stz NMITIMEN            ; disable NMI

	stz MOVEDIR             ; be still
	lda #DirDown            ; face down
	sta FACEDIR
	rep #$30                ; A,X,Y to 16-bit
	ldx #$0998              ; set the initial scroll
	stx MAPPOSX
	ldy #$0A58
	sty MAPPOSY
	stz MAPANGLE            ; initial rotation (none)
	lda #$0040              ; initial zoom (1x)
	sta MAPZOOM

	jsr LoadOverworldCharacters
	jsr LoadOverworldPalette
	jsr LoadOverworldSprites
	jsr LoadOverworldSpritePalette
	jsr LoadOverworldMapData

	jsr SetupVideo

	sep #$20                ; A to 8-bit
	lda #$81
	sta NMITIMEN            ; enable NMI, turn on automatic joypad polling

	rts
.endproc

.proc LoadOverworldCharacters
	; There are 256 8x8 characters that make up the graphics for the overworld.
	; All we need to do is shove them into VRAM starting at address 0.
	sep #$20                    ; A to 8-bit
	rep #$10                    ; X,Y to 16-bit
	stz MDMAEN                  ; reset DMA
	lda #$80                    ; VRAM increment on write to VMDATAH
	sta VMAINC
	ldx #$0000
	stx VMADDL                  ; start at VRAM address 0
	lda #<VMDATAH               ; write to VRAM high register (Mode 7 graphics)
	sta DMA0ADDB
	ldx #OverworldChr & $ffff   ; the OverworldChr address is long, so we mask it
	stx DMA0ADDAL               ; read from overworld CHR data
	lda #BANK_OVERWORLD         ; which is in this bank
	sta DMA0ADDAH
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
	lda #BANK_MAIN  ; set data bank to main
	pha
	plb

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

.proc LoadOverworldMapData
	; Here we decompress the map data.  The map is 256x256, but we will only load 64 rows
	; at a time, centered around the player's Y coordinate.  Each map row will go into the
	; "slot" of the actual row mod 64, so as you scroll up and down, the rows behind you
	; get cycled out for rows in front of you.
	TempMaxY = $00              ; Keep track of the maximum Y position
	rep #$20                    ; A to 16-bit
	lda MAPPOSY				    ; get the player's Y position
	lsr                         ; divide by 16 to get the tile position
	lsr
	lsr
	lsr
	sep #$30                    ; A,X,Y to 8-bit.  We'll wrap around the world correctly.
	clc
	adc #$1e                    ; max Y position is 30 rows higher (farther down)
	sta TempMaxY
	clc
	sbc #$3f                    ; min Y position is 63 rows below max.  We go farther up than
	tay                         ; down because we can see farther up due to the perspective.
@Loop:
	jsr DecompressMapRow           ; Decompress one row
	jsr CopyMapRowToBuffer         ; Copy the decompressed row to the tile map buffer
	jsr CopyTileMapBufferRowToVRAM ; Put it in VRAM
	iny
	cpy TempMaxY
	bne @Loop
	stz TileMapBufferDirty         ; this gets set by all the buffering, but we don't want to
	                               ; copy the buffer again when we enable NMI
	rts
.endproc

.proc DecompressMapRow
	; Map rows are referenced by a pointer table, 2 bytes per row, and the rows
	; themselves are RLE compressed.  There are 128 possible tiles, so the lower 7 bits
	; represent the tile that appears next.  If the MSB is not set, then the byte simply
	; represents one tile.  If the MSB is set, then the following byte is the number of
	; times the tile should appear.  A value of 0 means the tile appears 256 times, and
	; takes up the entire row.  Rows are terminated by $FF, even if the whole row is the
	; same tile.
	;
	; The value in the Y register is the row we want to decompress.
	; We will decompress it into the (Y mod 64)th row of uncompressed map data.
	TempSrc = $02              ; Keep track of where we're reading from
	phy                        ; Save Y which will definitely be overwritten by decoding
	php                        ; Save the bitness of the registers
	sep #$20                   ; Set A to 8-bit
	lda #BANK_OVERWORLD        ; Set data bank to overworld source
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
	lda #BANK_OVERWORLD        ; Set data bank to overworld source
	pha
	plb
	lda CompressedOverworld, X
	inx
	cmp #$ff                   ; are we done?
	bne :+
	plp                        ; Restore the bitness before
	ply                        ; restoring Y, otherwise we pop the wrong number of bytes
	rts
:
	pha                        ; remember the tile
	cmp #$80                   ; test the MSB
	bcs :+
                               ; if we're here, just one tile
	lda #OverworldMapBank      ; set data bank to overworld destination
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
	lda #OverworldMapBank      ; set data bank to overworld destination
	pha
	plb
	pla                        ; get the tile back
	and #$7f                   ; clear the repeat flag
@LoopRepeat:
	sta OverworldMap, Y        ; store it
	iny                        ; bump it
	dex                        ; loop it
	bne @LoopRepeat
	ldx TempSrc                ; restore where we were reading from
	jmp @Loop
.endproc

.proc CopyMapRowToBuffer
	; The value in the Y register is the row we want to copy.
	; We will copy the (Y mod 64)th row of uncompressed map data into the tile map buffer.
	; We need to do a lot of math, so we'll be using variables here instead of juggling
	; registers constantly.
	TempRowPointer    = $02
	TempBufferPointer = $04
	TempCounter       = $06
	TempRowPos        = $08
	phy
	php
	rep #$20                   ; A 16-bit
	tya
	and #$3f                   ; row mod 64
	sep #$20                   ; A 8-bit
	pha                        ; save the row index
	lda #$01                   ; 1 means the dirty buffer contains a row
	sta TileMapBufferDirty
	pla                        ; restore the row index
	sta TileMapBufferIndex
	rep #$20                   ; A 16-bit
	xba                        ; multiply by the row length (256)
	sta TempRowPointer
	lda MAPPOSX                ; get the X position, divide by 16 to get the tile position
	lsr
	lsr
	lsr
	lsr
	clc
	sbc #$001f                 ; subtract 31 to get the leftmost tile to copy
	and #$00ff                 ; if we underflowed, we just want to wrap
	clc
	adc TempRowPointer         ; add the row pointer, now we have the index of the leftmost tile
	sta TempBufferPointer      ; save the buffer pointer
	stz TempCounter            ; loop variable
	rep #$10                   ; X,Y to 16-bit
	sep #$20                   ; A to 8-bit
@Loop:
	lda #OverworldMapBank      ; set data bank for the decompressed map
	pha
	plb
	rep #$20                         ; A to 16-bit
	lda TempBufferPointer            ; We need to calculate the tile index in the decompressed map.
	and #$00ff                       ; just the x-coordinate
	adc TempCounter                  ; This is from the start position, plus our loop counter,
	and #$00ff                       ; and we have to wrap around to stay in the same row.
	sta TempRowPos                   ; hang onto this for later
	lda TempBufferPointer            ; now get the buffer pointer again
	and #$ff00                       ; just the row
	clc
	adc TempRowPos                   ; add the row position
	tax                              ; put it in X
	lda OverworldMap, X              ; get the tile
	and #$00ff                       ; just one byte
	tay                              ; put it in Y
	sep #$20                         ; A to 8-bit
	lda #BANK_MAIN                   ; set data bank for the tilemaps
	pha
	plb
	lda OverworldTilemaps, Y         ; get the upper-left character
	pha                              ; save it
	lda OverworldTilemaps + $80, Y   ; get the upper-right character
	pha                              ; save it
	lda OverworldTilemaps + $100, Y  ; get the lower-left character
	pha                              ; save it
	lda OverworldTilemaps + $180, Y  ; get the lower-right character
	pha                              ; save it
	rep #$20                         ; A back to 16-bit
	lda TempRowPos                   ; what's our position in the row
	and #$3f                         ; mod 64
	asl                              ; times 2, this is our index into the buffer
	tay                              ; put it in Y
	sep #$20                         ; A back to 8-bit
	pla                              ; grab the lower-right character
	sta TileMapBuffer + $81, Y       ; store it to the buffer
	pla                              ; grab the lower-left character
	sta TileMapBuffer + $80, Y       ; store it to the buffer
	pla                              ; grab the upper-right character
	sta TileMapBuffer + 1, Y         ; store it to the buffer
	pla                              ; grab the upper-left character
	sta TileMapBuffer, Y             ; store it to the buffer
	inc TempCounter
	lda TempCounter
	cmp #$40                         ; do this 64 times
	bne @Loop

	lda #BANK_MAIN                   ; back to main bank
	pha
	plb
	plp
	ply
	rts
.endproc

.proc CopyTileMapBufferRowToVRAM
	; We're going to copy 64 map tiles into VRAM.  The buffer is laid out so that the
	; tiles are aligned with the Mode 7 scroll, so there's no offsetting necessary
	; here, and the two rows of tilemaps are contiguous in memory.  So one DMA shot
	; straight into VRAM, 256 bytes.
	; The Y register holds the map row to copy from.
	php
	rep #$20                    ; A to 16-bit
	tya
	and #$003f                  ; row mod 64
	xba                         ; times 256
	sta VMADDL                  ; is the VRAM address
	rep #$10                    ; X,Y to 16-bit
	sep #$20                    ; A to 8-bit
	stz VMAINC                  ; VRAM increment on write to VMDATAL
	stz MDMAEN                  ; reset DMA

	lda #<VMDATAL               ; write to VRAM low register (Mode 7 tilemaps)
	sta DMA0ADDB
	ldx #TileMapBuffer
	stx DMA0ADDAL               ; read from the tilemap buffer
	lda #OverworldMapBank       ; which is in this bank
	sta DMA0ADDAH
	ldx #$0100                  ; write 256 bytes
	stx DMA0AMTL
	stz DMA0PARAM               ; configure DMA0 for A->B, inc A address, 1 byte to 1 register

	lda #$01                    ; DMA0 channel
	sta MDMAEN                  ; enable
	plp
	rts
.endproc

.proc CopyMapColumnToBuffer
	; The value in the X register is the row we want to copy.
	; This will be a little more straightforward than copying rows, because we can just start from
	; the first decompressed map row and go to the last one; the correct rows are already in the
	; correct place in the decompressed map data.
	TempBufferPointerLeft  = $04
	TempBufferPointerRight = $06
	TempColPos             = $08
	php
	rep #$20                   ; A 16-bit
	lda #TileMapBuffer
	sta TempBufferPointerLeft  ; initialize the buffer pointer
	clc
	adc #$80                   ; initialize the other buffer pointer
	sta TempBufferPointerRight
	sep #$20                   ; A 8-bit
	txa
	sta TempColPos             ; save this
	and #$3f                   ; mod 64 will be the column index into VRAM
	sta TileMapBufferIndex     ; store the index
	lda #$02                   ; 2 means the buffer has a column
	sta TileMapBufferDirty

	rep #$10                   ; X,Y to 16-bit
	ldy #$00
@Loop:
	rep #$20                   ; A 16-bit
	tya                        ; the row index
	xba                        ; times 256
	adc TempColPos             ; plus the column index
	tax                        ; is the tile index

	sep #$20                   ; A 8-bit
	lda #OverworldMapBank      ; set data bank for the decompressed map
	pha
	plb
	rep #$20                   ; A 16-bit
	lda OverworldMap, X        ; get the tile
	and #$00ff                 ; just one byte
	tax                        ; put it in X

	sep #$20                         ; A 8-bit
	lda #BANK_MAIN                   ; set data bank for the tilemaps
	pha
	plb
	lda OverworldTilemaps, X         ; get the upper-left character
	sta (TempBufferPointerLeft)      ; save it
	inc TempBufferPointerLeft
	lda OverworldTilemaps + $100, X  ; get the lower-left character
	sta (TempBufferPointerLeft)      ; save it
	inc TempBufferPointerLeft
	lda OverworldTilemaps + $80, X   ; get the upper-right character
	sta (TempBufferPointerRight)     ; save it
	inc TempBufferPointerRight
	lda OverworldTilemaps + $180, X  ; get the lower-right character
	sta (TempBufferPointerRight)     ; save it
	inc TempBufferPointerRight

	iny                              ; next row
	cpy #$40                         ; do this 64 times
	bne @Loop

	lda #BANK_MAIN                   ; back to main bank
	pha
	plb
	plp
	rts
.endproc

.proc CopyTileMapBufferColumnToVRAM
	; Same as the above function, except we're doing a column of map tiles.  This will
	; need to be two DMA transfers, one for each column of 8x8 characters.
	; The X register holds the map column to copy from.
	php
	rep #$20                    ; A to 16-bit
	txa
	and #$003f                  ; column mod 64
	asl                         ; times 2
	sta VMADDL                  ; is the VRAM address for the left column
	pha                         ; save this for later

	rep #$10                    ; X,Y to 16-bit
	sep #$20                    ; A to 8-bit
	lda #$02
	sta VMAINC                  ; increment VRAM address by $80 (one row)
	stz MDMAEN                  ; reset DMA
	lda #<VMDATAL               ; write to VRAM low register (Mode 7 tilemaps)
	sta DMA0ADDB
	ldy #TileMapBuffer
	sty DMA0ADDAL               ; read from the tilemap buffer
	lda #OverworldMapBank       ; which is in this bank
	sta DMA0ADDAH
	ldy #$0080                  ; write 128 bytes
	sty DMA0AMTL
	stz DMA0PARAM               ; configure DMA0 for A->B, inc A address, 1 byte to 1 register
	lda #$01                    ; DMA0 channel
	sta MDMAEN                  ; enable

	; Copy the right column
	rep #$20                    ; A to 16-bit
	pla                         ; fetch the VRAM address again
	adc #$01                    ; add 2 bytes to move over one column
	sta VMADDL                  ; this is the VRAM address for the right column
	sep #$20                    ; A to 8-bit
	stz MDMAEN                  ; reset DMA
;   I think this can all be skipped because it's already configured above
;	lda #<VMDATAL               ; write to VRAM low register (Mode 7 tilemaps)
;	sta DMA0ADDB
;	ldy #TileMapBuffer
;	sty DMA0ADDAL               ; read from the tilemap buffer
;	lda #OverworldMapBank       ; which is in this bank
;	sta DMA0ADDAH
;   We need to reset the byte count, though
	ldy #$0080                  ; write 128 bytes
	sty DMA0AMTL
;	stz DMA0PARAM               ; configure DMA0 for A->B, inc A address, 1 byte to 1 register
	lda #$01                    ; DMA0 channel
	sta MDMAEN                  ; enable

	plp
	rts
.endproc

.proc CopyTileMapBufferToVRAM
	; Check if the tilemap buffer is dirty, then determine whether to
	; copy a row or column, and do so.
	php
	sep #$20                    ; A 8-bit
	rep #$10                    ; X,Y 16-bit
	lda TileMapBufferDirty
	cmp #$01
	bne CheckColumn
	ldy TileMapBufferIndex
	jsr CopyTileMapBufferRowToVRAM
	bra Done

CheckColumn:
	cmp #$02
	bne Done
	ldx TileMapBufferIndex
	jsr CopyTileMapBufferColumnToVRAM
	; bra Done

Done:
	stz TileMapBufferDirty      ; zero the dirty flag
	plp
	rts
.endproc

.proc LoadOverworldSprites
	; There are a 12 character classes and 3 vehicles, each with 6 frames of animation.
	; Each of these is a 16x16 sprite taking up 128 bytes.  All told, 12 KB of data
	; (with room at the end for one more set of 6).
	sep #$20                      ; A to 8-bit
	rep #$10                      ; X,Y to 16-bit
	stz MDMAEN                    ; reset DMA
	lda #$80                      ; VRAM increment on write to VMDATAH
	sta VMAINC
	ldx #$4000
	stx VMADDL                    ; start at VRAM address $4000
	lda #<VMDATAL                 ; write to VRAM low register
	sta DMA0ADDB
	ldx #OverworldSprites & $ffff ; OverworldSprites address is long
	stx DMA0ADDAL                 ; read from overworld sprite data
	lda #BANK_OWSPRITE            ; which is in this bank
	sta DMA0ADDAH
	ldx #$3000                    ; write 12 KB (128 bytes * 6 frames * 16 classes/vehicles)
	stx DMA0AMTL
	lda #$01
	sta DMA0PARAM                 ; configure DMA0 for A->B, inc A address, 2 bytes to 2 registers (VMDATAL/H)
	sta MDMAEN                    ; enable
	rts
.endproc

.proc LoadOverworldSpritePalette
	rep #$20        ; set A to 16-bit so we can
	lda #$0000      ; clear the high byte
	sep #$20        ; set A to 8-bit
	rep #$10        ; set X,Y to 16-bit
	lda #BANK_MAIN  ; set data bank to main
	pha
	plb

	lda #$80
	sta CGADD       ; start at CGRAM address $80
	ldx #$0000
Loop:
	lda OverworldSpritePalette, X ; get a byte of palette data
	sta CGDATA                    ; write it to CGRAM
	inx
	cpx #$0020                    ; length of palette data
	bne Loop

	rts
.endproc

.proc SetupVideo
	sep #$20                ; set A to 8-bit

	lda #$07
	sta BGMODE
	stz M7SEL               ; wrap tiles, no flipping
	jsr SetMode7Matrix      ; set up Mode 7 transform parameters

	lda #$62                ; sprites are 16x16 and 32x32, sprite RAM is at $8000 (but word address $4000)
	sta OBJSEL

	lda #$11                ; enable BG1 and sprites
	sta TM

	lda #$0f
	sta INIDISP             ; release forced blanking, set screen to full brightness

	rts
.endproc

.proc DoOverworldMovement
	rep #$20                ; set A to 16-bit
	sep #$10                ; set X,Y to 8-bit

	ldx MOVEDIR             ; see if we're already moving
	bne Move

	; check the dpad, if any of the directional buttons are pressed,
	; move the screen accordingly
CheckUpButton:
	lda JoyPad1                         ; read joypad buttons pressed
	and #BUTTON_UP
	beq CheckDownButton
	ldx #DirUp
	stx FACEDIR
	; check if we are able to move up
	stx MOVEDIR

CheckDownButton:
	lda JoyPad1
	and #BUTTON_DOWN
	beq CheckLeftButton
	ldx #DirDown
	stx FACEDIR
	; check if we are able to move down
	stx MOVEDIR

CheckLeftButton:
	lda JoyPad1
	and #BUTTON_LEFT
	beq CheckRightButton
	ldx #DirLeft
	stx FACEDIR
	; check if we are able to move left
	stx MOVEDIR

CheckRightButton:
	lda JoyPad1
	and #BUTTON_RIGHT
	beq Done
	ldx #DirRight
	stx FACEDIR
	; check if we are able to move right
	stx MOVEDIR

Move:
	ldx MOVEDIR

MoveUp:
	cpx #DirUp
	bne MoveDown
	lda MAPPOSY
	dec
	and #$0fff       ; wrap around
	sta MAPPOSY
	jmp DoneMoving

MoveDown:
	cpx #DirDown
	bne MoveLeft
	lda MAPPOSY
	inc
	and #$0fff       ; wrap around
	sta MAPPOSY
	jmp DoneMoving

MoveLeft:
	cpx #DirLeft
	bne MoveRight
	lda MAPPOSX
	dec
	and #$0fff       ; wrap around
	sta MAPPOSX
	jmp DoneMoving

MoveRight:
	cpx #DirRight
	bne Done
	lda MAPPOSX
	inc
	and #$0fff       ; wrap around
	sta MAPPOSX
	jmp DoneMoving

DoneMoving:
	and #$000f       ; get the count of pixels walked
	cmp #$0008       ; offset by 8 because we're in the "center" of a tile
	bne Done         ; if it wasn't evenly divisible by 16, we're still walking
	ldx #$00         ; otherwise we stop walking
	stx MOVEDIR
	jsr LandedOnSquare

Done:
	jsr SetOverworldCharacterObj
	rts
.endproc

.proc LandedOnSquare
	; First thing is to load map data into WRAM and map graphics into VRAM.
	; We decompress a new map row into WRAM if we were walking up or down.
	sep #$20                  ; set A to 8-bit
	lda FACEDIR
CheckUp:
	cmp #DirUp
	bne CheckDown
	rep #$20                  ; A to 16-bit
	lda MAPPOSY               ; we have to calculate the Y coordinate
	lsr
	lsr
	lsr
	lsr
	sep #$20                  ; A back to 8-bit
	clc
	sbc #$21                  ; 33 rows up
	tay                       ; calling convention for passing the row
	jsr DecompressMapRow
	jsr CopyMapRowToBuffer
	jmp CheckEvent

CheckDown:
	cmp #DirDown
	bne CheckLeft
	rep #$20                  ; A to 16-bit
	lda MAPPOSY               ; we have to calculate the Y coordinate
	lsr
	lsr
	lsr
	lsr
	sep #$20                  ; A back to 8-bit
	clc
	adc #$1e                  ; 30 rows down
	tay                       ; calling convention for passing the row
	jsr DecompressMapRow
	jsr CopyMapRowToBuffer
	jmp CheckEvent

	; If we're going left or right, the map data is already loaded, we just need
	; to copy graphics into VRAM.
CheckLeft:
	cmp #DirLeft
	bne CheckRight
	rep #$20                  ; A to 16-bit
	lda MAPPOSX               ; we have to calculate the X coordinate
	lsr
	lsr
	lsr
	lsr
	sep #$20                  ; A back to 8-bit
	clc
	sbc #$1f                  ; 31 columns left
	tax                       ; calling convention for passing the column
	jsr CopyMapColumnToBuffer
	jmp CheckEvent
CheckRight:
	cmp #DirRight             ; theoretically, we don't need this
	bne CheckEvent            ; or this
	rep #$20                  ; A to 16-bit
	lda MAPPOSX               ; we have to calculate the X coordinate
	lsr
	lsr
	lsr
	lsr
	sep #$20                  ; A back to 8-bit
	clc
	adc #$20                  ; 32 columns right
	tax                       ; calling convention for passing the column
	jsr CopyMapColumnToBuffer
	jmp CheckEvent

	; Then we check to see if we triggered an event, like entering a cave or town.
CheckEvent:
	; Finally, check for an enemy encounter.
CheckEncounter:
	rts
.endproc

.proc SetOverworldCharacterObj
	sep #$20             ; set A to 8-bit
	lda #$78             ; player sprite position is $78, $68
	sta OamMirror
	lda #$68
	sta OamMirror + 1
	lda FACEDIR          ; get the facing direction
	cmp #DirRight        ; if we're facing right,
	bne :+
	dec                  ; then load the facing left sprite,
	pha                  ; save it
	lda #$50             ; and flip the sprite horizontally
	sta OamMirror + 3
	pla                  ; recall the sprite
	jmp FlipDone
:
	pha
	lda #$10             ; otherwise no flip, 1 priority, 0th palette
	sta OamMirror + 3
	pla                  ; recall the sprite
FlipDone:
	dec                  ; minus 1
	asl                  ; times 2, since there are 2 frames of animation
	asl                  ; times 2 again?
	pha                  ; save this, this is the sprite we want
	rep #$20             ; A to 16-bit
	lda MAPPOSX          ; we need to see if we should load frame 2
	and #$0008           ; see if we've walked 8 pixels, result will be 0 if we have because we're offset by 8
	bne :+
	sep #$20             ; A back to 8-bit
	pla                  ; fetch the sprite
	inc                  ; we want the next one
	inc
	jmp AnimationDone
:
.a16                     ; need the hint here that A is still 16-bit
	lda MAPPOSY          ; we might be walking vertically
	and #$0008
	bne :+
	sep #$20             ; A back to 8-bit
	pla                  ; fetch the sprite
	inc                  ; we want the next one
	inc
	jmp AnimationDone
:
	sep #$20             ; A back to 8-bit
	pla                  ; fetch the sprite
AnimationDone:
	sta OamMirror + 2
	lda #$01             ; reset the high bit of the x position
	trb OamMirror + $200
	rts
.endproc

.proc SetMode7Matrix
	sep #$20          ; set A to 8-bit
	lda #BANK_MAIN    ; set data bank to main (where the trig tables are)
	pha
	plb

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
