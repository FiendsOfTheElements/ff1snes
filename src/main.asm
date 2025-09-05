.p816
.a8
.i16

.segment "TITLE"
	.byte "FF1SNES             " ; 20 characters available here, do not pad with zeroes

.segment "ROMINFO"
	.byte $00 ; Use ROMINFOEX
	.byte $20 ; LoROM, SlowROM
	.byte $02 ; Battery SRAM, no co-processor
	.byte $07 ; 128K ROM (we'll increase this later)
	.byte $01 ; 2K SRAM (can increase if needed)
	.byte $01 ; North America (there doesn't seem to be a standard for "any region")
	.byte $33 ; Developer ID ($33 means use ROMINFOEX)
	.byte $00 ; Original version (1.0)
	.word $AAAA, $5555 ; Checksum (placeholder values)

.segment "ROMINFOEX"
	.byte "FE"   ; (Fiends of the Elements)
	.byte "FF1S" ; Game code
	.byte $00, $00, $00, $00, $00, $00 ; Unused
	.byte $00 ; No Flash ROM
	.byte $00 ; No Expansion RAM
	.byte $00 ; Not a special version
	.byte $00 ; No co-processor

.segment "VECTORS"
	; native mode
	;     NOT    USED   COP,   BRK,   ABT,   NMI,        RST,          IRQ
	.addr $0000, $0000, $0000, $0000, $0000, NMIHandler, $0000,        $0000

	; emulation mode
	;     NOT    USED   COP,   BRK,   ABT,   NMI,        RST,          IRQ
	.addr $0000, $0000, $0000, $0000, $0000, $0000,      ResetHandler, $0000

.import GetJoypadInputs

.import LoadOverworld
.import DoOverworldMovement
.import SetMode7Matrix

.include "registers.inc"
.include "defines.inc"

.segment "CODE"

.proc   ResetHandler
	sei                     ; disable interrupts
	clc                     ; clear the carry flag
	xce                     ; switch the 65816 to native (16-bit mode)
	rep #$10                ; set X and Y to 16-bit
	sep #$20                ; set A to 8-bit

	ldx #$1fff              ; set the stack pointer to $1fff
	txs

	jsr InitializeOam
	jsr LoadOverworld

	jmp GameLoop            ; all initialization is done
.endproc

.proc InitializeOam
	ldx #$1ff               ; size of main OAM
@BaseLoop:
	stz OamMirror, X        ; zero out main OAM
	dex
	bpl @BaseLoop

	lda #$55                ; this sets the sprite size to 0 and puts the sprite off the right side of the screen
	ldx #$1f                ; size of upper OAM
@UpperLoop:
	sta OamMirror + $200, X
	dex
	bpl @UpperLoop

	rts
.endproc

.proc   GameLoop
	wai                     ; wait for NMI / V-Blank

	jsr GetJoypadInputs
	jsr DoOverworldMovement

	jmp GameLoop
.endproc

.proc   NMIHandler
	php
	rep #$10                ; set X and Y to 16-bit
	sep #$20                ; set A to 8-bit

	lda RDNMI               ; read NMI status, acknowledge NMI

	stz MDMAEN                  ; reset DMA
	ldx #$0000
	stx OAMADDL                 ; start at OAM address 0
	lda #<OAMDATA               ; write to OAM
	sta DMA7ADDB
	ldx #OamMirror              ; OamMirror is a short address
	stx DMA7ADDAL               ; read from OamMirror
	stz DMA7ADDAH               ; doesn't matter which bank
	ldx #$220                   ; write 544 bytes
	stx DMA7AMTL
	stz DMA7PARAM               ; configure DMA7 for A->B, inc A address, 1 byte to 1 register
	lda #$80                    ; enable DMA7
	sta MDMAEN

	jsr SetMode7Matrix

	plp
	rti
.endproc
