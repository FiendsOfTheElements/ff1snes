.p816
.a8
.i16

.import GetJoypadInputs

.import LoadOverworld
.import DoOverworldMovement
.import SetMode7Matrix

.include "registers.inc"

.segment "CODE"

.proc   ResetHandler
	sei                     ; disable interrupts
	clc                     ; clear the carry flag
	xce                     ; switch the 65816 to native (16-bit mode)
	rep #$10                ; set X and Y to 16-bit
	sep #$20                ; set A to 8-bit

	ldx #$1fff              ; set the stack pointer to $1fff
	txs

	jsr LoadOverworld

	jmp GameLoop            ; all initialization is done
.endproc

.proc   GameLoop
	wai                     ; wait for NMI / V-Blank

	jsr GetJoypadInputs
	jsr DoOverworldMovement

	jmp GameLoop
.endproc

.proc   NMIHandler
	lda RDNMI               ; read NMI status, acknowledge NMI

	jsr SetMode7Matrix

	rti
.endproc



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
	.word             $0000, $0000    ; four unused bytes
	; native mode
	;     NOT    USED   COP,   BRK,   ABT,   NMI,        RST,          IRQ
	.addr $0000, $0000, $0000, $0000, $0000, NMIHandler, $0000,        $0000

	; emulation mode
	;     NOT    USED   COP,   BRK,   ABT,   NMI,        RST,          IRQ
	.addr $0000, $0000, $0000, $0000, $0000, $0000,      ResetHandler, $0000
