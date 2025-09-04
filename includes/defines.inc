; COLOR
;
; Given three integers ranging from 0 to 31, outputs a 16-bit value representing
; the equivalent SNES color value.
.macro COLOR r, g, b
    .word (b << 10) + (g << 5) + r
.endmacro

BANK_MAIN      = $00
BANK_OVERWORLD = $02
BANK_OWSPRITE  = $03

; WRAM locations CAN overlap, but only if the bits of code using the overlap are okay with it!

; Low WRAM locations
; We don't have to define EVERY temp variable here.  It is expected that procs will define their own
; temp variables, and that module code is kept reasonably close together so they can coordinate.
JoyPad1          = $0300     ; data read from joypad 1
JoyTrigger1      = $0302     ; trigger read from joypad 1
JoyHeld1         = $0304     ; held buttons read from joypad 1

OamMirror        = $05E0     ; to $07FF, this gets DMA'd to OAM every frame

TileMapBuffer    = $1800     ; this is a buffer from which a row or column of tiles will be DMA'd into VRAM, 256 bytes

StackBase        = $1E00     ; expected to be no more than 512 bytes, grows from $1FFF downward

; High WRAM locations
OverworldMapBank = $7E
OverworldMap     = $4000     ; store the decompressed overworld map from $7E:4000-7E:7FFF (64 rows x 256 columns, 4096 bytes)
