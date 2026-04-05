.p816
.a8
.i16

.include "defines.inc"
.include "registers.inc"
.include "joypad.inc"

.segment "CODE"

; The "Final Fantasy Superizer" on the title screen is sprites, so that
; they can be rendered over the overworld in Mode 7.  To simplify this,
; we can just bulk load the sprite configuration data from ROM.
.proc LoadTitleScreenSprites

.endproc
