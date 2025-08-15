; COLOR
;
; Given three integers ranging from 0 to 31, outputs a 16-bit value representing
; the equivalent SNES color value.
.macro COLOR
    .dw (\3 << 10) + (\2 << 5) + \1
.endm
