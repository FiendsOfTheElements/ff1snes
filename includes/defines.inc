; COLOR
;
; Given three integers ranging from 0 to 31, outputs a 16-bit value representing
; the equivalent SNES color value.
.macro COLOR r, g, b
    .dword (b << 10) + (g << 5) + r
.endmacro
