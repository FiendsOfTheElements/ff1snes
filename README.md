# ff1snes

A port of Final Fantasy to the SNES.  Written from the ground up: No code was copied from the original NES game, though the disassembly was certainly used as a reference.

## Building
To build the ROM, you'll need cc65 and make.  You'll also need an FF1 ROM, which you can place in the root directory and name `ff1.nes`.
The necessary data will be automatically extracted and compiled into the SNES ROM.

Just run `make` from the command line.

**Note:** We want the Makefile to eventually run the ff1ripper scripts as .cs files straight from the command line, this will require .NET 10.
