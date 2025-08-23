SRC = $(wildcard src/*.asm)
OBJ = $(patsubst src/%.asm, obj/%.o, $(SRC))
INC = $(wildcard includes/*.inc)

demo.smc: $(OBJ)
	ld65 -C memory-map.cfg $(OBJ) -o z2snes.smc

obj/%.o: src/%.asm $(INC)
	@mkdir -p obj
	ca65 --cpu 65816 --include-dir includes --bin-include-dir assets --smart -o $@ $<

clean:
	rm obj/*
