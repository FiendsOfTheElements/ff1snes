SRC = $(wildcard src/*.asm)
OBJ = $(patsubst src/%.asm, obj/%.o, $(SRC))
INC = $(wildcard includes/*.inc)

demo.smc: $(OBJ)
	ld65 -C memory-map.cfg $(OBJ) -o z2snes.smc

rip:
	dotnet run --project utils/ff1ripper
	mv *.4bpp assets/graphics
	mv *.bin assets/data

obj/%.o: src/%.asm $(INC) rip
	@mkdir -p obj
	ca65 --cpu 65816 --include-dir includes --bin-include-dir assets --smart -o $@ $<

clean:
	rm obj/*
	rm assets/data/*
	rm assets/graphics/*
