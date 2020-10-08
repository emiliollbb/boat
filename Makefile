FLAGS +=
LDFLAGS += -Cnes.cfg

AS65 ?= ca65
LD65 ?= ld65

ASSEMBLE = $(AS65) $(FLAGS)
LINK = $(LD65) $(LDFLAGS) $(FLAGS)
CHR = python3 ${NES_CHR_ENCODE}/nes_chr_encode.py

mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
dirname := $(basename $(notdir $(patsubst %/,%,$(dir $(mkfile_path)))))

SOURCES = $(wildcard *.asm)
OBJECTS =  $(SOURCES:.asm=.o)

EXECUTABLE = boat.nes

.PHONY: all clean

all: $(EXECUTABLE)
clean:
	-rm -v $(OBJECTS) $(EXECUTABLE) && rm -f tiles.chr tiles.png

tiles.chr: sprites_tiles.png background_tiles.png
	rm -f tiles.chr tiles.png && convert -append sprites_tiles.png background_tiles.png tiles.png && ${CHR} tiles.png tiles.chr

$(EXECUTABLE): $(OBJECTS) nes.cfg
	$(LINK) -o$@ $<

%.o : %.asm tiles.chr
	$(ASSEMBLE) -o$@ $<
