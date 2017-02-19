TARGET = gameboy
LIBS = -lm $(shell sdl2-config --libs)
CC = gcc
CFLAGS = -g -Wall -std=c99 $(shell sdl2-config --cflags)

.PHONY: default all clean

default: $(TARGET)
all: default

OBJECTS = $(patsubst %.c, %.o, $(wildcard *.c))
HEADERS = $(wildcard *.h)

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

$(TARGET): $(OBJECTS)
	$(CC) $(OBJECTS) -Wall $(LIBS) -o $@

blargg:
	cd test-roms/blargg/cpu_instrs/source && make

blargg-cpu-individual-test: blargg default
	ls test-roms/blargg/cpu_instrs/source/*.gb | grep -v 02 | xargs -l ./gameboy

blargg-cpu-test: blargg
	./gameboy test-roms/blargg/cpu_instrs/cpu_instrs.gb

wla:
	cd test-roms/wla && make

wla-test: wla default
	./gameboy test-roms/wla/test.gb

clean:
	-rm -f *.o
	-rm -f $(TARGET)
