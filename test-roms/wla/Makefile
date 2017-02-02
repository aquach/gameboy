.PHONY: default all clean

C_TARGETS = $(patsubst %.c, %.gb, $(wildcard *.c))
ASM_TARGETS = $(patsubst %.s, %.gb, $(wildcard *.s))

all: $(C_TARGETS) $(ASM_TARGETS)

%.o: %.s
	wla-gb $<

%.gb: %.o
	wlalink linkfile test.gb

clean:
	rm -f *.gb *.o
