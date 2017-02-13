.PHONY: default all clean

ASM_TARGETS = $(patsubst %.s, %.gb, $(wildcard *.s))

all: test.gb

%.o: %.s
	wla-gb $<

%.gb: %.o
	wlalink linkfile test.gb

clean:
	rm -f *.gb *.o
