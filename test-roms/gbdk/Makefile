.PHONY: default all clean

GBDK_DIR = ~/Downloads/gbdk/

C_TARGETS = $(patsubst %.c, %.gb, $(wildcard *.c))
ASM_TARGETS = $(patsubst %.s, %.gb, $(wildcard *.s))

all: $(C_TARGETS) $(ASM_TARGETS)

%.gb: %.c
	GBDKDIR=${GBDK_DIR} ${GBDK_DIR}bin/lcc -o $@ $<

%.gb: %.s
	GBDKDIR=${GBDK_DIR} ${GBDK_DIR}bin/lcc -o $@ $<

clean:
	-rm -f *.gb
