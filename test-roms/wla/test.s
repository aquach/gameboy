.memorymap
slotsize $4000
slot 0 $0000
slot 1 $4000
defaultslot 0
.endme

.rombankmap
bankstotal 2
banksize $4000
banks 2
.endro

.orga $0100

ld a, $0
ldh ($6), a

ld a, $4
ldh ($7), a

ld a, $4
ldh ($ff), a

ei
nop

call loop

loop:
	jp loop

