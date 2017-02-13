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

.orga $0040

; stop

.orga $0100

DI

; Enable VBlank.
; ld hl, $ffff
; ld (hl), 1

; ld hl, $ff41
; ld (hl), $c

; LCDC
ld hl, $ff40
ld (hl), $82

; OBP1
ld hl, $ff48
ld (hl), $e4

; Tile.
ld bc, tile
ld hl, $8000
ld e, 16

load:
	ld a, (bc)
	ld (hl), a
	inc hl
	inc bc
	dec e
	jr nz, load

; Sprite.
ld hl, $fe00
ld (hl), $20

ld hl, $fe01
ld (hl), $30

ld hl, $fe02
ld (hl), $0

ld hl, $fe03
ld (hl), $40

EI

loop:
	nop
	jp loop

tile:
	.byte $20, $20, $20, $20, $24, $24, $24, $24, $00, $00, $81, $81, $7e, $7e, $00, $00

