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

halt
ld a, 0
call delay_a_20_cycles
stop

delay_a_20_cycles:
-    sub  5    ; 2
		 jr   nc,- ;3/2 do multiples of 5
		 rra       ; 1
		 jr   nc,+ ;3/2 bit 0
+    adc  1    ; 2
		 ret  nc   ;5/2 -1: 0 cycles
		 ret  z    ;5/2  0: 2 cycles
		 nop       ; 1   1: 4 cycles
		 ret       ; 4 (thanks to dclxvi for original algorithm)

