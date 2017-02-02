#include <gb/gb.h>
#include <gb/hardware.h>

void main(void) {
  short a = 1;
  short b = 1;
  short new;

  while (b < 130) {
    SB_REG = 5;
    SC_REG = 1;
    SB_REG = a;
    SC_REG = 1;
    SB_REG = b;
    SC_REG = 1;

    new = a + b;
    a = b;
    b = new;
  }
}
