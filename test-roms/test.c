#include <gb/gb.h>
#include <gb/hardware.h>

void main(void) {
  int a = 1;
  int b = 1;
  int new;

  while (b < 130) {
    SB_REG = 0;
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
