#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

typedef struct {
  unsigned char memory[64 * 1024];

  // Pair orderings are important because you can use these in pairs: AF, BC, DE, HL.
  // This assumes little-endian architecture since A should be high when reading FA.
  unsigned char F;
  unsigned char A;

  unsigned char C;
  unsigned char B;

  unsigned char E;
  unsigned char D;

  unsigned char L;
  unsigned char H;

  unsigned short SP;
  unsigned short PC;
} Cpu;

#define CPU_AF_REF(c) ((unsigned short*)&(c)->A)
#define CPU_BC_REF(c) ((unsigned short*)&(c)->B)
#define CPU_DE_REF(c) ((unsigned short*)&(c)->D)
#define CPU_HL_REF(c) ((unsigned short*)&(c)->H)

#define CPU_AF(c) (*CPU_AF_REF(c))
#define CPU_BC(c) (*CPU_BC_REF(c))
#define CPU_DE(c) (*CPU_DE_REF(c))
#define CPU_HL(c) (*CPU_HL_REF(c))

#define BIT(v, index) (((v) >> (index)) & 1)

#define CPU_FLAG_Z(f) BIT((f), 7)
#define CPU_FLAG_N(f) BIT((f), 6)
#define CPU_FLAG_H(f) BIT((f), 5)
#define CPU_FLAG_C(f) BIT((f), 4)

#define CPU_F(z, n, h, c) ((z) << 7 | (n) << 6 | (h) << 5 | (c) << 4)

void add_8_bit(
  unsigned char a,
  unsigned char b,
  unsigned char* result,
  unsigned char* carry,
  unsigned char* half_carry
) {
  unsigned short full_add = (unsigned short)a + (unsigned short)b;
  *result = full_add & 0xff;
  *carry = full_add >> 8;
  *half_carry = (((unsigned short)a & 0xf) + ((unsigned short)b & 0xf)) >> 4;
}

void sub_8_bit(
  unsigned char a,
  unsigned char b,
  unsigned char* result,
  unsigned char* borrow,
  unsigned char* half_borrow
) {
  unsigned char carry;
  unsigned char half_carry;
  add_8_bit(a, 256 - b, result, &carry, &half_carry);
  // 3 - 1 => 3 + 255 => carry flag set, which means borrow flag not set.
  // 3 - 4 => 3 + 252 => carry flag not set, which means borrow flag set.
  *borrow = !carry;
  *half_borrow = !half_carry;
}

void add_16_bit(
  unsigned short a,
  unsigned short b,
  unsigned short* result,
  unsigned char* carry,
  unsigned char* half_carry
) {
  unsigned int full_add = (unsigned int)a + (unsigned int)b;
  *result = full_add & 0xffff;
  *carry = full_add >> 16;
  *half_carry = (((unsigned int)a & 0xfff) + ((unsigned int)b & 0xfff)) >> 12;
}

// TODO: H/C flag may be "no borrow" instead of "did borrow".

void sub_16_bit(
  unsigned short a,
  unsigned short b,
  unsigned short* result,
  unsigned char* borrow,
  unsigned char* half_borrow
) {
  unsigned char carry;
  unsigned char half_carry;
  add_16_bit(a, 65536 - b, result, &carry, &half_carry);
  *borrow = !carry;
  *half_borrow = !half_carry;
}

void cpu_initialize(Cpu* cpu) {
  // TODO
}

void cpu_read_mem(Cpu* cpu, short address, unsigned char* output, int len) {
  memcpy(output, cpu->memory + address, len);
}

void cpu_write_mem(Cpu* cpu, short address, unsigned char* data, int len) {
  if (address == 0xff02) { // SC
    printf("%c", cpu->memory[0xff01]); // SB
  } else {
    memcpy(cpu->memory + address, data, len);
  }
}

void cpu_step_clock(Cpu* cpu) {
    unsigned char opcode;
    cpu_read_mem(cpu, cpu->PC, &opcode, 1);

    int num_cycles;
    switch (opcode) {
      case 0x00:
        // NOP
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x01:
        // LD BC,d16
        cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)CPU_BC_REF(cpu), 2);
        cpu->PC += 3;
        num_cycles = 12;
        break;

      case 0x02:
        // LD (BC),A
        cpu_write_mem(cpu, CPU_BC(cpu), &cpu->A, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x03:
        // INC BC
        *CPU_BC_REF(cpu) = *CPU_BC_REF(cpu) + 1;
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x04:
        // INC B
        {
          unsigned char carry;
          unsigned char half_carry;
          add_8_bit(cpu->B, 1, &cpu->B, &carry, &half_carry);
          cpu->F = CPU_F(cpu->B == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x05:
        // DEC B
        {
          unsigned char borrow;
          unsigned char half_borrow;
          sub_8_bit(cpu->B, 1, &cpu->B, &borrow, &half_borrow);
          cpu->F = CPU_F(cpu->B == 0 ? 1 : 0, 1, half_borrow, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x06:
        // LD B,d8
        cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&cpu->B, 1);
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x07:
        // RLCA
        {
          unsigned char bit = BIT(cpu->A, 7);
          cpu->A = (cpu->A << 1) | bit;
          cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, 0, bit);
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x08:
        // LD (a16),SP
        {
          unsigned short address;
          cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&address, 2);
          cpu_write_mem(cpu, address, (unsigned char*)&cpu->SP, 2);
          cpu->PC += 3;
          num_cycles = 20;
        }
        break;

      case 0x09:
        // ADD HL,BC
        {
          unsigned char carry;
          unsigned char half_carry;
          add_16_bit(CPU_HL(cpu), CPU_BC(cpu), CPU_HL_REF(cpu), &carry, &half_carry);
          cpu->F = CPU_F(CPU_FLAG_Z(cpu->F), 0, half_carry, carry);
          cpu->PC += 1;
          num_cycles = 8;
        }
        break;

      case 0x0a:
        // LD A,(BC)
        cpu_read_mem(cpu, CPU_BC(cpu), &cpu->A, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x0b:
        // DEC BC
        *CPU_BC_REF(cpu) = *CPU_BC_REF(cpu) - 1;
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x0c:
        // INC C
        {
          unsigned char carry;
          unsigned char half_carry;
          add_8_bit(cpu->C, 1, &cpu->C, &carry, &half_carry);
          cpu->F = CPU_F(cpu->C == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x0d:
        // DEC C
        {
          unsigned char borrow;
          unsigned char half_borrow;
          sub_8_bit(cpu->C, 1, &cpu->C, &borrow, &half_borrow);
          cpu->F = CPU_F(cpu->C == 0 ? 1 : 0, 1, half_borrow, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x0e:
        // LD C,d8
        cpu_read_mem(cpu, cpu->PC + 1, &cpu->C, 1);
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x0f:
        // RRCA
        {
          unsigned char bit = BIT(cpu->A, 0);
          cpu->A = (cpu->A >> 1) | (bit << 7);
          cpu->F = CPU_F(0, 0, 0, bit); // TODO: Z flag is a matter of debate. Could be 0, 1, or Z.
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x10:
        // STOP 0
        // TODO
        cpu->PC += 2;
        num_cycles = 4;
        break;

      case 0x11:
        // LD DE,d16
        cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)CPU_DE_REF(cpu), 2);
        cpu->PC += 3;
        num_cycles = 12;
        break;

      case 0x12:
        // LD (DE),A
        cpu_write_mem(cpu, CPU_DE(cpu), &cpu->A, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x13:
        // INC DE
        *CPU_DE_REF(cpu) = *CPU_DE_REF(cpu) + 1;
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x14:
        // INC D
        {
          unsigned char carry;
          unsigned char half_carry;
          add_8_bit(cpu->D, 1, &cpu->D, &carry, &half_carry);
          cpu->F = CPU_F(cpu->D == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x15:
        // DEC D
        {
          unsigned char borrow;
          unsigned char half_borrow;
          sub_8_bit(cpu->D, 1, &cpu->D, &borrow, &half_borrow);
          cpu->F = CPU_F(cpu->D == 0 ? 1 : 0, 1, half_borrow, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x16:
        // LD D,d8
        cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&cpu->D, 1);
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x17:
        // RLA
        {
          unsigned char bit = BIT(cpu->A, 7);
          cpu->A = (cpu->A << 1) | CPU_FLAG_C(cpu->F);
          cpu->F = CPU_F(0, 0, 0, bit); // TODO: Z is up for debate.
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x18:
        // JR r8
        {
          // TODO: is this correct?
          char offset;
          cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&offset, 1);
          cpu->PC = cpu->PC + offset;
          num_cycles = 12;
        }
        break;

      case 0x19:
        // ADD HL,DE
        {
          unsigned char carry;
          unsigned char half_carry;
          add_16_bit(CPU_HL(cpu), CPU_DE(cpu), CPU_HL_REF(cpu), &carry, &half_carry);
          cpu->F = CPU_F(CPU_FLAG_Z(cpu->F), 0, half_carry, carry);
          cpu->PC += 1;
          num_cycles = 8;
        }
        break;

      case 0x1a:
        // LD A,(DE)
        cpu_read_mem(cpu, CPU_DE(cpu), &cpu->A, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x1b:
        // DEC DE
        *CPU_DE_REF(cpu) = *CPU_DE_REF(cpu) - 1;
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x1c:
        // INC E
        {
          unsigned char carry;
          unsigned char half_carry;
          add_8_bit(cpu->E, 1, &cpu->E, &carry, &half_carry);
          cpu->F = CPU_F(cpu->E == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x1d:
        // DEC E
        {
          unsigned char borrow;
          unsigned char half_borrow;
          sub_8_bit(cpu->E, 1, &cpu->E, &borrow, &half_borrow);
          cpu->F = CPU_F(cpu->E == 0 ? 1 : 0, 1, half_borrow, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x1e:
        // LD E,d8
        cpu_read_mem(cpu, cpu->PC + 1, &cpu->E, 1);
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x1f:
        // RRA
        {
          unsigned char bit = BIT(cpu->A, 0);
          cpu->A = (cpu->A >> 1) | (CPU_FLAG_C(cpu->F) << 7);
          cpu->F = CPU_F(0, 0, 0, bit); // TODO: Z flag is up for debate.
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x20:
        // JR NZ,r8
        {
          // TODO: is this correct?
          char offset;
          cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&offset, 1);

          if (!CPU_FLAG_Z(cpu->F)) {
            cpu->PC = cpu->PC + offset;
            num_cycles = 12;
          } else {
            cpu->PC += 2;
            num_cycles = 8;
          }
        }
        break;

      case 0x21:
        // LD HL,d16
        cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)CPU_HL_REF(cpu), 2);
        cpu->PC += 3;
        num_cycles = 12;
        break;

      case 0x22:
        // LD (HL+),A
        cpu_write_mem(cpu, CPU_HL(cpu), &cpu->A, 1);
        *CPU_HL_REF(cpu) = CPU_HL(cpu) + 1;
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x23:
        // INC HL
        *CPU_HL_REF(cpu) = CPU_HL(cpu) + 1;
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x24:
        // INC H
        {
          unsigned char carry;
          unsigned char half_carry;
          add_8_bit(cpu->H, 1, &cpu->H, &carry, &half_carry);
          cpu->F = CPU_F(cpu->H == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x25:
        // DEC H
        {
          unsigned char borrow;
          unsigned char half_borrow;
          sub_8_bit(cpu->H, 1, &cpu->H, &borrow, &half_borrow);
          cpu->F = CPU_F(cpu->H == 0 ? 1 : 0, 1, half_borrow, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x26:
        // LD H,d8
        cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&cpu->H, 1);
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x27:
        // DAA
        // TODO: WTF is this: http://z80-heaven.wikidot.com/instructions-set:daa
        {
          unsigned char low_nibble = cpu->A & 0xf;
          if (low_nibble > 9 || CPU_FLAG_H(cpu->F)) {
            cpu->A += 6;
          }

          unsigned char high_nibble = cpu->A >> 4;
          bool do_second_addition = high_nibble > 9 || CPU_FLAG_C(cpu->F);
          if (do_second_addition) {
            cpu->A += 0x60;
          }

          cpu->F = CPU_F(cpu->A == 0, CPU_FLAG_N(cpu->F), 0, do_second_addition ? 1 : 0);

          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x28:
        // JR Z,r8
        {
          // TODO: is this correct?
          char offset;
          cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&offset, 1);

          if (CPU_FLAG_Z(cpu->F)) {
            cpu->PC = cpu->PC + offset;
            num_cycles = 12;
          } else {
            cpu->PC += 2;
            num_cycles = 8;
          }
        }
        break;

      case 0x29:
        // ADD HL,HL
        {
          unsigned char carry;
          unsigned char half_carry;
          add_16_bit(CPU_HL(cpu), CPU_HL(cpu), CPU_HL_REF(cpu), &carry, &half_carry);
          cpu->F = CPU_F(CPU_FLAG_Z(cpu->F), 0, half_carry, carry);
          cpu->PC += 1;
          num_cycles = 8;
        }
        break;

      case 0x2a:
        // LD A,(HL+)
        cpu_read_mem(cpu, CPU_HL(cpu), &cpu->A, 1);
        *CPU_HL_REF(cpu) = CPU_HL(cpu) + 1;
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x2b:
        // DEC HL
        *CPU_HL_REF(cpu) = CPU_HL(cpu) - 1;
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x2c:
        // INC L
        {
          unsigned char carry;
          unsigned char half_carry;
          add_8_bit(cpu->L, 1, &cpu->L, &carry, &half_carry);
          cpu->F = CPU_F(cpu->L == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x2d:
        // DEC L
        {
          unsigned char borrow;
          unsigned char half_borrow;
          sub_8_bit(cpu->L, 1, &cpu->L, &borrow, &half_borrow);
          cpu->F = CPU_F(cpu->L == 0 ? 1 : 0, 1, half_borrow, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x2e:
        // LD L,d8
        cpu_read_mem(cpu, cpu->PC + 1, &cpu->L, 1);
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x2f:
        // CPL
        cpu->A = ~cpu->A;
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x30:
        // JR NC,r8
        {
          // TODO: is this correct?
          char offset;
          cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&offset, 1);

          if (!CPU_FLAG_C(cpu->F)) {
            cpu->PC = cpu->PC + offset;
            num_cycles = 12;
          } else {
            cpu->PC += 2;
            num_cycles = 8;
          }
        }
        break;

      case 0x31:
        // LD SP,d16
        cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&cpu->SP, 2);
        cpu->PC += 3;
        num_cycles = 12;
        break;

      case 0x32:
        // LD (HL-),A
        cpu_read_mem(cpu, CPU_HL(cpu), &cpu->A, 1);
        *CPU_HL_REF(cpu) = CPU_HL(cpu) - 1;
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x33:
        // INC SP
        cpu->SP++;
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x34:
        // INC (HL)
        {
          unsigned char value;
          cpu_read_mem(cpu, CPU_HL(cpu), &value, 1);
          unsigned char carry;
          unsigned char half_carry;
          add_16_bit(value, 1, CPU_HL_REF(cpu), &carry, &half_carry);
          cpu->F = CPU_F(CPU_HL(cpu) == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 12;
        }
        break;

      case 0x35:
        // DEC (HL)
        {
          unsigned char value;
          cpu_read_mem(cpu, CPU_HL(cpu), &value, 1);
          unsigned char borrow;
          unsigned char half_borrow;
          sub_16_bit(value, 1, CPU_HL_REF(cpu), &borrow, &half_borrow);
          cpu->F = CPU_F(CPU_HL(cpu) == 0 ? 1 : 0, 0, half_borrow, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 12;
        }
        break;

      case 0x36:
        // LD (HL),d8
        {
          unsigned char value;
          cpu_read_mem(cpu, cpu->PC + 1, &value, 1);
          cpu_write_mem(cpu, CPU_HL(cpu), &value, 1);
          cpu->PC += 2;
          num_cycles = 12;
        }
        break;

      case 0x37:
        // SCF
        cpu->F = CPU_F(CPU_FLAG_Z(cpu->F), 0, 0, 1);
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x38:
        // JR C,r8
        {
          // TODO: is this correct?
          char offset;
          cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&offset, 1);

          if (CPU_FLAG_C(cpu->F)) {
            cpu->PC = cpu->PC + offset;
            num_cycles = 12;
          } else {
            cpu->PC += 2;
            num_cycles = 8;
          }
        }
        break;

      case 0x39:
        // ADD HL,SP
        {
          unsigned char carry;
          unsigned char half_carry;
          add_16_bit(CPU_HL(cpu), cpu->SP, CPU_HL_REF(cpu), &carry, &half_carry);
          cpu->F = CPU_F(CPU_FLAG_Z(cpu->F), 0, half_carry, carry);
          cpu->PC += 1;
          num_cycles = 8;
        }
        break;

      case 0x3a:
        // LD A,(HL-)
        cpu_read_mem(cpu, CPU_HL(cpu), &cpu->A, 1);
        *CPU_HL_REF(cpu) = CPU_HL(cpu) - 1;
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x3b:
        // DEC SP
        cpu->SP--;
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x3c:
        // INC A
        {
          unsigned char carry;
          unsigned char half_carry;
          add_8_bit(cpu->A, 1, &cpu->A, &carry, &half_carry);
          cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x3d:
        // DEC A
        {
          unsigned char borrow;
          unsigned char half_borrow;
          sub_8_bit(cpu->A, 1, &cpu->A, &borrow, &half_borrow);
          cpu->F = CPU_F(CPU_FLAG_Z(cpu->F), 1, half_borrow, CPU_FLAG_C(cpu->F));
          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x3e:
        // LD A,d8
        cpu_read_mem(cpu, cpu->PC + 1, &cpu->A, 1);
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x3f:
        // CCF
        cpu->F = CPU_F(CPU_FLAG_Z(cpu->F), 0, 0, CPU_FLAG_C(cpu->F) ? 0 : 1);
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x40: // LD B,B
      case 0x41: // LD B,C
      case 0x42: // LD B,D
      case 0x43: // LD B,E
      case 0x44: // LD B,H
      case 0x45: // LD B,L
      case 0x47: // LD B,A
      case 0x48: // LD C,B
      case 0x49: // LD C,C
      case 0x4a: // LD C,D
      case 0x4b: // LD C,E
      case 0x4c: // LD C,H
      case 0x4d: // LD C,L
      case 0x4f: // LD C,A
      case 0x50: // LD D,B
      case 0x51: // LD D,C
      case 0x52: // LD D,D
      case 0x53: // LD D,E
      case 0x54: // LD D,H
      case 0x55: // LD D,L
      case 0x57: // LD D,A
      case 0x58: // LD E,B
      case 0x59: // LD E,C
      case 0x5a: // LD E,D
      case 0x5b: // LD E,E
      case 0x5c: // LD E,H
      case 0x5d: // LD E,L
      case 0x5f: // LD E,A
      case 0x60: // LD H,B
      case 0x61: // LD H,C
      case 0x62: // LD H,D
      case 0x63: // LD H,E
      case 0x64: // LD H,H
      case 0x65: // LD H,L
      case 0x67: // LD H,A
      case 0x68: // LD L,B
      case 0x69: // LD L,C
      case 0x6a: // LD L,D
      case 0x6b: // LD L,E
      case 0x6c: // LD L,H
      case 0x6d: // LD L,L
      case 0x6f: // LD L,A
      case 0x78: // LD A,B
      case 0x79: // LD A,C
      case 0x7a: // LD A,D
      case 0x7b: // LD A,E
      case 0x7c: // LD A,H
      case 0x7d: // LD A,L
      case 0x7f: // LD A,A
        {
          unsigned char high_nibble = opcode >> 4;
          unsigned char low_nibble = opcode & 0xf;

          unsigned char* dest;
          switch (high_nibble) {
            case 4:
              if (low_nibble <= 7)
                dest = &cpu->B;
              else
                dest = &cpu->C;
              break;
            case 5:
              if (low_nibble <= 7)
                dest = &cpu->D;
              else
                dest = &cpu->E;
              break;
            case 6:
              if (low_nibble <= 7)
                dest = &cpu->H;
              else
                dest = &cpu->L;
              break;
            case 7:
              dest = &cpu->A;
              break;
            default:
              assert(false);
          }

          unsigned char* src;
          switch (low_nibble % 8) {
            case 0:
              dest = &cpu->B;
              break;
            case 1:
              dest = &cpu->C;
              break;
            case 2:
              dest = &cpu->D;
              break;
            case 3:
              dest = &cpu->E;
              break;
            case 4:
              dest = &cpu->H;
              break;
            case 5:
              dest = &cpu->L;
              break;
            case 7:
              dest = &cpu->A;
              break;
            default:
              assert(false);
          }

          *dest = *src;
        }
        break;

      case 0x46:
        // LD B,(HL)
        cpu_read_mem(cpu, CPU_HL(cpu), &cpu->B, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x4e:
        // LD C,(HL)
        cpu_read_mem(cpu, CPU_HL(cpu), &cpu->C, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x56:
        // LD D,(HL)
        cpu_read_mem(cpu, CPU_HL(cpu), &cpu->D, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x5e:
        // LD E,(HL)
        cpu_read_mem(cpu, CPU_HL(cpu), &cpu->E, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x66:
        // LD H,(HL)
        cpu_read_mem(cpu, CPU_HL(cpu), &cpu->H, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x6e:
        // LD L,(HL)
        cpu_read_mem(cpu, CPU_HL(cpu), &cpu->L, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x70:
        // LD (HL),B
        cpu_write_mem(cpu, CPU_HL(cpu), &cpu->B, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x71:
        // LD (HL),C
        cpu_write_mem(cpu, CPU_HL(cpu), &cpu->C, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x72:
        // LD (HL),D
        cpu_write_mem(cpu, CPU_HL(cpu), &cpu->D, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x73:
        // LD (HL),E
        cpu_write_mem(cpu, CPU_HL(cpu), &cpu->E, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x74:
        // LD (HL),H
        cpu_write_mem(cpu, CPU_HL(cpu), &cpu->H, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x75:
        // LD (HL),L
        cpu_write_mem(cpu, CPU_HL(cpu), &cpu->L, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x76:
        // HALT
        // TODO
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x77:
        // LD (HL),A
        cpu_write_mem(cpu, CPU_HL(cpu), &cpu->A, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x7e:
        // LD A,(HL)
        cpu_read_mem(cpu, CPU_HL(cpu), &cpu->A, 1);
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x80: // ADD A,B
      case 0x81: // ADD A,C
      case 0x82: // ADD A,D
      case 0x83: // ADD A,E
      case 0x84: // ADD A,H
      case 0x85: // ADD A,L
      case 0x87: // ADD A,A
      case 0x88: // ADC A,B
      case 0x89: // ADC A,C
      case 0x8a: // ADC A,D
      case 0x8b: // ADC A,E
      case 0x8c: // ADC A,H
      case 0x8d: // ADC A,L
      case 0x8f: // ADC A,A
      case 0x90: // SUB A,B
      case 0x91: // SUB A,C
      case 0x92: // SUB A,D
      case 0x93: // SUB A,E
      case 0x94: // SUB A,H
      case 0x95: // SUB A,L
      case 0x97: // SUB A,A
      case 0x98: // SBC A,B
      case 0x99: // SBC A,C
      case 0x9a: // SBC A,D
      case 0x9b: // SBC A,E
      case 0x9c: // SBC A,H
      case 0x9d: // SBC A,L
      case 0x9f: // SBC A,A
      case 0xa0: // AND B
      case 0xa1: // AND C
      case 0xa2: // AND D
      case 0xa3: // AND E
      case 0xa4: // AND H
      case 0xa5: // AND L
      case 0xa7: // AND A
      case 0xa8: // XOR B
      case 0xa9: // XOR C
      case 0xaa: // XOR D
      case 0xab: // XOR E
      case 0xac: // XOR H
      case 0xad: // XOR L
      case 0xaf: // XOR A
      case 0xb0: // OR B
      case 0xb1: // OR C
      case 0xb2: // OR D
      case 0xb3: // OR E
      case 0xb4: // OR H
      case 0xb5: // OR L
      case 0xb7: // OR A
      case 0xb8: // CP B
      case 0xb9: // CP C
      case 0xba: // CP D
      case 0xbb: // CP E
      case 0xbc: // CP H
      case 0xbd: // CP L
      case 0xbf: // CP A
        {
          unsigned char low_nibble = opcode & 0xf;

          unsigned char source;
          switch (low_nibble % 8) {
            case 0:
              source = cpu->B;
              break;
            case 1:
              source = cpu->C;
              break;
            case 2:
              source = cpu->D;
              break;
            case 3:
              source = cpu->E;
              break;
            case 4:
              source = cpu->H;
              break;
            case 5:
              source = cpu->L;
              break;
            case 7:
              source = cpu->A;
              break;
            default:
              assert(false);
          }

          unsigned char high_nibble = opcode >> 4;

          switch (high_nibble) {
            case 8:
              if (low_nibble < 8) {
                // ADD
                unsigned char carry;
                unsigned char half_carry;
                add_8_bit(cpu->A, source, &cpu->A, &carry, &half_carry);
                cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_carry, carry);
              } else {
                // ADC
                unsigned char carry_1;
                unsigned char half_carry_1;
                add_8_bit(cpu->A, source, &cpu->A, &carry_1, &half_carry_1);

                unsigned char carry_2;
                unsigned char half_carry_2;
                add_8_bit(cpu->A, CPU_FLAG_C(cpu->F), &cpu->A, &carry_2, &half_carry_2);

                cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_carry_1 || half_carry_2 ? 1 : 0, carry_1 || carry_2 ? 1 : 0);
              }
              break;

            case 9:
              if (low_nibble < 8) {
                // SUB
                unsigned char borrow;
                unsigned char half_borrow;
                sub_8_bit(cpu->A, source, &cpu->A, &borrow, &half_borrow);
                cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_borrow, borrow);
              } else {
                // SBC
                unsigned char borrow_1;
                unsigned char half_borrow_1;
                sub_8_bit(cpu->A, source, &cpu->A, &borrow_1, &half_borrow_1);

                unsigned char borrow_2;
                unsigned char half_borrow_2;
                sub_8_bit(cpu->A, CPU_FLAG_C(cpu->F), &cpu->A, &borrow_2, &half_borrow_2);

                cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_borrow_1 || half_borrow_2 ? 1 : 0, borrow_1 || borrow_2 ? 1 : 0);
              }
              break;

            case 10:
              if (low_nibble < 8) {
                // AND
                cpu->A = cpu->A & source;
                cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, 1, 0);
              } else {
                // XOR
                cpu->A = cpu->A ^ source;
                cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, 0, 0);
              }
              break;

            case 11:
              if (low_nibble < 8) {
                // OR
                cpu->A = cpu->A | source;
                cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, 1, 0);
              } else {
                // CP
                unsigned char trash;
                unsigned char borrow;
                unsigned char half_borrow;
                sub_8_bit(cpu->A, source, &trash, &borrow, &half_borrow);
                cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_borrow, borrow);
              }
              break;
            default:
              assert(false);
          }

          cpu->PC += 1;
          num_cycles = 4;
        }
        break;

      case 0x86:
        // ADD A,(HL)
        {
          unsigned char value;
          cpu_read_mem(cpu, CPU_HL(cpu), &value, 1);

          unsigned char carry;
          unsigned char half_carry;
          add_8_bit(cpu->A, value, &cpu->A, &carry, &half_carry);
          cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_carry, carry);
          cpu->PC += 1;
          num_cycles = 8;
        }
        break;

      case 0x8e:
        // ADC A,(HL)
        {
          unsigned char value;
          cpu_read_mem(cpu, CPU_HL(cpu), &value, 1);

          unsigned char carry_1;
          unsigned char half_carry_1;
          add_8_bit(cpu->A, value, &cpu->A, &carry_1, &half_carry_1);

          unsigned char carry_2;
          unsigned char half_carry_2;
          add_8_bit(cpu->A, CPU_FLAG_C(cpu->F), &cpu->A, &carry_2, &half_carry_2);

          cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_carry_1 || half_carry_2 ? 1 : 0, carry_1 || carry_2 ? 1 : 0);
          cpu->PC += 1;
          num_cycles = 8;
        }
        break;

      case 0x96:
        // SUB (HL)
        {
          unsigned char value;
          cpu_read_mem(cpu, CPU_HL(cpu), &value, 1);

          unsigned char carry;
          unsigned char half_carry;
          sub_8_bit(cpu->A, value, &cpu->A, &carry, &half_carry);
          cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_carry, carry);
          cpu->PC += 1;
          num_cycles = 8;
        }
        break;

      case 0x9e:
        // SBC A,(HL)
        {
          unsigned char value;
          cpu_read_mem(cpu, CPU_HL(cpu), &value, 1);

          unsigned char carry_1;
          unsigned char half_carry_1;
          sub_8_bit(cpu->A, value, &cpu->A, &carry_1, &half_carry_1);

          unsigned char carry_2;
          unsigned char half_carry_2;
          sub_8_bit(cpu->A, CPU_FLAG_C(cpu->F), &cpu->A, &carry_2, &half_carry_2);

          cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_carry_1 || half_carry_2 ? 1 : 0, carry_1 || carry_2 ? 1 : 0);
          cpu->PC += 1;
          num_cycles = 8;
        }
        break;

      case 0xa6:
        // AND (HL)
        {
          unsigned char value;
          cpu_read_mem(cpu, CPU_HL(cpu), &value, 1);

          cpu->A = cpu->A & value;
          cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, 1, 0);
          cpu->PC += 1;
          num_cycles = 8;
        }
        break;

      case 0xae:
        // XOR (HL)
        {
          unsigned char value;
          cpu_read_mem(cpu, CPU_HL(cpu), &value, 1);

          cpu->A = cpu->A ^ value;
          cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, 1, 0);
          cpu->PC += 1;
          num_cycles = 8;
        }
        break;

      case 0xb6:
        // OR (HL)
        {
          unsigned char value;
          cpu_read_mem(cpu, CPU_HL(cpu), &value, 1);

          cpu->A = cpu->A | value;
          cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, 1, 0);
          cpu->PC += 1;
          num_cycles = 8;
        }
        break;

      case 0xbe:
        // CP (HL)
        {
          unsigned char value;
          cpu_read_mem(cpu, CPU_HL(cpu), &value, 1);

          unsigned char trash;
          unsigned char borrow;
          unsigned char half_borrow;
          sub_8_bit(cpu->A, value, &trash, &borrow, &half_borrow);

          cpu->PC += 1;
          num_cycles = 8;
        }
        break;

      case 0xc0: // RET NZ
      case 0xc8: // RET Z
      case 0xd0: // RET NC
      case 0xd8: // RET C
        {
          bool ret;
          switch (opcode) {
            case 0xc0:
              ret = !CPU_FLAG_Z(cpu->F);
              break;
            case 0xc8:
              ret = CPU_FLAG_Z(cpu->F);
              break;
            case 0xd0:
              ret = !CPU_FLAG_C(cpu->F);
              break;
            case 0xd8:
              ret = CPU_FLAG_C(cpu->F);
              break;
            default:
              assert(false);
          }

          if (ret) {
            cpu_read_mem(cpu, cpu->SP, (unsigned char*)&cpu->PC, 2);
            cpu->SP += 2;
            num_cycles = 20;
          } else {
            num_cycles = 8;
          }
        }
        break;

      case 0xc9:
        // RET
        cpu_read_mem(cpu, cpu->SP, (unsigned char*)&cpu->PC, 2);
        cpu->SP += 2;
        num_cycles = 16;
        break;

      case 0xc1: // POP BC
      case 0xd1: // POP DE
      case 0xe1: // POP HL
      case 0xf1: // POP AF
        {
          unsigned short* dest;
          switch (opcode >> 4) {
            case 12:
              dest = CPU_BC_REF(cpu);
              break;
            case 13:
              dest = CPU_DE_REF(cpu);
              break;
            case 14:
              dest = CPU_HL_REF(cpu);
              break;
            case 15:
              dest = CPU_AF_REF(cpu);
              break;
            default:
              assert(false);
          }

          cpu_read_mem(cpu, cpu->SP, (unsigned char*)dest, 2);
          cpu->SP += 2;

          cpu->PC += 1;
          num_cycles = 12;
        }
        break;

      case 0xc2: // JP NZ,a16
      case 0xca: // JP Z,a16
      case 0xd2: // JP NC,a16
      case 0xda: // JP C,a16
        {
          bool jump;
          switch (opcode) {
            case 0xca:
              jump = !CPU_FLAG_Z(cpu->F);
              break;
            case 0xc2:
              jump = CPU_FLAG_Z(cpu->F);
              break;
            case 0xd2:
              jump = !CPU_FLAG_C(cpu->F);
              break;
            case 0xda:
              jump = CPU_FLAG_C(cpu->F);
              break;
            default:
              assert(false);
          }

          if (jump) {
            cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&cpu->PC, 2);
            num_cycles = 16;
          } else {
            num_cycles = 12;
          }
        }
        break;

      case 0xc3:
        // JP a16
        cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&cpu->PC, 2);
        num_cycles = 16;
        break;

      case 0xc4:
        // CALL NZ,a16
        cpu->PC += 3;
        num_cycles = 24/12;
        break;

      case 0xc5: // PUSH BC
      case 0xd5: // PUSH DE
      case 0xe5: // PUSH HL
      case 0xf5: // PUSH AF
        {
          unsigned short* src;
          switch (opcode >> 4) {
            case 12:
              src = CPU_BC_REF(cpu);
              break;
            case 13:
              src = CPU_DE_REF(cpu);
              break;
            case 14:
              src = CPU_HL_REF(cpu);
              break;
            case 15:
              src = CPU_AF_REF(cpu);
              break;
            default:
              assert(false);
          }

          cpu->SP -= 2;
          cpu_write_mem(cpu, cpu->SP, (unsigned char*)src, 2);

          cpu->PC += 1;
          num_cycles = 16;
        }
        break;

      case 0xc6:
        // ADD A,d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0xc7: // RST 00H
      case 0xcf: // RST 08H
      case 0xd7: // RST 10H
      case 0xdf: // RST 18H
      case 0xe7: // RST 20H
      case 0xef: // RST 28H
      case 0xf7: // RST 30H
      case 0xff: // RST 38H
        {
          unsigned short dest;
          switch (opcode) {
            case 0xc7:
              dest = 0x00;
            case 0xcf:
              dest = 0x08;
            case 0xd7:
              dest = 0x10;
            case 0xdf:
              dest = 0x18;
            case 0xe7:
              dest = 0x20;
            case 0xef:
              dest = 0x28;
            case 0xf7:
              dest = 0x30;
            case 0xff:
              dest = 0x38;
            default:
              assert(false);
          }

          cpu->SP -= 2;
          cpu_write_mem(cpu, cpu->SP, (unsigned char*)&cpu->PC, 2);

          cpu->PC = dest;
          num_cycles = 16;
        }
        break;

      case 0xcb:
        // PREFIX CB
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xcc:
        // CALL Z,a16
        cpu->PC += 3;
        num_cycles = 24/12;
        break;

      case 0xcd:
        // CALL a16
        cpu->PC += 3;
        num_cycles = 24;
        break;

      case 0xce:
        // ADC A,d8
        cpu->PC += 2;
        num_cycles = 8;
        break;


      case 0xd4:
        // CALL NC,a16
        cpu->PC += 3;
        num_cycles = 24/12;
        break;

      case 0xd6:
        // SUB d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0xd9:
        // RETI
        cpu->PC += 1;
        num_cycles = 16;
        break;

      case 0xdc:
        // CALL C,a16
        cpu->PC += 3;
        num_cycles = 24/12;
        break;

      case 0xde:
        // SBC A,d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0xe0:
        // LDH (a8),A
        cpu->PC += 2;
        num_cycles = 12;
        break;

      case 0xe2:
        // LD (C),A
        cpu_write_mem(cpu, cpu->C, &cpu->A, 1);
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0xe6:
        // AND d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0xe8:
        // ADD SP,r8
        cpu->PC += 2;
        num_cycles = 16;
        break;

      case 0xe9:
        // JP (HL)
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xea:
        // LD (a16),A
        cpu->PC += 3;
        num_cycles = 16;
        break;

      case 0xee:
        // XOR d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0xf0:
        // LDH A,(a8)
        cpu->PC += 2;
        num_cycles = 12;
        break;

      case 0xf2:
        // LD A,(C)
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0xf3:
        // DI
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xf6:
        // OR d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0xf8:
        // LD HL,SP+r8
        cpu->PC += 2;
        num_cycles = 12;
        break;

      case 0xf9:
        // LD SP,HL
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0xfa:
        // LD A,(a16)
        cpu->PC += 3;
        num_cycles = 16;
        break;

      case 0xfb:
        // EI
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xfe:
        // CP d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      default:
        assert(false);
    }
}

void test_math() {
  unsigned char carry;
  unsigned char half_carry;
  unsigned char borrow;
  unsigned char half_borrow;

  {
    unsigned char result;

    printf("8 bit add:\n");

    add_8_bit(1, 3, &result, &carry, &half_carry);
    printf("result: %d, carry: %d, half_carry: %d\n", result, carry, half_carry);

    add_8_bit(15, 15, &result, &carry, &half_carry);
    printf("result: %d, carry: %d, half_carry: %d\n", result, carry, half_carry);

    add_8_bit(255, 3, &result, &carry, &half_carry);
    printf("result: %d, carry: %d, half_carry: %d\n", result, carry, half_carry);

    add_8_bit(240, 17, &result, &carry, &half_carry);
    printf("result: %d, carry: %d, half_carry: %d\n", result, carry, half_carry);

    printf("8 bit sub:\n");

    sub_8_bit(3, 1, &result, &borrow, &half_borrow);
    printf("result: %d, borrow: %d, half_borrow: %d\n", result, borrow, half_borrow);

    sub_8_bit(16, 3, &result, &borrow, &half_borrow);
    printf("result: %d, borrow: %d, half_borrow: %d\n", result, borrow, half_borrow);

    sub_8_bit(1, 3, &result, &borrow, &half_borrow);
    printf("result: %d, borrow: %d, half_borrow: %d\n", result, borrow, half_borrow);

    sub_8_bit(18, 34, &result, &borrow, &half_borrow);
    printf("result: %d, borrow: %d, half_borrow: %d\n", result, borrow, half_borrow);
  }

  {
    unsigned short result;

    printf("16 bit add:\n");

    add_16_bit(240, 17, &result, &carry, &half_carry);
    printf("result: %d, carry: %d, half_carry: %d\n", result, carry, half_carry);

    add_16_bit(4095, 5, &result, &carry, &half_carry);
    printf("result: %d, carry: %d, half_carry: %d\n", result, carry, half_carry);

    add_16_bit(65000, 600, &result, &carry, &half_carry);
    printf("result: %d, carry: %d, half_carry: %d\n", result, carry, half_carry);

    add_16_bit(62000, 4096, &result, &carry, &half_carry);
    printf("result: %d, carry: %d, half_carry: %d\n", result, carry, half_carry);
  }
}

int main(int argc, char **argv) {
  Cpu cpu;
  cpu_initialize(&cpu);

  return 0;
}
