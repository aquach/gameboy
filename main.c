#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#define DEBUG 0

typedef struct {
  unsigned char memory[64 * 1024];

  // Pair orderings are important because you can use these in pairs: AF, BC, DE, HL.
  // This assumes little-endian architecture since A should be most significant when reading AF.
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

#define CPU_AF_REF(c) ((unsigned short*)&(c)->F)
#define CPU_BC_REF(c) ((unsigned short*)&(c)->C)
#define CPU_DE_REF(c) ((unsigned short*)&(c)->E)
#define CPU_HL_REF(c) ((unsigned short*)&(c)->L)

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
  if (b == 0) {
    *result = a;
    *borrow = false;
    *half_borrow = false;
  } else {
    add_8_bit(a, 256 - b, result, &carry, &half_carry);
    // 3 - 1 => 3 + 255 => carry flag set, which means borrow flag not set.
    // 3 - 4 => 3 + 252 => carry flag not set, which means borrow flag set.
    *borrow = !carry;
    *half_borrow = !half_carry;
  }
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
  if (b == 0) {
    *result = a;
    *borrow = false;
    *half_borrow = false;
  } else {
    add_16_bit(a, 65536 - b, result, &carry, &half_carry);
    *borrow = !carry;
    *half_borrow = !half_carry;
  }
}

void cpu_initialize(Cpu* cpu) {
  *CPU_AF_REF(cpu) = 0x01B0;
  *CPU_BC_REF(cpu) = 0x0013;
  *CPU_DE_REF(cpu) = 0x00D8;
  *CPU_HL_REF(cpu) = 0x014D;
  cpu->SP = 0xFFFE;
  cpu->PC = 0x0200;
  cpu->memory[0xFF05] = 0x00; // TIMA
  cpu->memory[0xFF06] = 0x00; // TMA
  cpu->memory[0xFF07] = 0x00; // TAC
  cpu->memory[0xFF10] = 0x80; // NR10
  cpu->memory[0xFF11] = 0xBF; // NR11
  cpu->memory[0xFF12] = 0xF3; // NR12
  cpu->memory[0xFF14] = 0xBF; // NR14
  cpu->memory[0xFF16] = 0x3F; // NR21
  cpu->memory[0xFF17] = 0x00; // NR22
  cpu->memory[0xFF19] = 0xBF; // NR24
  cpu->memory[0xFF1A] = 0x7F; // NR30
  cpu->memory[0xFF1B] = 0xFF; // NR31
  cpu->memory[0xFF1C] = 0x9F; // NR32
  cpu->memory[0xFF1E] = 0xBF; // NR33
  cpu->memory[0xFF20] = 0xFF; // NR41
  cpu->memory[0xFF21] = 0x00; // NR42
  cpu->memory[0xFF22] = 0x00; // NR43
  cpu->memory[0xFF23] = 0xBF; // NR30
  cpu->memory[0xFF24] = 0x77; // NR50
  cpu->memory[0xFF25] = 0xF3; // NR51
  cpu->memory[0xFF26] = 0x0F; // NR52
  cpu->memory[0xFF40] = 0x91; // LCDC
  cpu->memory[0xFF42] = 0x00; // SCY
  cpu->memory[0xFF43] = 0x00; // SCX
  cpu->memory[0xFF45] = 0x00; // LYC
  cpu->memory[0xFF47] = 0xFC; // BGP
  cpu->memory[0xFF48] = 0xFF; // OBP0
  cpu->memory[0xFF49] = 0xFF; // OBP1
  cpu->memory[0xFF4A] = 0x00; // WY
  cpu->memory[0xFF4B] = 0x00; // WX
  cpu->memory[0xFFFF] = 0x00; // IE
}

void cpu_read_mem(Cpu* cpu, unsigned short address, unsigned char* output, int len) {
  memcpy(output, cpu->memory + address, len);
}

void cpu_write_mem(Cpu* cpu, unsigned short address, unsigned char* data, int len) {
  if (address == 0xff02) { // SC
    printf("%d\n", cpu->memory[0xff01]); // SB
  } else {
    memcpy(cpu->memory + address, data, len);
  }
}

void cpu_print_instruction(Cpu* cpu) {
  unsigned char opcode;
  cpu_read_mem(cpu, cpu->PC, &opcode, 1);

  char* instructions[256];
  instructions[0x00] = "NOP";
  instructions[0x01] = "LD BC,d16";
  instructions[0x02] = "LD (BC),A";
  instructions[0x03] = "INC BC";
  instructions[0x04] = "INC B";
  instructions[0x05] = "DEC B";
  instructions[0x06] = "LD B,d8";
  instructions[0x07] = "RLCA";
  instructions[0x08] = "LD (a16),SP";
  instructions[0x09] = "ADD HL,BC";
  instructions[0x0a] = "LD A,(BC)";
  instructions[0x0b] = "DEC BC";
  instructions[0x0c] = "INC C";
  instructions[0x0d] = "DEC C";
  instructions[0x0e] = "LD C,d8";
  instructions[0x0f] = "RRCA";
  instructions[0x10] = "STOP 0";
  instructions[0x11] = "LD DE,d16";
  instructions[0x12] = "LD (DE),A";
  instructions[0x13] = "INC DE";
  instructions[0x14] = "INC D";
  instructions[0x15] = "DEC D";
  instructions[0x16] = "LD D,d8";
  instructions[0x17] = "RLA";
  instructions[0x18] = "JR r8";
  instructions[0x19] = "ADD HL,DE";
  instructions[0x1a] = "LD A,(DE)";
  instructions[0x1b] = "DEC DE";
  instructions[0x1c] = "INC E";
  instructions[0x1d] = "DEC E";
  instructions[0x1e] = "LD E,d8";
  instructions[0x1f] = "RRA";
  instructions[0x20] = "JR NZ,r8";
  instructions[0x21] = "LD HL,d16";
  instructions[0x22] = "LD (HL+),A";
  instructions[0x23] = "INC HL";
  instructions[0x24] = "INC H";
  instructions[0x25] = "DEC H";
  instructions[0x26] = "LD H,d8";
  instructions[0x27] = "DAA";
  instructions[0x28] = "JR Z,r8";
  instructions[0x29] = "ADD HL,HL";
  instructions[0x2a] = "LD A,(HL+)";
  instructions[0x2b] = "DEC HL";
  instructions[0x2c] = "INC L";
  instructions[0x2d] = "DEC L";
  instructions[0x2e] = "LD L,d8";
  instructions[0x2f] = "CPL";
  instructions[0x30] = "JR NC,r8";
  instructions[0x31] = "LD SP,d16";
  instructions[0x32] = "LD (HL-),A";
  instructions[0x33] = "INC SP";
  instructions[0x34] = "INC (HL)";
  instructions[0x35] = "DEC (HL)";
  instructions[0x36] = "LD (HL),d8";
  instructions[0x37] = "SCF";
  instructions[0x38] = "JR C,r8";
  instructions[0x39] = "ADD HL,SP";
  instructions[0x3a] = "LD A,(HL-)";
  instructions[0x3b] = "DEC SP";
  instructions[0x3c] = "INC A";
  instructions[0x3d] = "DEC A";
  instructions[0x3e] = "LD A,d8";
  instructions[0x3f] = "CCF";
  instructions[0x40] = "LD B,B";
  instructions[0x41] = "LD B,C";
  instructions[0x42] = "LD B,D";
  instructions[0x43] = "LD B,E";
  instructions[0x44] = "LD B,H";
  instructions[0x45] = "LD B,L";
  instructions[0x46] = "LD B,(HL)";
  instructions[0x47] = "LD B,A";
  instructions[0x48] = "LD C,B";
  instructions[0x49] = "LD C,C";
  instructions[0x4a] = "LD C,D";
  instructions[0x4b] = "LD C,E";
  instructions[0x4c] = "LD C,H";
  instructions[0x4d] = "LD C,L";
  instructions[0x4e] = "LD C,(HL)";
  instructions[0x4f] = "LD C,A";
  instructions[0x50] = "LD D,B";
  instructions[0x51] = "LD D,C";
  instructions[0x52] = "LD D,D";
  instructions[0x53] = "LD D,E";
  instructions[0x54] = "LD D,H";
  instructions[0x55] = "LD D,L";
  instructions[0x56] = "LD D,(HL)";
  instructions[0x57] = "LD D,A";
  instructions[0x58] = "LD E,B";
  instructions[0x59] = "LD E,C";
  instructions[0x5a] = "LD E,D";
  instructions[0x5b] = "LD E,E";
  instructions[0x5c] = "LD E,H";
  instructions[0x5d] = "LD E,L";
  instructions[0x5e] = "LD E,(HL)";
  instructions[0x5f] = "LD E,A";
  instructions[0x60] = "LD H,B";
  instructions[0x61] = "LD H,C";
  instructions[0x62] = "LD H,D";
  instructions[0x63] = "LD H,E";
  instructions[0x64] = "LD H,H";
  instructions[0x65] = "LD H,L";
  instructions[0x66] = "LD H,(HL)";
  instructions[0x67] = "LD H,A";
  instructions[0x68] = "LD L,B";
  instructions[0x69] = "LD L,C";
  instructions[0x6a] = "LD L,D";
  instructions[0x6b] = "LD L,E";
  instructions[0x6c] = "LD L,H";
  instructions[0x6d] = "LD L,L";
  instructions[0x6e] = "LD L,(HL)";
  instructions[0x6f] = "LD L,A";
  instructions[0x70] = "LD (HL),B";
  instructions[0x71] = "LD (HL),C";
  instructions[0x72] = "LD (HL),D";
  instructions[0x73] = "LD (HL),E";
  instructions[0x74] = "LD (HL),H";
  instructions[0x75] = "LD (HL),L";
  instructions[0x76] = "HALT";
  instructions[0x77] = "LD (HL),A";
  instructions[0x78] = "LD A,B";
  instructions[0x79] = "LD A,C";
  instructions[0x7a] = "LD A,D";
  instructions[0x7b] = "LD A,E";
  instructions[0x7c] = "LD A,H";
  instructions[0x7d] = "LD A,L";
  instructions[0x7e] = "LD A,(HL)";
  instructions[0x7f] = "LD A,A";
  instructions[0x80] = "ADD A,B";
  instructions[0x81] = "ADD A,C";
  instructions[0x82] = "ADD A,D";
  instructions[0x83] = "ADD A,E";
  instructions[0x84] = "ADD A,H";
  instructions[0x85] = "ADD A,L";
  instructions[0x86] = "ADD A,(HL)";
  instructions[0x87] = "ADD A,A";
  instructions[0x88] = "ADC A,B";
  instructions[0x89] = "ADC A,C";
  instructions[0x8a] = "ADC A,D";
  instructions[0x8b] = "ADC A,E";
  instructions[0x8c] = "ADC A,H";
  instructions[0x8d] = "ADC A,L";
  instructions[0x8e] = "ADC A,(HL)";
  instructions[0x8f] = "ADC A,A";
  instructions[0x90] = "SUB B";
  instructions[0x91] = "SUB C";
  instructions[0x92] = "SUB D";
  instructions[0x93] = "SUB E";
  instructions[0x94] = "SUB H";
  instructions[0x95] = "SUB L";
  instructions[0x96] = "SUB (HL)";
  instructions[0x97] = "SUB A";
  instructions[0x98] = "SBC A,B";
  instructions[0x99] = "SBC A,C";
  instructions[0x9a] = "SBC A,D";
  instructions[0x9b] = "SBC A,E";
  instructions[0x9c] = "SBC A,H";
  instructions[0x9d] = "SBC A,L";
  instructions[0x9e] = "SBC A,(HL)";
  instructions[0x9f] = "SBC A,A";
  instructions[0xa0] = "AND B";
  instructions[0xa1] = "AND C";
  instructions[0xa2] = "AND D";
  instructions[0xa3] = "AND E";
  instructions[0xa4] = "AND H";
  instructions[0xa5] = "AND L";
  instructions[0xa6] = "AND (HL)";
  instructions[0xa7] = "AND A";
  instructions[0xa8] = "XOR B";
  instructions[0xa9] = "XOR C";
  instructions[0xaa] = "XOR D";
  instructions[0xab] = "XOR E";
  instructions[0xac] = "XOR H";
  instructions[0xad] = "XOR L";
  instructions[0xae] = "XOR (HL)";
  instructions[0xaf] = "XOR A";
  instructions[0xb0] = "OR B";
  instructions[0xb1] = "OR C";
  instructions[0xb2] = "OR D";
  instructions[0xb3] = "OR E";
  instructions[0xb4] = "OR H";
  instructions[0xb5] = "OR L";
  instructions[0xb6] = "OR (HL)";
  instructions[0xb7] = "OR A";
  instructions[0xb8] = "CP B";
  instructions[0xb9] = "CP C";
  instructions[0xba] = "CP D";
  instructions[0xbb] = "CP E";
  instructions[0xbc] = "CP H";
  instructions[0xbd] = "CP L";
  instructions[0xbe] = "CP (HL)";
  instructions[0xbf] = "CP A";
  instructions[0xc0] = "RET NZ";
  instructions[0xc1] = "POP BC";
  instructions[0xc2] = "JP NZ,a16";
  instructions[0xc3] = "JP a16";
  instructions[0xc4] = "CALL NZ,a16";
  instructions[0xc5] = "PUSH BC";
  instructions[0xc6] = "ADD A,d8";
  instructions[0xc7] = "RST 00H";
  instructions[0xc8] = "RET Z";
  instructions[0xc9] = "RET";
  instructions[0xca] = "JP Z,a16";
  instructions[0xcb] = "PREFIX CB";
  instructions[0xcc] = "CALL Z,a16";
  instructions[0xcd] = "CALL a16";
  instructions[0xce] = "ADC A,d8";
  instructions[0xcf] = "RST 08H";
  instructions[0xd0] = "RET NC";
  instructions[0xd1] = "POP DE";
  instructions[0xd2] = "JP NC,a16";
  instructions[0xd4] = "CALL NC,a16";
  instructions[0xd5] = "PUSH DE";
  instructions[0xd6] = "SUB d8";
  instructions[0xd7] = "RST 10H";
  instructions[0xd8] = "RET C";
  instructions[0xd9] = "RETI";
  instructions[0xda] = "JP C,a16";
  instructions[0xdc] = "CALL C,a16";
  instructions[0xde] = "SBC A,d8";
  instructions[0xdf] = "RST 18H";
  instructions[0xe0] = "LDH (a8),A";
  instructions[0xe1] = "POP HL";
  instructions[0xe2] = "LD (C),A";
  instructions[0xe5] = "PUSH HL";
  instructions[0xe6] = "AND d8";
  instructions[0xe7] = "RST 20H";
  instructions[0xe8] = "ADD SP,r8";
  instructions[0xe9] = "JP (HL)";
  instructions[0xea] = "LD (a16),A";
  instructions[0xee] = "XOR d8";
  instructions[0xef] = "RST 28H";
  instructions[0xf0] = "LDH A,(a8)";
  instructions[0xf1] = "POP AF";
  instructions[0xf2] = "LD A,(C)";
  instructions[0xf3] = "DI";
  instructions[0xf5] = "PUSH AF";
  instructions[0xf6] = "OR d8";
  instructions[0xf7] = "RST 30H";
  instructions[0xf8] = "LD HL,SP+r8";
  instructions[0xf9] = "LD SP,HL";
  instructions[0xfa] = "LD A,(a16)";
  instructions[0xfb] = "EI";
  instructions[0xfe] = "CP d8";
  instructions[0xff] = "RST 38H";

  printf("%s (0x%02x) ", instructions[opcode], opcode);

  if (
      opcode == 0x06 ||
      opcode == 0x0e ||
      opcode == 0x16 ||
      opcode == 0x18 ||
      opcode == 0x1e ||
      opcode == 0x20 ||
      opcode == 0x26 ||
      opcode == 0x28 ||
      opcode == 0x2e ||
      opcode == 0x30 ||
      opcode == 0x36 ||
      opcode == 0x38 ||
      opcode == 0x3e ||
      opcode == 0xc6 ||
      opcode == 0xce ||
      opcode == 0xd6 ||
      opcode == 0xde ||
      opcode == 0xe0 ||
      opcode == 0xe2 ||
      opcode == 0xe6 ||
      opcode == 0xe8 ||
      opcode == 0xee ||
      opcode == 0xf0 ||
      opcode == 0xf2 ||
      opcode == 0xf6 ||
      opcode == 0xf8 ||
      opcode == 0xfe
  ) {
    unsigned char arg;
    cpu_read_mem(cpu, cpu->PC + 1, &arg, 1);
    printf("arg: %03d (0x%02x)\n", arg, arg);
  } else if (
    opcode == 0x01 ||
    opcode == 0x08 ||
    opcode == 0x11 ||
    opcode == 0x21 ||
    opcode == 0x31 ||
    opcode == 0xc2 ||
    opcode == 0xc3 ||
    opcode == 0xc4 ||
    opcode == 0xca ||
    opcode == 0xcc ||
    opcode == 0xcd ||
    opcode == 0xd2 ||
    opcode == 0xd4 ||
    opcode == 0xda ||
    opcode == 0xdc ||
    opcode == 0xea ||
    opcode == 0xfa
  ) {
    unsigned short arg;
    cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&arg, 2);
    printf("arg: %05d (0x%04x)\n", arg, arg);
  } else {
    printf("\n");
  }
}

void cpu_dump_registers(Cpu* cpu) {
  printf(
      "A: %03d (0x%02x) \
B: %03d (0x%02x) \
C: %03d (0x%02x) \
D: %03d (0x%02x) \
E: %03d (0x%02x) \
H: %03d (0x%02x) \
L: %03d (0x%02x)\n\
F: %03d (0x%02x) \
Z: %01d \
H: %01d \
N: %01d \
C: %01d\n\
AF: %05d (0x%04x) \
BC: %05d (0x%04x) \
DE: %05d (0x%04x) \
HL: %05d (0x%04x)\n\
SP: %05d (0x%04x) \
PC: %05d (0x%04x)\n",

      cpu->A, cpu->A,
      cpu->B, cpu->B,
      cpu->C, cpu->C,
      cpu->D, cpu->D,
      cpu->E, cpu->E,
      cpu->H, cpu->H,
      cpu->L, cpu->L,
      cpu->F, cpu->F,
      CPU_FLAG_Z(cpu->F),
      CPU_FLAG_H(cpu->F),
      CPU_FLAG_N(cpu->F),
      CPU_FLAG_C(cpu->F),
      CPU_AF(cpu), CPU_AF(cpu),
      CPU_BC(cpu), CPU_BC(cpu),
      CPU_DE(cpu), CPU_DE(cpu),
      CPU_HL(cpu), CPU_HL(cpu),
      cpu->SP, cpu->SP,
      cpu->PC, cpu->PC
  );
}

void cpu_dump_stack(Cpu* cpu) {
  printf("Stack:\n");
  for (unsigned short addr = 0xfffe; addr >= 0xfff0; addr--) {
    printf("0x%02x: %03d (0x%02d)\n", addr, cpu->memory[addr], cpu->memory[addr]);
  }
}

void cpu_step_clock(Cpu* cpu) {
  assert(cpu->PC <= 0x8000);

  if (DEBUG) {
    printf("Executing: ");
    cpu_print_instruction(cpu);
  }

  unsigned char opcode;
  cpu_read_mem(cpu, cpu->PC, &opcode, 1);
  cpu->PC++;

  assert(cpu->PC <= 0x8000);

  int num_cycles;
  switch (opcode) {
    case 0x00:
      // NOP
      num_cycles = 4;
      break;

    case 0x01:
      // LD BC,d16
      cpu_read_mem(cpu, cpu->PC, (unsigned char*)CPU_BC_REF(cpu), 2);
      cpu->PC += 2;
      num_cycles = 12;
      break;

    case 0x02:
      // LD (BC),A
      cpu_write_mem(cpu, CPU_BC(cpu), &cpu->A, 1);
      num_cycles = 8;
      break;

    case 0x03:
      // INC BC
      *CPU_BC_REF(cpu) = *CPU_BC_REF(cpu) + 1;
      num_cycles = 8;
      break;

    case 0x04:
      // INC B
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(cpu->B, 1, &cpu->B, &carry, &half_carry);
        cpu->F = CPU_F(cpu->B == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
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
        num_cycles = 4;
      }
      break;

    case 0x06:
      // LD B,d8
      cpu_read_mem(cpu, cpu->PC, (unsigned char*)&cpu->B, 1);
      cpu->PC++;
      num_cycles = 8;
      break;

    case 0x07:
      // RLCA
      {
        unsigned char bit = BIT(cpu->A, 7);
        cpu->A = (cpu->A << 1) | bit;
        cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, 0, bit);
        num_cycles = 4;
      }
      break;

    case 0x08:
      // LD (a16),SP
      {
        unsigned short address;
        cpu_read_mem(cpu, cpu->PC, (unsigned char*)&address, 2);
        cpu->PC += 2;
        cpu_write_mem(cpu, address, (unsigned char*)&cpu->SP, 2);
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
        num_cycles = 8;
      }
      break;

    case 0x0a:
      // LD A,(BC)
      cpu_read_mem(cpu, CPU_BC(cpu), &cpu->A, 1);
      num_cycles = 8;
      break;

    case 0x0b:
      // DEC BC
      *CPU_BC_REF(cpu) = *CPU_BC_REF(cpu) - 1;
      num_cycles = 8;
      break;

    case 0x0c:
      // INC C
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(cpu->C, 1, &cpu->C, &carry, &half_carry);
        cpu->F = CPU_F(cpu->C == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
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
        num_cycles = 4;
      }
      break;

    case 0x0e:
      // LD C,d8
      cpu_read_mem(cpu, cpu->PC, &cpu->C, 1);
      cpu->PC++;
      num_cycles = 8;
      break;

    case 0x0f:
      // RRCA
      {
        unsigned char bit = BIT(cpu->A, 0);
        cpu->A = (cpu->A >> 1) | (bit << 7);
        cpu->F = CPU_F(0, 0, 0, bit); // TODO: Z flag is a matter of debate. Could be 0, 1, or Z.
        num_cycles = 4;
      }
      break;

    case 0x10:
      // STOP 0
      assert(false);
      cpu->PC++;
      num_cycles = 4;
      break;

    case 0x11:
      // LD DE,d16
      cpu_read_mem(cpu, cpu->PC, (unsigned char*)CPU_DE_REF(cpu), 2);
      cpu->PC += 2;
      num_cycles = 12;
      break;

    case 0x12:
      // LD (DE),A
      cpu_write_mem(cpu, CPU_DE(cpu), &cpu->A, 1);
      num_cycles = 8;
      break;

    case 0x13:
      // INC DE
      *CPU_DE_REF(cpu) = *CPU_DE_REF(cpu) + 1;
      num_cycles = 8;
      break;

    case 0x14:
      // INC D
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(cpu->D, 1, &cpu->D, &carry, &half_carry);
        cpu->F = CPU_F(cpu->D == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
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
        num_cycles = 4;
      }
      break;

    case 0x16:
      // LD D,d8
      cpu_read_mem(cpu, cpu->PC, (unsigned char*)&cpu->D, 1);
      cpu->PC++;
      num_cycles = 8;
      break;

    case 0x17:
      // RLA
      {
        unsigned char bit = BIT(cpu->A, 7);
        cpu->A = (cpu->A << 1) | CPU_FLAG_C(cpu->F);
        cpu->F = CPU_F(0, 0, 0, bit); // TODO: Z is up for debate.
        num_cycles = 4;
      }
      break;

    case 0x18:
      // JR r8
      {
        char offset;
        cpu_read_mem(cpu, cpu->PC, (unsigned char*)&offset, 1);
        cpu->PC++;
        cpu->PC += offset;
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
        num_cycles = 8;
      }
      break;

    case 0x1a:
      // LD A,(DE)
      cpu_read_mem(cpu, CPU_DE(cpu), &cpu->A, 1);
      num_cycles = 8;
      break;

    case 0x1b:
      // DEC DE
      *CPU_DE_REF(cpu) = *CPU_DE_REF(cpu) - 1;
      num_cycles = 8;
      break;

    case 0x1c:
      // INC E
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(cpu->E, 1, &cpu->E, &carry, &half_carry);
        cpu->F = CPU_F(cpu->E == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
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
        num_cycles = 4;
      }
      break;

    case 0x1e:
      // LD E,d8
      cpu_read_mem(cpu, cpu->PC, &cpu->E, 1);
      cpu->PC++;
      num_cycles = 8;
      break;

    case 0x1f:
      // RRA
      {
        unsigned char bit = BIT(cpu->A, 0);
        cpu->A = (cpu->A >> 1) | (CPU_FLAG_C(cpu->F) << 7);
        cpu->F = CPU_F(0, 0, 0, bit); // TODO: Z flag is up for debate.
        num_cycles = 4;
      }
      break;

    case 0x20: // JR NZ,r8
    case 0x28: // JR Z,r8
    case 0x30: // JR NC,r8
    case 0x38: // JR C,r8
      {
        char offset;
        cpu_read_mem(cpu, cpu->PC, (unsigned char*)&offset, 1);
        cpu->PC++;

        bool jump;
        switch (opcode) {
          case 0x20:
            jump = !CPU_FLAG_Z(cpu->F);
            break;
          case 0x28:
            jump = CPU_FLAG_Z(cpu->F);
            break;
          case 0x30:
            jump = !CPU_FLAG_C(cpu->F);
            break;
          case 0x38:
            jump = CPU_FLAG_C(cpu->F);
            break;
          default:
            assert(false);
        }

        if (jump) {
          cpu->PC += offset;
          num_cycles = 12;
        } else {
          num_cycles = 8;
        }
      }
      break;

    case 0x21:
      // LD HL,d16
      cpu_read_mem(cpu, cpu->PC, (unsigned char*)CPU_HL_REF(cpu), 2);
      cpu->PC += 2;
      num_cycles = 12;
      break;

    case 0x22:
      // LD (HL+),A
      cpu_write_mem(cpu, CPU_HL(cpu), &cpu->A, 1);
      *CPU_HL_REF(cpu) = CPU_HL(cpu) + 1;
      num_cycles = 8;
      break;

    case 0x23:
      // INC HL
      *CPU_HL_REF(cpu) = CPU_HL(cpu) + 1;
      num_cycles = 8;
      break;

    case 0x24:
      // INC H
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(cpu->H, 1, &cpu->H, &carry, &half_carry);
        cpu->F = CPU_F(cpu->H == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
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
        num_cycles = 4;
      }
      break;

    case 0x26:
      // LD H,d8
      cpu_read_mem(cpu, cpu->PC, (unsigned char*)&cpu->H, 1);
      cpu->PC++;
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
        num_cycles = 4;
      }
      break;

    case 0x29:
      // ADD HL,HL
      {
        unsigned char carry;
        unsigned char half_carry;
        add_16_bit(CPU_HL(cpu), CPU_HL(cpu), CPU_HL_REF(cpu), &carry, &half_carry);
        cpu->F = CPU_F(CPU_FLAG_Z(cpu->F), 0, half_carry, carry);
        num_cycles = 8;
      }
      break;

    case 0x2a:
      // LD A,(HL+)
      cpu_read_mem(cpu, CPU_HL(cpu), &cpu->A, 1);
      *CPU_HL_REF(cpu) = CPU_HL(cpu) + 1;
      num_cycles = 8;
      break;

    case 0x2b:
      // DEC HL
      *CPU_HL_REF(cpu) = CPU_HL(cpu) - 1;
      num_cycles = 8;
      break;

    case 0x2c:
      // INC L
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(cpu->L, 1, &cpu->L, &carry, &half_carry);
        cpu->F = CPU_F(cpu->L == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
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
        num_cycles = 4;
      }
      break;

    case 0x2e:
      // LD L,d8
      cpu_read_mem(cpu, cpu->PC, &cpu->L, 1);
      cpu->PC++;
      num_cycles = 8;
      break;

    case 0x2f:
      // CPL
      cpu->A = ~cpu->A;
      num_cycles = 4;
      break;

    case 0x31:
      // LD SP,d16
      cpu_read_mem(cpu, cpu->PC, (unsigned char*)&cpu->SP, 2);
      cpu->PC += 2;
      num_cycles = 12;
      break;

    case 0x32:
      // LD (HL-),A
      cpu_write_mem(cpu, CPU_HL(cpu), &cpu->A, 1);
      *CPU_HL_REF(cpu) = CPU_HL(cpu) - 1;
      num_cycles = 8;
      break;

    case 0x33:
      // INC SP
      cpu->SP++;
      num_cycles = 8;
      break;

    case 0x34:
      // INC (HL)
      {
        unsigned char value;
        cpu_read_mem(cpu, CPU_HL(cpu), &value, 1);
        cpu->PC++;

        unsigned char carry;
        unsigned char half_carry;
        add_16_bit(value, 1, CPU_HL_REF(cpu), &carry, &half_carry);
        cpu->F = CPU_F(CPU_HL(cpu) == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
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
        num_cycles = 12;
      }
      break;

    case 0x36:
      // LD (HL),d8
      {
        unsigned char value;
        cpu_read_mem(cpu, cpu->PC, &value, 1);
        cpu->PC++;

        cpu_write_mem(cpu, CPU_HL(cpu), &value, 1);
        num_cycles = 12;
      }
      break;

    case 0x37:
      // SCF
      cpu->F = CPU_F(CPU_FLAG_Z(cpu->F), 0, 0, 1);
      num_cycles = 4;
      break;

    case 0x39:
      // ADD HL,SP
      {
        unsigned char carry;
        unsigned char half_carry;
        add_16_bit(CPU_HL(cpu), cpu->SP, CPU_HL_REF(cpu), &carry, &half_carry);
        cpu->F = CPU_F(CPU_FLAG_Z(cpu->F), 0, half_carry, carry);
        num_cycles = 8;
      }
      break;

    case 0x3a:
      // LD A,(HL-)
      cpu_read_mem(cpu, CPU_HL(cpu), &cpu->A, 1);
      *CPU_HL_REF(cpu) = CPU_HL(cpu) - 1;
      num_cycles = 8;
      break;

    case 0x3b:
      // DEC SP
      cpu->SP--;
      num_cycles = 8;
      break;

    case 0x3c:
      // INC A
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(cpu->A, 1, &cpu->A, &carry, &half_carry);
        cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(cpu->F));
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
        num_cycles = 4;
      }
      break;

    case 0x3e:
      // LD A,d8
      cpu_read_mem(cpu, cpu->PC, &cpu->A, 1);
      cpu->PC++;
      num_cycles = 8;
      break;

    case 0x3f:
      // CCF
      cpu->F = CPU_F(CPU_FLAG_Z(cpu->F), 0, 0, CPU_FLAG_C(cpu->F) ? 0 : 1);
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

        unsigned char* src = NULL;
        switch (low_nibble % 8) {
          case 0:
            src = &cpu->B;
            break;
          case 1:
            src = &cpu->C;
            break;
          case 2:
            src = &cpu->D;
            break;
          case 3:
            src = &cpu->E;
            break;
          case 4:
            src = &cpu->H;
            break;
          case 5:
            src = &cpu->L;
            break;
          case 7:
            src = &cpu->A;
            break;
          default:
            assert(false);
        }

        *dest = *src;

        num_cycles = 4;
      }

      break;

    case 0x46:
      // LD B,(HL)
      cpu_read_mem(cpu, CPU_HL(cpu), &cpu->B, 1);
      num_cycles = 8;
      break;

    case 0x4e:
      // LD C,(HL)
      cpu_read_mem(cpu, CPU_HL(cpu), &cpu->C, 1);
      num_cycles = 8;
      break;

    case 0x56:
      // LD D,(HL)
      cpu_read_mem(cpu, CPU_HL(cpu), &cpu->D, 1);
      num_cycles = 8;
      break;

    case 0x5e:
      // LD E,(HL)
      cpu_read_mem(cpu, CPU_HL(cpu), &cpu->E, 1);
      num_cycles = 8;
      break;

    case 0x66:
      // LD H,(HL)
      cpu_read_mem(cpu, CPU_HL(cpu), &cpu->H, 1);
      num_cycles = 8;
      break;

    case 0x6e:
      // LD L,(HL)
      cpu_read_mem(cpu, CPU_HL(cpu), &cpu->L, 1);
      num_cycles = 8;
      break;

    case 0x70:
      // LD (HL),B
      cpu_write_mem(cpu, CPU_HL(cpu), &cpu->B, 1);
      num_cycles = 8;
      break;

    case 0x71:
      // LD (HL),C
      cpu_write_mem(cpu, CPU_HL(cpu), &cpu->C, 1);
      num_cycles = 8;
      break;

    case 0x72:
      // LD (HL),D
      cpu_write_mem(cpu, CPU_HL(cpu), &cpu->D, 1);
      num_cycles = 8;
      break;

    case 0x73:
      // LD (HL),E
      cpu_write_mem(cpu, CPU_HL(cpu), &cpu->E, 1);
      num_cycles = 8;
      break;

    case 0x74:
      // LD (HL),H
      cpu_write_mem(cpu, CPU_HL(cpu), &cpu->H, 1);
      num_cycles = 8;
      break;

    case 0x75:
      // LD (HL),L
      cpu_write_mem(cpu, CPU_HL(cpu), &cpu->L, 1);
      num_cycles = 8;
      break;

    case 0x76:
      // HALT
      assert(false);
      num_cycles = 4;
      break;

    case 0x77:
      // LD (HL),A
      cpu_write_mem(cpu, CPU_HL(cpu), &cpu->A, 1);
      num_cycles = 8;
      break;

    case 0x7e:
      // LD A,(HL)
      cpu_read_mem(cpu, CPU_HL(cpu), &cpu->A, 1);
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

        if (opcode == 0xf1) {
          // Discard lower nibble.
          cpu->F = (cpu->F >> 4) << 4;
        }

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

        unsigned short dest;
        cpu_read_mem(cpu, cpu->PC, (unsigned char*)&dest, 2);
        cpu->PC += 2;

        if (jump) {
          cpu->PC = dest;
          num_cycles = 16;
        } else {
          num_cycles = 12;
        }
      }
      break;

    case 0xc3:
      // JP a16
      cpu_read_mem(cpu, cpu->PC, (unsigned char*)&cpu->PC, 2);
      num_cycles = 16;
      break;

    case 0xc4: // CALL NZ,a16
    case 0xcc: // CALL Z,a16
    case 0xd4: // CALL NC,a16
    case 0xdc: // CALL C,a16
      {
        bool call;
        switch (opcode) {
          case 0xc4:
            call = !CPU_FLAG_Z(cpu->F);
            break;
          case 0xcc:
            call = CPU_FLAG_Z(cpu->F);
            break;
          case 0xd4:
            call = !CPU_FLAG_C(cpu->F);
            break;
          case 0xdc:
            call = CPU_FLAG_C(cpu->F);
            break;
          default:
            assert(false);
        }

        unsigned short dest;
        cpu_read_mem(cpu, cpu->PC, (unsigned char*)&dest, 2);
        cpu->PC += 2;

        if (call) {
          cpu->SP -= 2;
          cpu_write_mem(cpu, cpu->SP, (unsigned char*)&cpu->PC, 2);
          cpu->PC = dest;
          num_cycles = 24;
        } else {
          num_cycles = 12;
        }
      }
      break;

    case 0xcd: // CALL a16
      {
        unsigned short dest;
        cpu_read_mem(cpu, cpu->PC, (unsigned char*)&dest, 2);
        cpu->PC += 2;

        cpu->SP -= 2;
        cpu_write_mem(cpu, cpu->SP, (unsigned char*)&cpu->PC, 2);
        cpu->PC = dest;
        num_cycles = 24;
      }
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

        num_cycles = 16;
      }
      break;

    case 0xc6:
      // ADD A,d8
      {
        unsigned char source;
        cpu_read_mem(cpu, cpu->PC, &source, 1);
        cpu->PC++;

        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(cpu->A, source, &cpu->A, &carry, &half_carry);
        cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_carry, carry);
        num_cycles = 8;
      }
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
            break;
          case 0xcf:
            dest = 0x08;
            break;
          case 0xd7:
            dest = 0x10;
            break;
          case 0xdf:
            dest = 0x18;
            break;
          case 0xe7:
            dest = 0x20;
            break;
          case 0xef:
            dest = 0x28;
            break;
          case 0xf7:
            dest = 0x30;
            break;
          case 0xff:
            dest = 0x38;
            break;
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
      assert(false);
      num_cycles = 4;
      break;

    case 0xce:
      // ADC A,d8
      {
        unsigned char value;
        cpu_read_mem(cpu, cpu->PC, &value, 1);
        cpu->PC++;

        unsigned char carry_1;
        unsigned char half_carry_1;
        add_8_bit(cpu->A, value, &cpu->A, &carry_1, &half_carry_1);

        unsigned char carry_2;
        unsigned char half_carry_2;
        add_8_bit(cpu->A, CPU_FLAG_C(cpu->F), &cpu->A, &carry_2, &half_carry_2);

        cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_carry_1 || half_carry_2 ? 1 : 0, carry_1 || carry_2 ? 1 : 0);
        num_cycles = 8;
      }
      break;

    case 0xd6:
      // SUB d8
      {
        unsigned char source;
        cpu_read_mem(cpu, cpu->PC, &source, 1);
        cpu->PC++;

        unsigned char carry;
        unsigned char half_carry;
        sub_8_bit(cpu->A, source, &cpu->A, &carry, &half_carry);
        cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_carry, carry);
        num_cycles = 8;
      }
      break;

    case 0xd9:
      // RETI
      assert(false);
      num_cycles = 16;
      break;

    case 0xde:
      // SBC A,d8
      {
        unsigned char value;
        cpu_read_mem(cpu, cpu->PC, &value, 1);
        cpu->PC++;

        unsigned char carry_1;
        unsigned char half_carry_1;
        sub_8_bit(cpu->A, value, &cpu->A, &carry_1, &half_carry_1);

        unsigned char carry_2;
        unsigned char half_carry_2;
        sub_8_bit(cpu->A, CPU_FLAG_C(cpu->F), &cpu->A, &carry_2, &half_carry_2);

        cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_carry_1 || half_carry_2 ? 1 : 0, carry_1 || carry_2 ? 1 : 0);
        num_cycles = 8;
      }
      break;

    case 0xe0:
      // LDH (a8),A
      {
        unsigned char addr;
        cpu_read_mem(cpu, cpu->PC, &addr, 1);
        cpu->PC++;
        unsigned short offset_addr = (unsigned short)addr + 0xff00;
        cpu_write_mem(cpu, offset_addr, &cpu->A, 1);
        num_cycles = 12;
      }
      break;

    case 0xe2:
      // LD (C),A
      cpu->PC++;
      cpu_write_mem(cpu, 0xff00 + cpu->C, &cpu->A, 1);
      num_cycles = 8;
      break;

    case 0xe6:
      // AND d8
      {
        unsigned char value;
        cpu_read_mem(cpu, cpu->PC, &value, 1);
        cpu->PC++;

        cpu->A = cpu->A & value;
        cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, 1, 0);
        num_cycles = 8;
      }
      break;

    case 0xe8:
      // ADD SP,r8
      {
        char offset;
        cpu_read_mem(cpu, cpu->PC, (unsigned char*)&offset, 1);
        cpu->PC++;

        if (offset >= 0) {
          unsigned char carry;
          unsigned char half_carry;
          add_16_bit(cpu->SP, (unsigned char)offset, &cpu->SP, &carry, &half_carry);
          cpu->F = CPU_F(0, 0, half_carry, carry);
        } else {
          unsigned char borrow;
          unsigned char half_borrow;
          sub_16_bit(cpu->SP, (unsigned char)-offset, &cpu->SP, &borrow, &half_borrow);
          cpu->F = CPU_F(0, 0, half_borrow, borrow);
        }

        num_cycles = 16;
      }
      break;

    case 0xe9:
      // JP (HL)
      cpu_read_mem(cpu, cpu->PC, (unsigned char*)CPU_HL_REF(cpu), 2);
      num_cycles = 4;
      break;

    case 0xea:
      // LD (a16),A
      {
        unsigned short address;
        cpu_read_mem(cpu, cpu->PC, (unsigned char*)&address, 2);
        cpu->PC += 2;
        cpu_write_mem(cpu, address, &cpu->A, 1);
        num_cycles = 16;
      }
      break;

    case 0xee:
      // XOR d8
      {
        unsigned char value;
        cpu_read_mem(cpu, cpu->PC, &value, 1);
        cpu->PC++;

        cpu->A = cpu->A ^ value;
        cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, 1, 0);
        num_cycles = 8;
      }
      break;

    case 0xf0:
      // LDH A,(a8)
      {
        unsigned char value;
        cpu_read_mem(cpu, cpu->PC, &value, 1);
        cpu->PC++;
        cpu_read_mem(cpu, 0xff00 + value, &cpu->A, 1);
        num_cycles = 12;
      }
      break;

    case 0xf2:
      // LD A,(C)
      cpu->PC++;
      cpu_read_mem(cpu, 0xff00 + cpu->C, &cpu->A, 1);
      num_cycles = 8;
      break;

    case 0xf3:
      // DI
      assert(false);
      num_cycles = 4;
      break;

    case 0xf6:
      // OR d8
      {
        unsigned char value;
        cpu_read_mem(cpu, cpu->PC, &value, 1);
        cpu->PC++;

        cpu->A = cpu->A | value;
        cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, 1, 0);
        num_cycles = 8;
      }
      break;

    case 0xf8:
      // LD HL,SP+r8
      {
        char offset;
        cpu_read_mem(cpu, cpu->PC, (unsigned char*)&offset, 1);
        cpu->PC++;
        *CPU_HL_REF(cpu) = cpu->SP + offset;
        num_cycles = 12;
      }
      break;

    case 0xf9:
      // LD SP,HL
      cpu->SP = CPU_HL(cpu);
      num_cycles = 8;
      break;

    case 0xfa:
      // LD A,(a16)
      {
        unsigned short address;
        cpu_read_mem(cpu, cpu->PC, (unsigned char*)&address, 2);
        cpu->PC += 2;
        cpu_read_mem(cpu, address, (unsigned char*)&cpu->A, 1);
        num_cycles = 16;
      }
      break;

    case 0xfb:
      // EI
      assert(false);
      num_cycles = 4;
      break;

    case 0xfe:
      // CP d8
      {
        unsigned char value;
        cpu_read_mem(cpu, cpu->PC, &value, 1);
        cpu->PC++;

        unsigned char trash;
        unsigned char borrow;
        unsigned char half_borrow;
        sub_8_bit(cpu->A, value, &trash, &borrow, &half_borrow);
        cpu->F = CPU_F(cpu->A == 0 ? 1 : 0, 0, half_borrow, borrow);

        num_cycles = 8;
      }
      break;

    default:
      assert(false);
  }

  if (DEBUG) {
    cpu_dump_registers(cpu);
    cpu_dump_stack(cpu);
    printf("\n");
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

void cpu_load_rom_from_file(Cpu* cpu, const char* rom_path) {
  FILE* fp = fopen(rom_path, "r");
  fread(cpu->memory, 0x8000, 1, fp);
}

int main(int argc, char **argv) {
  if (argc != 2) {
    printf("Usage: ./gameboy rom.gb\n");
    return 1;
  }

  Cpu cpu;
  cpu_initialize(&cpu);

  const char* rom_path = argv[1];
  cpu_load_rom_from_file(&cpu, rom_path);

  while (1)
    cpu_step_clock(&cpu);

  return 0;
}
