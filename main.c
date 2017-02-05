#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

char DEBUG = 0;
unsigned long long COUNTDOWN = 200000000;

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

  int set_ime_after_n_instructions;
  int unset_ime_after_n_instructions;
  bool IME;

  unsigned long long global_simulated_ticks; // In CPU clock ticks.
  int ticks_to_next_instruction;
} Gameboy;

#define REG_SB (0xFF01)
#define REG_SC (0xFF02)
#define REG_DIV (0xFF04)
#define REG_TIMA (0xFF05)
#define REG_TMA (0xFF06)
#define REG_TAC (0xFF07)
#define REG_IF (0xFF0F)
#define REG_NR10 (0xFF10)
#define REG_NR11 (0xFF11)
#define REG_NR12 (0xFF12)
#define REG_NR14 (0xFF14)
#define REG_NR21 (0xFF16)
#define REG_NR22 (0xFF17)
#define REG_NR24 (0xFF19)
#define REG_NR30 (0xFF1A)
#define REG_NR31 (0xFF1B)
#define REG_NR32 (0xFF1C)
#define REG_NR33 (0xFF1E)
#define REG_NR41 (0xFF20)
#define REG_NR42 (0xFF21)
#define REG_NR43 (0xFF22)
#define REG_NR44 (0xFF23)
#define REG_NR50 (0xFF24)
#define REG_NR51 (0xFF25)
#define REG_NR52 (0xFF26)
#define REG_LCDC (0xFF40)
#define REG_SCY (0xFF42)
#define REG_SCX (0xFF43)
#define REG_LYC (0xFF45)
#define REG_BGP (0xFF47)
#define REG_OBP0 (0xFF48)
#define REG_OBP1 (0xFF49)
#define REG_WY (0xFF4A)
#define REG_WX (0xFF4B)
#define REG_IE (0xFFFF)

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

#define SET_BIT(v, index) ((v) | (1 << (index)))
#define UNSET_BIT(v, index) ((v) & ~(1 << (index)))

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
  unsigned short full_sub = (unsigned short)a - (unsigned short)b;
  *result = full_sub & 0xff;
  *borrow = a < b;
  *half_borrow = (((unsigned short)a & 0xf) < ((unsigned short)b & 0xf));
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

void sub_16_bit(
  unsigned short a,
  unsigned short b,
  unsigned short* result,
  unsigned char* borrow,
  unsigned char* half_borrow
) {
  unsigned short full_sub = (unsigned int)a - (unsigned int)b;
  *result = full_sub & 0xffff;
  *borrow = a < b;
  *half_borrow = (((unsigned int)a >> 8) & 0xf) < (((unsigned int)b >> 8) & 0xf);
}

unsigned char rlc(unsigned char value, unsigned char* output_carry) {
  // Rotate left, storing 7 in C.
  *output_carry = BIT(value, 7);
  return (value << 1) | *output_carry;
}

unsigned char rl(unsigned char value, unsigned char input_carry, unsigned char* output_carry) {
  // Rotate left as if C were bit 8.
  *output_carry = BIT(value, 7);
  return (value << 1) | input_carry;
}

unsigned char rrc(unsigned char value, unsigned char* output_carry) {
  // Rotate right, storing value of old bit 0 in C.
  *output_carry = BIT(value, 0);
  return (value >> 1) | (*output_carry << 7);
}

unsigned char rr(unsigned char value, unsigned char input_carry, unsigned char* output_carry) {
  // Rotate right as if C were bit -1.
  *output_carry = BIT(value, 0);
  return (value >> 1) | (input_carry << 7);
}

unsigned char sla(unsigned char value, unsigned char* output_carry) {
  // Shift left, storing 0 in bit 0 and bit 7 in C.
  *output_carry = BIT(value, 7);
  return value << 1;
}

unsigned char sra(unsigned char value, unsigned char* output_carry) {
  // Shift right, storing value of bit 7 into new bit 7 and bit 0 in C.
  *output_carry = BIT(value, 0);
  return (value >> 1) | (BIT(value, 7) << 7);
}

unsigned char srl(unsigned char value, unsigned char* output_carry) {
  // Shift right, storing 0 in bit 7 and bit 0 in C.
  *output_carry = BIT(value, 0);
  return value >> 1;
}

unsigned char swap(unsigned char value) {
  // Swaps upper and lower nibble.
  unsigned char upper = value >> 4;
  unsigned char lower = value & 0xf;
  return (lower << 4) | upper;
}

void gameboy_initialize(Gameboy* gb) {
  memset(gb->memory, 0, 65536);
  *CPU_AF_REF(gb) = 0x01B0;
  *CPU_BC_REF(gb) = 0x0013;
  *CPU_DE_REF(gb) = 0x00D8;
  *CPU_HL_REF(gb) = 0x014D;
  gb->SP = 0xFFFE;
  gb->PC = 0x0100;
  gb->memory[REG_TIMA] = 0x00;
  gb->memory[REG_TMA] = 0x00;
  gb->memory[REG_TAC] = 0x00;
  gb->memory[REG_NR10] = 0x80;
  gb->memory[REG_NR11] = 0xBF;
  gb->memory[REG_NR12] = 0xF3;
  gb->memory[REG_NR14] = 0xBF;
  gb->memory[REG_NR21] = 0x3F;
  gb->memory[REG_NR22] = 0x00;
  gb->memory[REG_NR24] = 0xBF;
  gb->memory[REG_NR30] = 0x7F;
  gb->memory[REG_NR31] = 0xFF;
  gb->memory[REG_NR32] = 0x9F;
  gb->memory[REG_NR33] = 0xBF;
  gb->memory[REG_NR41] = 0xFF;
  gb->memory[REG_NR42] = 0x00;
  gb->memory[REG_NR43] = 0x00;
  gb->memory[REG_NR30] = 0xBF;
  gb->memory[REG_NR50] = 0x77;
  gb->memory[REG_NR51] = 0xF3;
  gb->memory[REG_NR52] = 0x0F;
  gb->memory[REG_LCDC] = 0x91;
  gb->memory[REG_SCY] = 0x00;
  gb->memory[REG_SCX] = 0x00;
  gb->memory[REG_LYC] = 0x00;
  gb->memory[REG_BGP] = 0xFC;
  gb->memory[REG_OBP0] = 0xFF;
  gb->memory[REG_OBP1] = 0xFF;
  gb->memory[REG_WY] = 0x00;
  gb->memory[REG_WX] = 0x00;
  gb->memory[REG_IE] = 0x00;

  gb->IME = 0;
  gb->set_ime_after_n_instructions = -1;
  gb->unset_ime_after_n_instructions = -1;

  gb->global_simulated_ticks = 0;
  gb->ticks_to_next_instruction = 0;
}

void gameboy_read_mem(Gameboy* gb, unsigned short address, unsigned char* output, int len) {
  memcpy(output, gb->memory + address, len);
}

void gameboy_write_mem(Gameboy* gb, unsigned short address, unsigned char* data, int len) {
  if (address == REG_SC) {
    printf("%c", gb->memory[REG_SB]);
  } else if (address == REG_DIV) {
    gb->memory[REG_DIV] = 0;
  } else {
    memcpy(gb->memory + address, data, len);
  }
}

void gameboy_print_instruction(Gameboy* gb) {
  unsigned char opcode;
  gameboy_read_mem(gb, gb->PC, &opcode, 1);

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
    gameboy_read_mem(gb, gb->PC + 1, &arg, 1);
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
    gameboy_read_mem(gb, gb->PC + 1, (unsigned char*)&arg, 2);
    printf("arg: %05d (0x%04x)\n", arg, arg);
  } else {
    printf("\n");
  }
}

void gameboy_dump_registers(Gameboy* gb) {
  /* printf( */
  /*   "%d%d%d%d%d%d%d%d\n", */
  /*   BIT(gb->A, 7), */
  /*   BIT(gb->A, 6), */
  /*   BIT(gb->A, 5), */
  /*   BIT(gb->A, 4), */
  /*   BIT(gb->A, 3), */
  /*   BIT(gb->A, 2), */
  /*   BIT(gb->A, 1), */
  /*   BIT(gb->A, 0) */
  /* ); */
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

      gb->A, gb->A,
      gb->B, gb->B,
      gb->C, gb->C,
      gb->D, gb->D,
      gb->E, gb->E,
      gb->H, gb->H,
      gb->L, gb->L,
      gb->F, gb->F,
      CPU_FLAG_Z(gb->F),
      CPU_FLAG_H(gb->F),
      CPU_FLAG_N(gb->F),
      CPU_FLAG_C(gb->F),
      CPU_AF(gb), CPU_AF(gb),
      CPU_BC(gb), CPU_BC(gb),
      CPU_DE(gb), CPU_DE(gb),
      CPU_HL(gb), CPU_HL(gb),
      gb->SP, gb->SP,
      gb->PC, gb->PC
  );
}

void gameboy_dump_stack(Gameboy* gb) {
  printf("Stack:\n");
  for (unsigned short addr = 0xfffe; addr >= 0xfff0; addr--) {
    printf("0x%02x: %03d (0x%02d)\n", addr, gb->memory[addr], gb->memory[addr]);
  }
}

void gameboy_execute_cb_instruction(Gameboy* gb, int* num_cycles) {
  // PC is already pointing at the byte after the CB.

  unsigned char opcode;
  gameboy_read_mem(gb, gb->PC, &opcode, 1);
  gb->PC++;

  unsigned char hl_value;

  bool handled_by_hl = true;

  switch (opcode) {
    case 0x06:
      // RLC (HL)
      {
        gameboy_read_mem(gb, CPU_HL(gb), &hl_value, 1);
        unsigned char carry;
        hl_value = rlc(hl_value, &carry);
        gameboy_write_mem(gb, CPU_HL(gb), &hl_value, 1);
        gb->F = CPU_F(hl_value == 0 ? 1 : 0, 0, 0, carry);
      }
      break;
    case 0x0e:
      // RRC (HL)
      {
        gameboy_read_mem(gb, CPU_HL(gb), &hl_value, 1);
        unsigned char carry;
        hl_value = rrc(hl_value, &carry);
        gameboy_write_mem(gb, CPU_HL(gb), &hl_value, 1);
        gb->F = CPU_F(hl_value == 0 ? 1 : 0, 0, 0, carry);
      }
      break;
    case 0x16:
      // RL (HL)
      {
        gameboy_read_mem(gb, CPU_HL(gb), &hl_value, 1);
        unsigned char carry;
        hl_value = rl(hl_value, CPU_FLAG_C(gb->F), &carry);
        gameboy_write_mem(gb, CPU_HL(gb), &hl_value, 1);
        gb->F = CPU_F(hl_value == 0 ? 1 : 0, 0, 0, carry);
      }
      break;
    case 0x1e:
      // RR (HL)
      {
        gameboy_read_mem(gb, CPU_HL(gb), &hl_value, 1);
        unsigned char carry;
        hl_value = rr(hl_value, CPU_FLAG_C(gb->F), &carry);
        gameboy_write_mem(gb, CPU_HL(gb), &hl_value, 1);
        gb->F = CPU_F(hl_value == 0 ? 1 : 0, 0, 0, carry);
      }
      break;
    case 0x26:
      // SLA (HL)
      {
        gameboy_read_mem(gb, CPU_HL(gb), &hl_value, 1);
        unsigned char carry;
        hl_value = sla(hl_value, &carry);
        gameboy_write_mem(gb, CPU_HL(gb), &hl_value, 1);
        gb->F = CPU_F(hl_value == 0 ? 1 : 0, 0, 0, carry);
      }
      break;
    case 0x2e:
      // SRA (HL)
      {
        gameboy_read_mem(gb, CPU_HL(gb), &hl_value, 1);
        unsigned char carry;
        hl_value = sra(hl_value, &carry);
        gameboy_write_mem(gb, CPU_HL(gb), &hl_value, 1);
        gb->F = CPU_F(hl_value == 0 ? 1 : 0, 0, 0, carry);
      }
      break;
    case 0x36:
      // SWAP (HL)
      {
        gameboy_read_mem(gb, CPU_HL(gb), &hl_value, 1);
        hl_value = swap(hl_value);
        gameboy_write_mem(gb, CPU_HL(gb), &hl_value, 1);
        gb->F = CPU_F(hl_value == 0 ? 1 : 0, 0, 0, 0);
      }
      break;
    case 0x3e:
      // SRL (HL)
      {
        gameboy_read_mem(gb, CPU_HL(gb), &hl_value, 1);
        unsigned char carry;
        hl_value = srl(hl_value, &carry);
        gameboy_write_mem(gb, CPU_HL(gb), &hl_value, 1);
        gb->F = CPU_F(hl_value == 0 ? 1 : 0, 0, 0, carry);
      }
      break;

    case 0x46:
    case 0x4e:
    case 0x56:
    case 0x5e:
    case 0x66:
    case 0x6e:
    case 0x76:
    case 0x7e:
      // BIT b, (HL)
      {
        gameboy_read_mem(gb, CPU_HL(gb), &hl_value, 1);
        unsigned char target_bit = ((opcode >> 4) - 4) * 2 + (opcode & 0xf) / 8;
        gb->F = CPU_F(BIT(hl_value, target_bit) == 0 ? 1 : 0, 0, 1, CPU_FLAG_C(gb->F));
      }
      break;

    case 0x86:
    case 0x8e:
    case 0x96:
    case 0x9e:
    case 0xa6:
    case 0xae:
    case 0xb6:
    case 0xbe:
      // RES b, (HL)
      {
        gameboy_read_mem(gb, CPU_HL(gb), &hl_value, 1);
        unsigned char target_bit = ((opcode >> 4) - 8) * 2 + (opcode & 0xf) / 8;
        hl_value = UNSET_BIT(hl_value, target_bit);
        gameboy_write_mem(gb, CPU_HL(gb), &hl_value, 1);
      }
      break;

    case 0xc6:
    case 0xce:
    case 0xd6:
    case 0xde:
    case 0xe6:
    case 0xee:
    case 0xf6:
    case 0xfe:
      // SET b, (HL)
      {
        gameboy_read_mem(gb, CPU_HL(gb), &hl_value, 1);
        unsigned char target_bit = ((opcode >> 4) - 0xc) * 2 + (opcode & 0xf) / 8;
        hl_value = SET_BIT(hl_value, target_bit);
        gameboy_write_mem(gb, CPU_HL(gb), &hl_value, 1);
      }
      break;

    default:
      handled_by_hl = false;
  }

  if (handled_by_hl) {
    *num_cycles = 16;
    return;
  }

  unsigned char low_nibble = opcode & 0xf;

  unsigned char* reg;
  switch (low_nibble % 8) {
    case 0:
      reg = &gb->B;
      break;
    case 1:
      reg = &gb->C;
      break;
    case 2:
      reg = &gb->D;
      break;
    case 3:
      reg = &gb->E;
      break;
    case 4:
      reg = &gb->H;
      break;
    case 5:
      reg = &gb->L;
      break;
    case 7:
      reg = &gb->A;
      break;
    default:
      assert(false);
  }

  unsigned char high_nibble = opcode >> 4;

  switch (high_nibble) {
    case 0:
      if (low_nibble < 8) {
        // RLC
        unsigned char carry;
        *reg = rlc(*reg, &carry);
        gb->F = CPU_F(*reg == 0 ? 1 : 0, 0, 0, carry);
      } else {
        // RRC
        unsigned char carry;
        *reg = rrc(*reg, &carry);
        gb->F = CPU_F(*reg == 0 ? 1 : 0, 0, 0, carry);
      }
      break;

    case 1:
      if (low_nibble < 8) {
        // RL
        unsigned char carry;
        *reg = rl(*reg, CPU_FLAG_C(gb->F), &carry);
        gb->F = CPU_F(*reg == 0 ? 1 : 0, 0, 0, carry);
      } else {
        // RR
        unsigned char carry;
        *reg = rr(*reg, CPU_FLAG_C(gb->F), &carry);
        gb->F = CPU_F(*reg == 0 ? 1 : 0, 0, 0, carry);
      }
      break;

    case 2:
      if (low_nibble < 8) {
        // SLA
        unsigned char carry;
        *reg = sla(*reg, &carry);
        gb->F = CPU_F(*reg == 0 ? 1 : 0, 0, 0, carry);
      } else {
        // SRA
        unsigned char carry;
        *reg = sra(*reg, &carry);
        gb->F = CPU_F(*reg == 0 ? 1 : 0, 0, 0, carry);
      }
      break;

    case 3:
      if (low_nibble < 8) {
        // SWAP
        *reg = swap(*reg);
        gb->F = CPU_F(*reg == 0 ? 1 : 0, 0, 0, 0);
      } else {
        // SRL
        unsigned char carry;
        *reg = srl(*reg, &carry);
        gb->F = CPU_F(*reg == 0 ? 1 : 0, 0, 0, carry);
      }
      break;

    case 4:
    case 5:
    case 6:
    case 7:
      {
        // BIT
        unsigned char target_bit = (high_nibble - 4) * 2 + low_nibble / 8;
        gb->F = CPU_F(BIT(*reg, target_bit) == 0 ? 1 : 0, 0, 1, CPU_FLAG_C(gb->F));
      }
      break;

    case 8:
    case 9:
    case 10:
    case 11:
      {
        // RES
        unsigned char target_bit = (high_nibble - 8) * 2 + low_nibble / 8;
        *reg = UNSET_BIT(*reg, target_bit);
      }
      break;

    case 12:
    case 13:
    case 14:
    case 15:
      {
        // SET
        unsigned char target_bit = (high_nibble - 12) * 2 + low_nibble / 8;
        *reg = SET_BIT(*reg, target_bit);
      }
      break;

    default:
      assert(false);
  }

  *num_cycles = 8;
}

int gameboy_execute_instruction(Gameboy* gb) {
  if (DEBUG) {
    printf("Executing: ");
    gameboy_print_instruction(gb);
  }

  unsigned char opcode;
  gameboy_read_mem(gb, gb->PC, &opcode, 1);
  gb->PC++;

  int num_cycles;
  switch (opcode) {
    case 0x00:
      // NOP
      num_cycles = 4;
      break;

    case 0x01:
      // LD BC,d16
      gameboy_read_mem(gb, gb->PC, (unsigned char*)CPU_BC_REF(gb), 2);
      gb->PC += 2;
      num_cycles = 12;
      break;

    case 0x02:
      // LD (BC),A
      gameboy_write_mem(gb, CPU_BC(gb), &gb->A, 1);
      num_cycles = 8;
      break;

    case 0x03:
      // INC BC
      *CPU_BC_REF(gb) = *CPU_BC_REF(gb) + 1;
      num_cycles = 8;
      break;

    case 0x04:
      // INC B
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(gb->B, 1, &gb->B, &carry, &half_carry);
        gb->F = CPU_F(gb->B == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x05:
      // DEC B
      {
        unsigned char borrow;
        unsigned char half_borrow;
        sub_8_bit(gb->B, 1, &gb->B, &borrow, &half_borrow);
        gb->F = CPU_F(gb->B == 0 ? 1 : 0, 1, half_borrow, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x06:
      // LD B,d8
      gameboy_read_mem(gb, gb->PC, (unsigned char*)&gb->B, 1);
      gb->PC++;
      num_cycles = 8;
      break;

    case 0x07:
      // RLCA
      {
        unsigned char carry;
        gb->A = rlc(gb->A, &carry);
        gb->F = CPU_F(0, 0, 0, carry); // TODO: Z flag is a matter of debate. Could be 0, 1, or Z.
        num_cycles = 4;
      }
      break;

    case 0x08:
      // LD (a16),SP
      {
        unsigned short address;
        gameboy_read_mem(gb, gb->PC, (unsigned char*)&address, 2);
        gb->PC += 2;
        gameboy_write_mem(gb, address, (unsigned char*)&gb->SP, 2);
        num_cycles = 20;
      }
      break;

    case 0x09:
      // ADD HL,BC
      {
        unsigned char carry;
        unsigned char half_carry;
        add_16_bit(CPU_HL(gb), CPU_BC(gb), CPU_HL_REF(gb), &carry, &half_carry);
        gb->F = CPU_F(CPU_FLAG_Z(gb->F), 0, half_carry, carry);
        num_cycles = 8;
      }
      break;

    case 0x0a:
      // LD A,(BC)
      gameboy_read_mem(gb, CPU_BC(gb), &gb->A, 1);
      num_cycles = 8;
      break;

    case 0x0b:
      // DEC BC
      *CPU_BC_REF(gb) = *CPU_BC_REF(gb) - 1;
      num_cycles = 8;
      break;

    case 0x0c:
      // INC C
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(gb->C, 1, &gb->C, &carry, &half_carry);
        gb->F = CPU_F(gb->C == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x0d:
      // DEC C
      {
        unsigned char borrow;
        unsigned char half_borrow;
        sub_8_bit(gb->C, 1, &gb->C, &borrow, &half_borrow);
        gb->F = CPU_F(gb->C == 0 ? 1 : 0, 1, half_borrow, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x0e:
      // LD C,d8
      gameboy_read_mem(gb, gb->PC, &gb->C, 1);
      gb->PC++;
      num_cycles = 8;
      break;

    case 0x0f:
      // RRCA
      {
        unsigned char carry;
        gb->A = rrc(gb->A, &carry);
        gb->F = CPU_F(0, 0, 0, carry); // TODO: Z flag is a matter of debate. Could be 0, 1, or Z.
        num_cycles = 4;
      }
      break;

    case 0x10:
      // STOP 0
      printf("time: %llu\n", gb->global_simulated_ticks);
      gameboy_dump_registers(gb);
      assert(false);
      gb->PC++;
      num_cycles = 4;
      break;

    case 0x11:
      // LD DE,d16
      gameboy_read_mem(gb, gb->PC, (unsigned char*)CPU_DE_REF(gb), 2);
      gb->PC += 2;
      num_cycles = 12;
      break;

    case 0x12:
      // LD (DE),A
      gameboy_write_mem(gb, CPU_DE(gb), &gb->A, 1);
      num_cycles = 8;
      break;

    case 0x13:
      // INC DE
      *CPU_DE_REF(gb) = *CPU_DE_REF(gb) + 1;
      num_cycles = 8;
      break;

    case 0x14:
      // INC D
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(gb->D, 1, &gb->D, &carry, &half_carry);
        gb->F = CPU_F(gb->D == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x15:
      // DEC D
      {
        unsigned char borrow;
        unsigned char half_borrow;
        sub_8_bit(gb->D, 1, &gb->D, &borrow, &half_borrow);
        gb->F = CPU_F(gb->D == 0 ? 1 : 0, 1, half_borrow, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x16:
      // LD D,d8
      gameboy_read_mem(gb, gb->PC, (unsigned char*)&gb->D, 1);
      gb->PC++;
      num_cycles = 8;
      break;

    case 0x17:
      // RLA
      {
        unsigned char carry;
        gb->A = rl(gb->A, CPU_FLAG_C(gb->F), &carry);
        gb->F = CPU_F(0, 0, 0, carry); // TODO: Z flag is up for debate.
        num_cycles = 4;
      }
      break;

    case 0x18:
      // JR r8
      {
        char offset;
        gameboy_read_mem(gb, gb->PC, (unsigned char*)&offset, 1);
        gb->PC++;
        gb->PC += offset;
        num_cycles = 12;
      }
      break;

    case 0x19:
      // ADD HL,DE
      {
        unsigned char carry;
        unsigned char half_carry;
        add_16_bit(CPU_HL(gb), CPU_DE(gb), CPU_HL_REF(gb), &carry, &half_carry);
        gb->F = CPU_F(CPU_FLAG_Z(gb->F), 0, half_carry, carry);
        num_cycles = 8;
      }
      break;

    case 0x1a:
      // LD A,(DE)
      gameboy_read_mem(gb, CPU_DE(gb), &gb->A, 1);
      num_cycles = 8;
      break;

    case 0x1b:
      // DEC DE
      *CPU_DE_REF(gb) = *CPU_DE_REF(gb) - 1;
      num_cycles = 8;
      break;

    case 0x1c:
      // INC E
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(gb->E, 1, &gb->E, &carry, &half_carry);
        gb->F = CPU_F(gb->E == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x1d:
      // DEC E
      {
        unsigned char borrow;
        unsigned char half_borrow;
        sub_8_bit(gb->E, 1, &gb->E, &borrow, &half_borrow);
        gb->F = CPU_F(gb->E == 0 ? 1 : 0, 1, half_borrow, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x1e:
      // LD E,d8
      gameboy_read_mem(gb, gb->PC, &gb->E, 1);
      gb->PC++;
      num_cycles = 8;
      break;

    case 0x1f:
      // RRA
      {
        unsigned char carry;
        gb->A = rr(gb->A, CPU_FLAG_C(gb->F), &carry);
        gb->F = CPU_F(0, 0, 0, carry); // TODO: Z flag is up for debate.
        num_cycles = 4;
      }
      break;

    case 0x20: // JR NZ,r8
    case 0x28: // JR Z,r8
    case 0x30: // JR NC,r8
    case 0x38: // JR C,r8
      {
        char offset;
        gameboy_read_mem(gb, gb->PC, (unsigned char*)&offset, 1);
        gb->PC++;

        bool jump;
        switch (opcode) {
          case 0x20:
            jump = !CPU_FLAG_Z(gb->F);
            break;
          case 0x28:
            jump = CPU_FLAG_Z(gb->F);
            break;
          case 0x30:
            jump = !CPU_FLAG_C(gb->F);
            break;
          case 0x38:
            jump = CPU_FLAG_C(gb->F);
            break;
          default:
            assert(false);
        }

        if (jump) {
          gb->PC += offset;
          num_cycles = 12;
        } else {
          num_cycles = 8;
        }
      }
      break;

    case 0x21:
      // LD HL,d16
      gameboy_read_mem(gb, gb->PC, (unsigned char*)CPU_HL_REF(gb), 2);
      gb->PC += 2;
      num_cycles = 12;
      break;

    case 0x22:
      // LD (HL+),A
      gameboy_write_mem(gb, CPU_HL(gb), &gb->A, 1);
      *CPU_HL_REF(gb) = CPU_HL(gb) + 1;
      num_cycles = 8;
      break;

    case 0x23:
      // INC HL
      *CPU_HL_REF(gb) = CPU_HL(gb) + 1;
      num_cycles = 8;
      break;

    case 0x24:
      // INC H
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(gb->H, 1, &gb->H, &carry, &half_carry);
        gb->F = CPU_F(gb->H == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x25:
      // DEC H
      {
        unsigned char borrow;
        unsigned char half_borrow;
        sub_8_bit(gb->H, 1, &gb->H, &borrow, &half_borrow);
        gb->F = CPU_F(gb->H == 0 ? 1 : 0, 1, half_borrow, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x26:
      // LD H,d8
      gameboy_read_mem(gb, gb->PC, (unsigned char*)&gb->H, 1);
      gb->PC++;
      num_cycles = 8;
      break;

    case 0x27:
      // DAA
      // TODO: WTF is this: http://z80-heaven.wikidot.com/instructions-set:daa
      // TODO: probably broken.
      {
        unsigned char low_nibble = gb->A & 0xf;
        if (low_nibble > 9 || CPU_FLAG_H(gb->F)) {
          gb->A += 6;
        }

        unsigned char high_nibble = gb->A >> 4;
        bool do_second_addition = high_nibble > 9 || CPU_FLAG_C(gb->F);
        if (do_second_addition) {
          gb->A += 0x60;
        }

        gb->F = CPU_F(gb->A == 0, CPU_FLAG_N(gb->F), 0, do_second_addition ? 1 : 0);
        num_cycles = 4;
      }
      break;

    case 0x29:
      // ADD HL,HL
      {
        unsigned char carry;
        unsigned char half_carry;
        add_16_bit(CPU_HL(gb), CPU_HL(gb), CPU_HL_REF(gb), &carry, &half_carry);
        gb->F = CPU_F(CPU_FLAG_Z(gb->F), 0, half_carry, carry);
        num_cycles = 8;
      }
      break;

    case 0x2a:
      // LD A,(HL+)
      gameboy_read_mem(gb, CPU_HL(gb), &gb->A, 1);
      *CPU_HL_REF(gb) = CPU_HL(gb) + 1;
      num_cycles = 8;
      break;

    case 0x2b:
      // DEC HL
      *CPU_HL_REF(gb) = CPU_HL(gb) - 1;
      num_cycles = 8;
      break;

    case 0x2c:
      // INC L
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(gb->L, 1, &gb->L, &carry, &half_carry);
        gb->F = CPU_F(gb->L == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x2d:
      // DEC L
      {
        unsigned char borrow;
        unsigned char half_borrow;
        sub_8_bit(gb->L, 1, &gb->L, &borrow, &half_borrow);
        gb->F = CPU_F(gb->L == 0 ? 1 : 0, 1, half_borrow, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x2e:
      // LD L,d8
      gameboy_read_mem(gb, gb->PC, &gb->L, 1);
      gb->PC++;
      num_cycles = 8;
      break;

    case 0x2f:
      // CPL
      gb->A = ~gb->A;
      gb->F = CPU_F(CPU_FLAG_Z(gb->F), 1, 1, CPU_FLAG_C(gb->F));
      num_cycles = 4;
      break;

    case 0x31:
      // LD SP,d16
      gameboy_read_mem(gb, gb->PC, (unsigned char*)&gb->SP, 2);
      gb->PC += 2;
      num_cycles = 12;
      break;

    case 0x32:
      // LD (HL-),A
      gameboy_write_mem(gb, CPU_HL(gb), &gb->A, 1);
      *CPU_HL_REF(gb) = CPU_HL(gb) - 1;
      num_cycles = 8;
      break;

    case 0x33:
      // INC SP
      gb->SP++;
      num_cycles = 8;
      break;

    case 0x34:
      // INC (HL)
      {
        unsigned char value;
        gameboy_read_mem(gb, CPU_HL(gb), &value, 1);

        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(value, 1, &value, &carry, &half_carry);
        gameboy_write_mem(gb, CPU_HL(gb), &value, 1);
        gb->F = CPU_F(value == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(gb->F));
        num_cycles = 12;
      }
      break;

    case 0x35:
      // DEC (HL)
      {
        unsigned char value;
        gameboy_read_mem(gb, CPU_HL(gb), &value, 1);

        unsigned char borrow;
        unsigned char half_borrow;
        sub_8_bit(value, 1, &value, &borrow, &half_borrow);
        gameboy_write_mem(gb, CPU_HL(gb), &value, 1);
        gb->F = CPU_F(value == 0 ? 1 : 0, 1, half_borrow, CPU_FLAG_C(gb->F));
        num_cycles = 12;
      }
      break;

    case 0x36:
      // LD (HL),d8
      {
        unsigned char value;
        gameboy_read_mem(gb, gb->PC, &value, 1);
        gb->PC++;

        gameboy_write_mem(gb, CPU_HL(gb), &value, 1);
        num_cycles = 12;
      }
      break;

    case 0x37:
      // SCF
      gb->F = CPU_F(CPU_FLAG_Z(gb->F), 0, 0, 1);
      num_cycles = 4;
      break;

    case 0x39:
      // ADD HL,SP
      {
        unsigned char carry;
        unsigned char half_carry;
        add_16_bit(CPU_HL(gb), gb->SP, CPU_HL_REF(gb), &carry, &half_carry);
        gb->F = CPU_F(CPU_FLAG_Z(gb->F), 0, half_carry, carry);
        num_cycles = 8;
      }
      break;

    case 0x3a:
      // LD A,(HL-)
      gameboy_read_mem(gb, CPU_HL(gb), &gb->A, 1);
      *CPU_HL_REF(gb) = CPU_HL(gb) - 1;
      num_cycles = 8;
      break;

    case 0x3b:
      // DEC SP
      gb->SP--;
      num_cycles = 8;
      break;

    case 0x3c:
      // INC A
      {
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(gb->A, 1, &gb->A, &carry, &half_carry);
        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, half_carry, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x3d:
      // DEC A
      {
        unsigned char borrow;
        unsigned char half_borrow;
        sub_8_bit(gb->A, 1, &gb->A, &borrow, &half_borrow);
        gb->F = CPU_F(gb->A == 0, 1, half_borrow, CPU_FLAG_C(gb->F));
        num_cycles = 4;
      }
      break;

    case 0x3e:
      // LD A,d8
      gameboy_read_mem(gb, gb->PC, &gb->A, 1);
      gb->PC++;
      num_cycles = 8;
      break;

    case 0x3f:
      // CCF
      gb->F = CPU_F(CPU_FLAG_Z(gb->F), 0, 0, CPU_FLAG_C(gb->F) ? 0 : 1);
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
              dest = &gb->B;
            else
              dest = &gb->C;
            break;
          case 5:
            if (low_nibble <= 7)
              dest = &gb->D;
            else
              dest = &gb->E;
            break;
          case 6:
            if (low_nibble <= 7)
              dest = &gb->H;
            else
              dest = &gb->L;
            break;
          case 7:
            dest = &gb->A;
            break;
          default:
            assert(false);
        }

        unsigned char* src = NULL;
        switch (low_nibble % 8) {
          case 0:
            src = &gb->B;
            break;
          case 1:
            src = &gb->C;
            break;
          case 2:
            src = &gb->D;
            break;
          case 3:
            src = &gb->E;
            break;
          case 4:
            src = &gb->H;
            break;
          case 5:
            src = &gb->L;
            break;
          case 7:
            src = &gb->A;
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
      gameboy_read_mem(gb, CPU_HL(gb), &gb->B, 1);
      num_cycles = 8;
      break;

    case 0x4e:
      // LD C,(HL)
      gameboy_read_mem(gb, CPU_HL(gb), &gb->C, 1);
      num_cycles = 8;
      break;

    case 0x56:
      // LD D,(HL)
      gameboy_read_mem(gb, CPU_HL(gb), &gb->D, 1);
      num_cycles = 8;
      break;

    case 0x5e:
      // LD E,(HL)
      gameboy_read_mem(gb, CPU_HL(gb), &gb->E, 1);
      num_cycles = 8;
      break;

    case 0x66:
      // LD H,(HL)
      gameboy_read_mem(gb, CPU_HL(gb), &gb->H, 1);
      num_cycles = 8;
      break;

    case 0x6e:
      // LD L,(HL)
      gameboy_read_mem(gb, CPU_HL(gb), &gb->L, 1);
      num_cycles = 8;
      break;

    case 0x70:
      // LD (HL),B
      gameboy_write_mem(gb, CPU_HL(gb), &gb->B, 1);
      num_cycles = 8;
      break;

    case 0x71:
      // LD (HL),C
      gameboy_write_mem(gb, CPU_HL(gb), &gb->C, 1);
      num_cycles = 8;
      break;

    case 0x72:
      // LD (HL),D
      gameboy_write_mem(gb, CPU_HL(gb), &gb->D, 1);
      num_cycles = 8;
      break;

    case 0x73:
      // LD (HL),E
      gameboy_write_mem(gb, CPU_HL(gb), &gb->E, 1);
      num_cycles = 8;
      break;

    case 0x74:
      // LD (HL),H
      gameboy_write_mem(gb, CPU_HL(gb), &gb->H, 1);
      num_cycles = 8;
      break;

    case 0x75:
      // LD (HL),L
      gameboy_write_mem(gb, CPU_HL(gb), &gb->L, 1);
      num_cycles = 8;
      break;

    case 0x76:
      // HALT
      DEBUG = 1;
      /* if (COUNTDOWN == 0) */
      /*   COUNTDOWN = 150; */
      num_cycles = 4;
      break;

    case 0x77:
      // LD (HL),A
      gameboy_write_mem(gb, CPU_HL(gb), &gb->A, 1);
      num_cycles = 8;
      break;

    case 0x7e:
      // LD A,(HL)
      gameboy_read_mem(gb, CPU_HL(gb), &gb->A, 1);
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
            source = gb->B;
            break;
          case 1:
            source = gb->C;
            break;
          case 2:
            source = gb->D;
            break;
          case 3:
            source = gb->E;
            break;
          case 4:
            source = gb->H;
            break;
          case 5:
            source = gb->L;
            break;
          case 7:
            source = gb->A;
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
              add_8_bit(gb->A, source, &gb->A, &carry, &half_carry);
              gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, half_carry, carry);
            } else {
              // ADC
              unsigned char carry_1;
              unsigned char half_carry_1;
              add_8_bit(gb->A, source, &gb->A, &carry_1, &half_carry_1);

              unsigned char carry_2;
              unsigned char half_carry_2;
              add_8_bit(gb->A, CPU_FLAG_C(gb->F), &gb->A, &carry_2, &half_carry_2);

              gb->F = CPU_F(
                gb->A == 0 ? 1 : 0,
                0,
                half_carry_1 || half_carry_2 ? 1 : 0,
                carry_1 || carry_2 ? 1 : 0
              );
            }
            break;

          case 9:
            if (low_nibble < 8) {
              // SUB
              unsigned char borrow;
              unsigned char half_borrow;
              sub_8_bit(gb->A, source, &gb->A, &borrow, &half_borrow);
              gb->F = CPU_F(gb->A == 0 ? 1 : 0, 1, half_borrow, borrow);
            } else {
              // SBC
              unsigned char borrow_1;
              unsigned char half_borrow_1;
              sub_8_bit(gb->A, source, &gb->A, &borrow_1, &half_borrow_1);

              unsigned char borrow_2;
              unsigned char half_borrow_2;
              sub_8_bit(gb->A, CPU_FLAG_C(gb->F), &gb->A, &borrow_2, &half_borrow_2);

              gb->F = CPU_F(
                gb->A == 0 ? 1 : 0,
                1,
                half_borrow_1 || half_borrow_2 ? 1 : 0,
                borrow_1 || borrow_2 ? 1 : 0
              );
            }
            break;

          case 10:
            if (low_nibble < 8) {
              // AND
              gb->A = gb->A & source;
              gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, 1, 0);
            } else {
              // XOR
              gb->A = gb->A ^ source;
              gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, 0, 0);
            }
            break;

          case 11:
            if (low_nibble < 8) {
              // OR
              gb->A = gb->A | source;
              gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, 0, 0);
            } else {
              // CP
              unsigned char trash;
              unsigned char borrow;
              unsigned char half_borrow;
              sub_8_bit(gb->A, source, &trash, &borrow, &half_borrow);
              gb->F = CPU_F(trash == 0 ? 1 : 0, 1, half_borrow, borrow);
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
        gameboy_read_mem(gb, CPU_HL(gb), &value, 1);

        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(gb->A, value, &gb->A, &carry, &half_carry);
        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, half_carry, carry);
        num_cycles = 8;
      }
      break;

    case 0x8e:
      // ADC A,(HL)
      {
        unsigned char value;
        gameboy_read_mem(gb, CPU_HL(gb), &value, 1);

        unsigned char carry_1;
        unsigned char half_carry_1;
        add_8_bit(gb->A, value, &gb->A, &carry_1, &half_carry_1);

        unsigned char carry_2;
        unsigned char half_carry_2;
        add_8_bit(gb->A, CPU_FLAG_C(gb->F), &gb->A, &carry_2, &half_carry_2);

        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, half_carry_1 || half_carry_2 ? 1 : 0, carry_1 || carry_2 ? 1 : 0);
        num_cycles = 8;
      }
      break;

    case 0x96:
      // SUB (HL)
      {
        unsigned char value;
        gameboy_read_mem(gb, CPU_HL(gb), &value, 1);

        unsigned char carry;
        unsigned char half_carry;
        sub_8_bit(gb->A, value, &gb->A, &carry, &half_carry);
        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 1, half_carry, carry);
        num_cycles = 8;
      }
      break;

    case 0x9e:
      // SBC A,(HL)
      {
        unsigned char value;
        gameboy_read_mem(gb, CPU_HL(gb), &value, 1);

        unsigned char carry_1;
        unsigned char half_carry_1;
        sub_8_bit(gb->A, value, &gb->A, &carry_1, &half_carry_1);

        unsigned char carry_2;
        unsigned char half_carry_2;
        sub_8_bit(gb->A, CPU_FLAG_C(gb->F), &gb->A, &carry_2, &half_carry_2);

        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 1, half_carry_1 || half_carry_2 ? 1 : 0, carry_1 || carry_2 ? 1 : 0);
        num_cycles = 8;
      }
      break;

    case 0xa6:
      // AND (HL)
      {
        unsigned char value;
        gameboy_read_mem(gb, CPU_HL(gb), &value, 1);

        gb->A = gb->A & value;
        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, 1, 0);
        num_cycles = 8;
      }
      break;

    case 0xae:
      // XOR (HL)
      {
        unsigned char value;
        gameboy_read_mem(gb, CPU_HL(gb), &value, 1);

        gb->A = gb->A ^ value;
        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, 0, 0);
        num_cycles = 8;
      }
      break;

    case 0xb6:
      // OR (HL)
      {
        unsigned char value;
        gameboy_read_mem(gb, CPU_HL(gb), &value, 1);

        gb->A = gb->A | value;
        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, 0, 0);
        num_cycles = 8;
      }
      break;

    case 0xbe:
      // CP (HL)
      {
        unsigned char value;
        gameboy_read_mem(gb, CPU_HL(gb), &value, 1);

        unsigned char trash;
        unsigned char borrow;
        unsigned char half_borrow;
        sub_8_bit(gb->A, value, &trash, &borrow, &half_borrow);
        gb->F = CPU_F(trash == 0 ? 1 : 0, 1, half_borrow, borrow);

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
            ret = !CPU_FLAG_Z(gb->F);
            break;
          case 0xc8:
            ret = CPU_FLAG_Z(gb->F);
            break;
          case 0xd0:
            ret = !CPU_FLAG_C(gb->F);
            break;
          case 0xd8:
            ret = CPU_FLAG_C(gb->F);
            break;
          default:
            assert(false);
        }

        if (ret) {
          gameboy_read_mem(gb, gb->SP, (unsigned char*)&gb->PC, 2);
          gb->SP += 2;
          num_cycles = 20;
        } else {
          num_cycles = 8;
        }
      }
      break;

    case 0xc9:
      // RET
      gameboy_read_mem(gb, gb->SP, (unsigned char*)&gb->PC, 2);
      gb->SP += 2;
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
            dest = CPU_BC_REF(gb);
            break;
          case 13:
            dest = CPU_DE_REF(gb);
            break;
          case 14:
            dest = CPU_HL_REF(gb);
            break;
          case 15:
            dest = CPU_AF_REF(gb);
            break;
          default:
            assert(false);
        }

        gameboy_read_mem(gb, gb->SP, (unsigned char*)dest, 2);
        gb->SP += 2;

        if (opcode == 0xf1) {
          // Discard lower nibble.
          gb->F = (gb->F >> 4) << 4;
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
          case 0xc2:
            jump = !CPU_FLAG_Z(gb->F);
            break;
          case 0xca:
            jump = CPU_FLAG_Z(gb->F);
            break;
          case 0xd2:
            jump = !CPU_FLAG_C(gb->F);
            break;
          case 0xda:
            jump = CPU_FLAG_C(gb->F);
            break;
          default:
            assert(false);
        }

        unsigned short dest;
        gameboy_read_mem(gb, gb->PC, (unsigned char*)&dest, 2);
        gb->PC += 2;

        if (jump) {
          gb->PC = dest;
          num_cycles = 16;
        } else {
          num_cycles = 12;
        }
      }
      break;

    case 0xc3:
      // JP a16
      gameboy_read_mem(gb, gb->PC, (unsigned char*)&gb->PC, 2);
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
            call = !CPU_FLAG_Z(gb->F);
            break;
          case 0xcc:
            call = CPU_FLAG_Z(gb->F);
            break;
          case 0xd4:
            call = !CPU_FLAG_C(gb->F);
            break;
          case 0xdc:
            call = CPU_FLAG_C(gb->F);
            break;
          default:
            assert(false);
        }

        unsigned short dest;
        gameboy_read_mem(gb, gb->PC, (unsigned char*)&dest, 2);
        gb->PC += 2;

        if (call) {
          gb->SP -= 2;
          gameboy_write_mem(gb, gb->SP, (unsigned char*)&gb->PC, 2);
          gb->PC = dest;
          num_cycles = 24;
        } else {
          num_cycles = 12;
        }
      }
      break;

    case 0xcd: // CALL a16
      {
        unsigned short dest;
        gameboy_read_mem(gb, gb->PC, (unsigned char*)&dest, 2);
        gb->PC += 2;

        gb->SP -= 2;
        gameboy_write_mem(gb, gb->SP, (unsigned char*)&gb->PC, 2);
        gb->PC = dest;
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
            src = CPU_BC_REF(gb);
            break;
          case 13:
            src = CPU_DE_REF(gb);
            break;
          case 14:
            src = CPU_HL_REF(gb);
            break;
          case 15:
            src = CPU_AF_REF(gb);
            break;
          default:
            assert(false);
        }

        gb->SP -= 2;
        gameboy_write_mem(gb, gb->SP, (unsigned char*)src, 2);

        num_cycles = 16;
      }
      break;

    case 0xc6:
      // ADD A,d8
      {
        unsigned char source;
        gameboy_read_mem(gb, gb->PC, &source, 1);
        gb->PC++;

        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(gb->A, source, &gb->A, &carry, &half_carry);
        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, half_carry, carry);
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

        gb->SP -= 2;
        gameboy_write_mem(gb, gb->SP, (unsigned char*)&gb->PC, 2);

        gb->PC = dest;
        num_cycles = 16;
      }
      break;

    case 0xcb:
      // PREFIX CB
      gameboy_execute_cb_instruction(gb, &num_cycles);
      break;

    case 0xce:
      // ADC A,d8
      {
        unsigned char value;
        gameboy_read_mem(gb, gb->PC, &value, 1);
        gb->PC++;

        unsigned char carry_1;
        unsigned char half_carry_1;
        add_8_bit(gb->A, value, &gb->A, &carry_1, &half_carry_1);

        unsigned char carry_2;
        unsigned char half_carry_2;
        add_8_bit(gb->A, CPU_FLAG_C(gb->F), &gb->A, &carry_2, &half_carry_2);

        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, half_carry_1 || half_carry_2 ? 1 : 0, carry_1 || carry_2 ? 1 : 0);
        num_cycles = 8;
      }
      break;

    case 0xd6:
      // SUB d8
      {
        unsigned char source;
        gameboy_read_mem(gb, gb->PC, &source, 1);
        gb->PC++;

        unsigned char borrow;
        unsigned char half_borrow;
        sub_8_bit(gb->A, source, &gb->A, &borrow, &half_borrow);

        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 1, half_borrow, borrow);
        num_cycles = 8;
      }
      break;

    case 0xd9:
      // RETI
      gameboy_read_mem(gb, gb->SP, (unsigned char*)&gb->PC, 2);
      gb->SP += 2;
      gb->IME = 1;
      num_cycles = 16;
      break;

    case 0xde:
      // SBC A,d8
      {
        unsigned char value;
        gameboy_read_mem(gb, gb->PC, &value, 1);
        gb->PC++;

        unsigned char borrow_1;
        unsigned char half_borrow_1;
        sub_8_bit(gb->A, value, &gb->A, &borrow_1, &half_borrow_1);

        unsigned char borrow_2;
        unsigned char half_borrow_2;
        sub_8_bit(gb->A, CPU_FLAG_C(gb->F), &gb->A, &borrow_2, &half_borrow_2);

        gb->F = CPU_F(
          gb->A == 0 ? 1 : 0,
          1,
          half_borrow_1 || half_borrow_2 ? 1 : 0,
          borrow_1 || borrow_2 ? 1 : 0
        );
        num_cycles = 8;
      }
      break;

    case 0xe0:
      // LDH (a8),A
      {
        unsigned char addr;
        gameboy_read_mem(gb, gb->PC, &addr, 1);
        gb->PC++;
        unsigned short offset_addr = addr + 0xff00;
        gameboy_write_mem(gb, offset_addr, &gb->A, 1);
        num_cycles = 12;
      }
      break;

    case 0xe2:
      // LD (C),A
      gb->PC++;
      gameboy_write_mem(gb, 0xff00 + gb->C, &gb->A, 1);
      num_cycles = 8;
      break;

    case 0xe6:
      // AND d8
      {
        unsigned char value;
        gameboy_read_mem(gb, gb->PC, &value, 1);
        gb->PC++;

        gb->A = gb->A & value;
        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, 1, 0);
        num_cycles = 8;
      }
      break;

    case 0xe8:
      // ADD SP,r8
      {
        char offset;
        gameboy_read_mem(gb, gb->PC, (unsigned char*)&offset, 1);
        gb->PC++;

        unsigned char trash;
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(gb->SP & 0xff, offset, &trash, &carry, &half_carry);
        gb->F = CPU_F(0, 0, half_carry, carry);

        gb->SP += offset;

        num_cycles = 16;
      }
      break;

    case 0xe9:
      // JP (HL)
      gb->PC = CPU_HL(gb);
      num_cycles = 4;
      break;

    case 0xea:
      // LD (a16),A
      {
        unsigned short address;
        gameboy_read_mem(gb, gb->PC, (unsigned char*)&address, 2);
        gb->PC += 2;
        gameboy_write_mem(gb, address, &gb->A, 1);
        num_cycles = 16;
      }
      break;

    case 0xee:
      // XOR d8
      {
        unsigned char value;
        gameboy_read_mem(gb, gb->PC, &value, 1);
        gb->PC++;

        gb->A = gb->A ^ value;
        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, 0, 0);
        num_cycles = 8;
      }
      break;

    case 0xf0:
      // LDH A,(a8)
      {
        unsigned char value;
        gameboy_read_mem(gb, gb->PC, &value, 1);
        gb->PC++;
        gameboy_read_mem(gb, 0xff00 + value, &gb->A, 1);
        num_cycles = 12;
      }
      break;

    case 0xf2:
      // LD A,(C)
      gb->PC++;
      gameboy_read_mem(gb, 0xff00 + gb->C, &gb->A, 1);
      num_cycles = 8;
      break;

    case 0xf3:
      // DI
      if (gb->unset_ime_after_n_instructions == -1)
        gb->unset_ime_after_n_instructions = 1;
      num_cycles = 4;
      break;

    case 0xf6:
      // OR d8
      {
        unsigned char value;
        gameboy_read_mem(gb, gb->PC, &value, 1);
        gb->PC++;

        gb->A = gb->A | value;
        gb->F = CPU_F(gb->A == 0 ? 1 : 0, 0, 0, 0);
        num_cycles = 8;
      }
      break;

    case 0xf8:
      // LD HL,SP+r8
      {
        char offset;
        gameboy_read_mem(gb, gb->PC, (unsigned char*)&offset, 1);

        gb->PC++;

        unsigned char trash;

        // Set 8-bit half-carry/borrow and carry/borrow.
        unsigned char carry;
        unsigned char half_carry;
        add_8_bit(gb->SP & 0xff, offset, &trash, &carry, &half_carry);
        gb->F = CPU_F(0, 0, half_carry, carry);

        *CPU_HL_REF(gb) = gb->SP + offset;

        num_cycles = 12;
      }
      break;

    case 0xf9:
      // LD SP,HL
      gb->SP = CPU_HL(gb);
      num_cycles = 8;
      break;

    case 0xfa:
      // LD A,(a16)
      {
        unsigned short address;
        gameboy_read_mem(gb, gb->PC, (unsigned char*)&address, 2);
        gb->PC += 2;
        gameboy_read_mem(gb, address, (unsigned char*)&gb->A, 1);
        num_cycles = 16;
      }
      break;

    case 0xfb:
      // EI
      if (gb->set_ime_after_n_instructions == -1)
        gb->set_ime_after_n_instructions = 1;
      num_cycles = 4;
      break;

    case 0xfe:
      // CP d8
      {
        unsigned char value;
        gameboy_read_mem(gb, gb->PC, &value, 1);
        gb->PC++;

        unsigned char trash;
        unsigned char borrow;
        unsigned char half_borrow;
        sub_8_bit(gb->A, value, &trash, &borrow, &half_borrow);
        gb->F = CPU_F(trash == 0 ? 1 : 0, 1, half_borrow, borrow);

        num_cycles = 8;
      }
      break;

    default:
      assert(false);
  }

  if (gb->set_ime_after_n_instructions >= 0) {
    gb->set_ime_after_n_instructions--;
    if (gb->set_ime_after_n_instructions < 0) {
      gb->IME = 1;
    }
  }

  if (gb->unset_ime_after_n_instructions >= 0) {
    gb->unset_ime_after_n_instructions--;
    if (gb->unset_ime_after_n_instructions < 0) {
      gb->IME = 0;
    }
  }

  if (DEBUG) {
    gameboy_dump_registers(gb);
    gameboy_dump_stack(gb);
    printf("\n");
  }

  return num_cycles;
}

void gameboy_step_clock(Gameboy* gb) {
  gb->global_simulated_ticks++;

  // Increment timers.

  if (gb->global_simulated_ticks % 256 == 0) {
    // CPU clock / 256 = 16384 Hz.
    gb->memory[REG_DIV]++;
  }

  unsigned char tac = gb->memory[REG_TAC];
  bool timer_enabled = BIT(tac, 2);
  int timer_speed = tac & 0x11;

  if (timer_enabled) {
    int divider = (int[]){ 1024, 16, 64, 256 }[timer_speed];

    if (gb->global_simulated_ticks % divider == 0) {
      if (gb->memory[REG_TIMA] == 255) {
        gb->memory[REG_TIMA] = gb->memory[REG_TMA];
        gb->memory[REG_IF] = SET_BIT(gb->memory[REG_IF], 2);
      } else {
        gb->memory[REG_TIMA]++;
      }
    }
  }

  // Handle interrupts.
  if (gb->IME) {
    unsigned char IE = gb->memory[REG_IE];
    unsigned char IF = gb->memory[REG_IF];
    unsigned char triggered_interrupts = IE & IF;

    for (int bit = 0; bit <= 4; bit++) {
      if (BIT(triggered_interrupts, bit)) {
        // Turn off interrupts.
        gb->IME = false;

        // Flip off IF bit.
        gb->memory[REG_IF] = UNSET_BIT(gb->memory[REG_IF], bit);

        // Push PC onto stack.
        gb->SP -= 2;
        gameboy_write_mem(gb, gb->SP, (unsigned char*)&gb->PC, 2);

        // Go to interrupt handler.
        unsigned short dest_addr = (unsigned short[]){ 0x40, 0x48, 0x50, 0x58, 0x60 }[bit];
        gb->PC = dest_addr;
        break;
      }
    }
  }

  if (gb->ticks_to_next_instruction > 0) {
    gb->ticks_to_next_instruction--;
    return;
  }

  int num_cycles = gameboy_execute_instruction(gb);

  gb->ticks_to_next_instruction = num_cycles - 1;
}

void gameboy_load_rom_from_file(Gameboy* gb, const char* rom_path) {
  printf("Loading ROM from path: %s\n", rom_path);
  FILE* fp = fopen(rom_path, "r");
  assert(fp != NULL);
  fread(gb->memory, 0x8000, 1, fp);
}

int main(int argc, char **argv) {
  if (argc != 2) {
    printf("Usage: ./gameboy rom.gb\n");
    return 1;
  }

  Gameboy gb;
  gameboy_initialize(&gb);

  const char* rom_path = argv[1];
  gameboy_load_rom_from_file(&gb, rom_path);

  while (true) {
    gameboy_step_clock(&gb);
    if (COUNTDOWN > 1)
      COUNTDOWN--;

    if (COUNTDOWN == 1)
      break;
  }

  return 0;
}
