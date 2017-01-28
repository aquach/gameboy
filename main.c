#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

typedef struct {
  unsigned char memory[64 * 1024];

  // Pair ordering are important because you can use these in pairs: AF, BC, DE, HL.
  unsigned char A;
  unsigned char F;
  unsigned char B;
  unsigned char C;
  unsigned char D;
  unsigned char E;
  unsigned char H;
  unsigned char L;

  short SP;
  short PC;
} Cpu;

#define CPU_AF(c) ((unsigned short*)&(c)->A)
#define CPU_BC(c) ((unsigned short*)&(c)->B)
#define CPU_DE(c) ((unsigned short*)&(c)->D)
#define CPU_HL(c) ((unsigned short*)&(c)->H)

void cpu_initialize(Cpu* cpu) {
  // TODO
}

void cpu_read_mem(Cpu* cpu, short address, unsigned char* output, int len) {
  memcpy(output, cpu->memory + address, len);
}

void cpu_write_mem(Cpu* cpu, short address, unsigned char* data, int len) {
  memcpy(cpu->memory + address, data, len);
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
        {
          unsigned short immediate;
          cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&immediate, 2);
          *CPU_BC(cpu) = immediate;
          cpu->PC += 3;
          num_cycles = 12;
        }
        break;

      case 0x02:
        // LD (BC),A
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x03:
        // INC BC
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x04:
        // INC B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x05:
        // DEC B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x06:
        // LD B,d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x07:
        // RLCA
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x08:
        // LD (a16),SP
        cpu->PC += 3;
        num_cycles = 20;
        break;

      case 0x09:
        // ADD HL,BC
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x0a:
        // LD A,(BC)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x0b:
        // DEC BC
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x0c:
        // INC C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x0d:
        // DEC C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x0e:
        // LD C,d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x0f:
        // RRCA
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x10:
        // STOP 0
        cpu->PC += 2;
        num_cycles = 4;
        break;

      case 0x11:
        // LD DE,d16
        {
          unsigned short immediate;
          cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&immediate, 2);
          *CPU_DE(cpu) = immediate;
          cpu->PC += 3;
          num_cycles = 12;
        }
        break;

      case 0x12:
        // LD (DE),A
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x13:
        // INC DE
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x14:
        // INC D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x15:
        // DEC D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x16:
        // LD D,d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x17:
        // RLA
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x18:
        // JR r8
        cpu->PC += 2;
        num_cycles = 12;
        break;

      case 0x19:
        // ADD HL,DE
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x1a:
        // LD A,(DE)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x1b:
        // DEC DE
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x1c:
        // INC E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x1d:
        // DEC E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x1e:
        // LD E,d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x1f:
        // RRA
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x20:
        // JR NZ,r8
        cpu->PC += 2;
        num_cycles = 12/8;
        break;

      case 0x21:
        // LD HL,d16
        {
          unsigned short immediate;
          cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&immediate, 2);
          *CPU_HL(cpu) = immediate;
          cpu->PC += 3;
          num_cycles = 12;
        }
        break;

      case 0x22:
        // LD (HL+),A
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x23:
        // INC HL
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x24:
        // INC H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x25:
        // DEC H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x26:
        // LD H,d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x27:
        // DAA
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x28:
        // JR Z,r8
        cpu->PC += 2;
        num_cycles = 12/8;
        break;

      case 0x29:
        // ADD HL,HL
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x2a:
        // LD A,(HL+)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x2b:
        // DEC HL
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x2c:
        // INC L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x2d:
        // DEC L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x2e:
        // LD L,d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x2f:
        // CPL
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x30:
        // JR NC,r8
        cpu->PC += 2;
        num_cycles = 12/8;
        break;

      case 0x31:
        // LD SP,d16
        {
          unsigned short immediate;
          cpu_read_mem(cpu, cpu->PC + 1, (unsigned char*)&immediate, 2);
          cpu->SP = immediate;
          cpu->PC += 3;
          num_cycles = 12;
        }
        break;

      case 0x32:
        // LD (HL-),A
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x33:
        // INC SP
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x34:
        // INC (HL)
        cpu->PC += 1;
        num_cycles = 12;
        break;

      case 0x35:
        // DEC (HL)
        cpu->PC += 1;
        num_cycles = 12;
        break;

      case 0x36:
        // LD (HL),d8
        cpu->PC += 2;
        num_cycles = 12;
        break;

      case 0x37:
        // SCF
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x38:
        // JR C,r8
        cpu->PC += 2;
        num_cycles = 12/8;
        break;

      case 0x39:
        // ADD HL,SP
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x3a:
        // LD A,(HL-)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x3b:
        // DEC SP
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x3c:
        // INC A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x3d:
        // DEC A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x3e:
        // LD A,d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0x3f:
        // CCF
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x40:
        // LD B,B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x41:
        // LD B,C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x42:
        // LD B,D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x43:
        // LD B,E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x44:
        // LD B,H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x45:
        // LD B,L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x46:
        // LD B,(HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x47:
        // LD B,A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x48:
        // LD C,B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x49:
        // LD C,C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x4a:
        // LD C,D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x4b:
        // LD C,E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x4c:
        // LD C,H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x4d:
        // LD C,L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x4e:
        // LD C,(HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x4f:
        // LD C,A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x50:
        // LD D,B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x51:
        // LD D,C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x52:
        // LD D,D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x53:
        // LD D,E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x54:
        // LD D,H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x55:
        // LD D,L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x56:
        // LD D,(HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x57:
        // LD D,A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x58:
        // LD E,B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x59:
        // LD E,C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x5a:
        // LD E,D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x5b:
        // LD E,E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x5c:
        // LD E,H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x5d:
        // LD E,L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x5e:
        // LD E,(HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x5f:
        // LD E,A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x60:
        // LD H,B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x61:
        // LD H,C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x62:
        // LD H,D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x63:
        // LD H,E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x64:
        // LD H,H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x65:
        // LD H,L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x66:
        // LD H,(HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x67:
        // LD H,A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x68:
        // LD L,B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x69:
        // LD L,C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x6a:
        // LD L,D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x6b:
        // LD L,E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x6c:
        // LD L,H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x6d:
        // LD L,L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x6e:
        // LD L,(HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x6f:
        // LD L,A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x70:
        // LD (HL),B
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x71:
        // LD (HL),C
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x72:
        // LD (HL),D
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x73:
        // LD (HL),E
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x74:
        // LD (HL),H
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x75:
        // LD (HL),L
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x76:
        // HALT
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x77:
        // LD (HL),A
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x78:
        // LD A,B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x79:
        // LD A,C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x7a:
        // LD A,D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x7b:
        // LD A,E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x7c:
        // LD A,H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x7d:
        // LD A,L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x7e:
        // LD A,(HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x7f:
        // LD A,A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x80:
        // ADD A,B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x81:
        // ADD A,C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x82:
        // ADD A,D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x83:
        // ADD A,E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x84:
        // ADD A,H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x85:
        // ADD A,L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x86:
        // ADD A,(HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x87:
        // ADD A,A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x88:
        // ADC A,B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x89:
        // ADC A,C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x8a:
        // ADC A,D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x8b:
        // ADC A,E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x8c:
        // ADC A,H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x8d:
        // ADC A,L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x8e:
        // ADC A,(HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x8f:
        // ADC A,A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x90:
        // SUB B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x91:
        // SUB C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x92:
        // SUB D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x93:
        // SUB E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x94:
        // SUB H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x95:
        // SUB L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x96:
        // SUB (HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x97:
        // SUB A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x98:
        // SBC A,B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x99:
        // SBC A,C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x9a:
        // SBC A,D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x9b:
        // SBC A,E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x9c:
        // SBC A,H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x9d:
        // SBC A,L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0x9e:
        // SBC A,(HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0x9f:
        // SBC A,A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xa0:
        // AND B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xa1:
        // AND C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xa2:
        // AND D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xa3:
        // AND E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xa4:
        // AND H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xa5:
        // AND L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xa6:
        // AND (HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0xa7:
        // AND A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xa8:
        // XOR B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xa9:
        // XOR C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xaa:
        // XOR D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xab:
        // XOR E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xac:
        // XOR H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xad:
        // XOR L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xae:
        // XOR (HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0xaf:
        // XOR A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xb0:
        // OR B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xb1:
        // OR C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xb2:
        // OR D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xb3:
        // OR E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xb4:
        // OR H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xb5:
        // OR L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xb6:
        // OR (HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0xb7:
        // OR A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xb8:
        // CP B
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xb9:
        // CP C
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xba:
        // CP D
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xbb:
        // CP E
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xbc:
        // CP H
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xbd:
        // CP L
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xbe:
        // CP (HL)
        cpu->PC += 1;
        num_cycles = 8;
        break;

      case 0xbf:
        // CP A
        cpu->PC += 1;
        num_cycles = 4;
        break;

      case 0xc0:
        // RET NZ
        cpu->PC += 1;
        num_cycles = 20/8;
        break;

      case 0xc1:
        // POP BC
        cpu->PC += 1;
        num_cycles = 12;
        break;

      case 0xc2:
        // JP NZ,a16
        cpu->PC += 3;
        num_cycles = 16/12;
        break;

      case 0xc3:
        // JP a16
        cpu->PC += 3;
        num_cycles = 16;
        break;

      case 0xc4:
        // CALL NZ,a16
        cpu->PC += 3;
        num_cycles = 24/12;
        break;

      case 0xc5:
        // PUSH BC
        cpu->PC += 1;
        num_cycles = 16;
        break;

      case 0xc6:
        // ADD A,d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0xc7:
        // RST 00H
        cpu->PC += 1;
        num_cycles = 16;
        break;

      case 0xc8:
        // RET Z
        cpu->PC += 1;
        num_cycles = 20/8;
        break;

      case 0xc9:
        // RET
        cpu->PC += 1;
        num_cycles = 16;
        break;

      case 0xca:
        // JP Z,a16
        cpu->PC += 3;
        num_cycles = 16/12;
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

      case 0xcf:
        // RST 08H
        cpu->PC += 1;
        num_cycles = 16;
        break;

      case 0xd0:
        // RET NC
        cpu->PC += 1;
        num_cycles = 20/8;
        break;

      case 0xd1:
        // POP DE
        cpu->PC += 1;
        num_cycles = 12;
        break;

      case 0xd2:
        // JP NC,a16
        cpu->PC += 3;
        num_cycles = 16/12;
        break;

      case 0xd4:
        // CALL NC,a16
        cpu->PC += 3;
        num_cycles = 24/12;
        break;

      case 0xd5:
        // PUSH DE
        cpu->PC += 1;
        num_cycles = 16;
        break;

      case 0xd6:
        // SUB d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0xd7:
        // RST 10H
        cpu->PC += 1;
        num_cycles = 16;
        break;

      case 0xd8:
        // RET C
        cpu->PC += 1;
        num_cycles = 20/8;
        break;

      case 0xd9:
        // RETI
        cpu->PC += 1;
        num_cycles = 16;
        break;

      case 0xda:
        // JP C,a16
        cpu->PC += 3;
        num_cycles = 16/12;
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

      case 0xdf:
        // RST 18H
        cpu->PC += 1;
        num_cycles = 16;
        break;

      case 0xe0:
        // LDH (a8),A
        cpu->PC += 2;
        num_cycles = 12;
        break;

      case 0xe1:
        // POP HL
        cpu->PC += 1;
        num_cycles = 12;
        break;

      case 0xe2:
        // LD (C),A
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0xe5:
        // PUSH HL
        cpu->PC += 1;
        num_cycles = 16;
        break;

      case 0xe6:
        // AND d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0xe7:
        // RST 20H
        cpu->PC += 1;
        num_cycles = 16;
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

      case 0xef:
        // RST 28H
        cpu->PC += 1;
        num_cycles = 16;
        break;

      case 0xf0:
        // LDH A,(a8)
        cpu->PC += 2;
        num_cycles = 12;
        break;

      case 0xf1:
        // POP AF
        cpu->PC += 1;
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

      case 0xf5:
        // PUSH AF
        cpu->PC += 1;
        num_cycles = 16;
        break;

      case 0xf6:
        // OR d8
        cpu->PC += 2;
        num_cycles = 8;
        break;

      case 0xf7:
        // RST 30H
        cpu->PC += 1;
        num_cycles = 16;
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

      case 0xff:
        // RST 38H
        cpu->PC += 1;
        num_cycles = 16;
        break;

      default:
        assert(false);
    }
}

int main(int argc, char **argv) {
  assert(false);
  Cpu cpu;
  cpu_initialize(&cpu);

  return 0;
}
