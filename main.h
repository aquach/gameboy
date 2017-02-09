#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

extern char DEBUG;
extern unsigned long long COUNTDOWN;

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
#define REG_LY (0xFF44)
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

int gameboy_execute_instruction(Gameboy* gb);

void gameboy_read_mem(Gameboy* gb, unsigned short address, unsigned char* output, int len);
void gameboy_write_mem(Gameboy* gb, unsigned short address, unsigned char* data, int len);
