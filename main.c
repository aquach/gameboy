#include "main.h"

char DEBUG = 0;
unsigned long long COUNTDOWN = 200000000;

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
  gb->memory[REG_LY] = 0x00;
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

  if (gb->global_simulated_ticks % 456 == 0) {
    // Scanline finished.
    gb->memory[REG_LY]++;
    if (gb->memory[REG_LY] >= 153) {
      gb->memory[REG_LY] = 0;
    }

    if (gb->memory[REG_LY] == 144) {
      // Trigger VBlank.
      gb->memory[REG_IF] = SET_BIT(gb->memory[REG_IF], 0);
    } else if (gb->memory[REG_LY] == gb->memory[REG_LYC]) {
      // Trigger stat.
      gb->memory[REG_IF] = SET_BIT(gb->memory[REG_IF], 0);
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
