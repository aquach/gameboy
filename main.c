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
  gb->memory[REG_NR10] = 0x80;
  gb->memory[REG_NR11] = 0xBF;
  gb->memory[REG_NR12] = 0xF3;
  gb->memory[REG_NR14] = 0xBF;
  gb->memory[REG_NR21] = 0x3F;
  gb->memory[REG_NR24] = 0xBF;
  gb->memory[REG_NR30] = 0x7F;
  gb->memory[REG_NR31] = 0xFF;
  gb->memory[REG_NR32] = 0x9F;
  gb->memory[REG_NR33] = 0xBF;
  gb->memory[REG_NR41] = 0xFF;
  gb->memory[REG_NR30] = 0xBF;
  gb->memory[REG_NR50] = 0x77;
  gb->memory[REG_NR51] = 0xF3;
  gb->memory[REG_NR52] = 0x0F;
  gb->memory[REG_LCDC] = 0x91;
  gb->memory[REG_BGP] = 0xFC;
  gb->memory[REG_OBP0] = 0xFF;
  gb->memory[REG_OBP1] = 0xFF;

  gb->IME = 0;
  gb->set_ime_after_n_instructions = -1;
  gb->unset_ime_after_n_instructions = -1;

  gb->global_simulated_ticks = 0;
  gb->ticks_to_next_instruction = 0;
}

void tile_to_colors(Gameboy* gb, short* tile, int* output, unsigned short palette_reg) {
  for (int l = 0; l < 8; l++) {
    for (int c = 0; c < 8; c++) {
      int color_number = BIT(tile[l], 15 - c) | BIT(tile[l], 7 - c) << 1;
      assert(color_number >= 0);
      assert(color_number <= 3);

      int shade = (gb->memory[palette_reg] >> (color_number * 2)) & 0x3;

      int color;
      switch (shade) {
        case 0:
          color = 0xffffffff;
          break;
        case 1:
          color = 0xffaaaaaa;
          break;
        case 2:
          color = 0xff333333;
          break;
        case 3:
          color = 0xff000000;
          break;
        default:
          assert(false);
      }

      output[l * 8 + c] = color;
    }
  }
}

int compare_priority(const void* a, const void* b) {
  const sprite* sprite_a = (const sprite*)a;
  const sprite* sprite_b = (const sprite*)b;

  // First x, then table order.
  if (sprite_a->x < sprite_b->x) {
    return -1;
  }

  if (sprite_a->x > sprite_b->x) {
    return 1;
  }

  if (sprite_a < sprite_b) {
    return -1;
  }

  if (sprite_a > sprite_b) {
    return 1;
  }

  assert(false);
  return 0;
}

void gameboy_draw_scanline(Gameboy* gb) {
  unsigned int scanline_rgb[GB_SCREEN_WIDTH];

  int LY = gb->memory[REG_LY];

  if (BIT(gb->memory[REG_LCDC], 0)) {
    // Draw background and/or window.

    if (BIT(gb->memory[REG_LCDC], 5)) {
      // Draw window.
    }
  } else {
    // Background and window are white.
    memset(scanline_rgb, 0xffffffff, GB_SCREEN_WIDTH * 4);
  }

  if (BIT(gb->memory[REG_LCDC], 1)) {
    // Draw sprites.

    bool large_sprites = BIT(gb->memory[REG_LCDC], 2);

    sprite sprites[VIDEO_OAM_NUM_SPRITES];
    memcpy(sprites, &gb->memory[0xfe00], VIDEO_OAM_NUM_SPRITES * 4);
    qsort(sprites, VIDEO_OAM_NUM_SPRITES, 4, compare_priority);

    int indices_to_render[10];
    memset(indices_to_render, -1, 10);

    int num_sprites_on_line = 0;

    for (int i = 0; i < VIDEO_OAM_NUM_SPRITES && num_sprites_on_line < 10; i++) {
      sprite* sprite = &sprites[i];

      int bottom_y = sprite->y;
      int top_y = sprite->y + large_sprites ? 16 : 8;

      if (LY >= top_y && LY <= bottom_y) {
        indices_to_render[num_sprites_on_line] = i;
        num_sprites_on_line++;
      }
    }

    // Render indices backwards so that highest priority gets rendered last, so
    // it goes on top.

    for (int i = 9; i >= 0; i++) {
      if (indices_to_render[i] == -1)
        continue;

      sprite* sprite = &sprites[indices_to_render[i]];

      int bottom_y = sprite->y;
      int top_y = sprite->y + large_sprites ? 16 : 8;
      int src_y = VIDEO_OAM_Y_FLIP(sprite->attributes) ? bottom_y - LY : LY - top_y;
      assert(src_y >= 0);
      if (large_sprites)
        assert(src_y <= 15);
      else
        assert(src_y <= 7);

      int tile_no;
      if (large_sprites) {
        if (src_y >= 8) {
          tile_no = sprite->tile | 0x1;
          src_y -= 8;
        } else {
          tile_no = sprite->tile & 0xfe;
        }
      } else {
        tile_no = sprite->tile;
      }

      assert(tile_no <= 255);

      int tile_data[8 * 8];
      tile_to_colors(gb, (short*)&VIDEO_TILE_0(gb, tile_no), tile_data, VIDEO_OAM_PALETTE_NUMBER(sprite->attributes) ? REG_OBP1 : REG_OBP0);

      for (int c = 0; c < 8; c++) {
        int src_index = src_y * 8 + (VIDEO_OAM_X_FLIP(sprite->attributes) ? 7 - c : c);
        assert(src_index >= 0);
        assert(src_index <= 7);

        int dest_x = sprite->x - 8 + c;

        if (dest_x >= 0 && dest_x < GB_SCREEN_WIDTH) {
          scanline_rgb[dest_x] = tile_data[src_index];
        }
      }
    }
  }
}

void gameboy_read_mem(Gameboy* gb, unsigned short address, unsigned char* output, int len) {
  memcpy(output, gb->memory + address, len);
}

void gameboy_write_mem(Gameboy* gb, unsigned short address, unsigned char* data, int len) {
  if (address == REG_SC) {
    printf("%c", gb->memory[REG_SB]);
  } else if (address == REG_DIV) {
    gb->memory[REG_DIV] = 0;
  } else if (address == REG_LY) {
    gb->memory[REG_LY] = 0;
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
    gameboy_draw_scanline(gb);

    gb->memory[REG_LY]++;
    if (gb->memory[REG_LY] >= 153) {
      gb->memory[REG_LY] = 0;
    }

    if (gb->memory[REG_LY] == 144 && BIT(gb->memory[REG_STAT], 4)) {
      // Trigger VBlank.
      gb->memory[REG_IF] = SET_BIT(gb->memory[REG_IF], 0);
    } else if (gb->memory[REG_LY] == gb->memory[REG_LYC] && BIT(gb->memory[REG_STAT], 6)) {
      // Trigger STAT.
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
