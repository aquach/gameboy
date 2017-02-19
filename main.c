#include "main.h"

char DEBUG = 0;
int GB_TO_PC_SCALE = 4;

void sdl_assert(int result) {
  if (result != 0) {
    printf("SDL Error: %s\n", SDL_GetError());
    assert(false);
  }
}

void gameboy_initialize_sdl(Gameboy* gb) {
  if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
    printf("SDL_Init error: %s\n", SDL_GetError());
    exit(1);
  }

  SDL_Window* window = SDL_CreateWindow(
      "Gameboy",
      100,
      100,
      GB_SCREEN_WIDTH * GB_TO_PC_SCALE,
      GB_SCREEN_HEIGHT * GB_TO_PC_SCALE,
      SDL_WINDOW_SHOWN
      );
  if (window == NULL) {
    printf("SDL_CreateWindow error: %s\n", SDL_GetError());
    SDL_Quit();
    exit(1);
  }

  SDL_Surface* window_surface = SDL_GetWindowSurface(window);
  SDL_SetSurfaceBlendMode(window_surface, SDL_BLENDMODE_BLEND);

  gb->window = window;
  gb->window_surface = window_surface;
}

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

  gb->IME = false;
  gb->set_ime_after_n_instructions = -1;
  gb->unset_ime_after_n_instructions = -1;

  gb->global_simulated_ticks = 0;
  gb->ticks_to_next_instruction = 0;

  gb->halted = false;
  gb->halt_di_bug = false;

  gameboy_initialize_sdl(gb);
}

char TILE_TO_RGB_DEBUG = 0;

void tile_to_rgb(Gameboy* gb, short* tile, int* output, unsigned short palette_reg) {
  if (TILE_TO_RGB_DEBUG)
    printf("Palette: 0x%02x\n", gb->memory[palette_reg]);

  for (int l = 0; l < 8; l++) {
    if (TILE_TO_RGB_DEBUG)
      printf("Tile row %d: %04x\n", l, tile[l]);
    for (int c = 0; c < 8; c++) {
      char* line = (char*)&tile[l];
      int color_number = BIT(line[0], 7 - c) | BIT(line[1], 7 - c) << 1;
      assert(color_number >= 0);
      assert(color_number <= 3);

      int color;

      if (color_number == 0 && (palette_reg == REG_OBP0 || palette_reg == REG_OBP1)) {
        // Color number 0 is transparent for sprites.
        color = 0x00000000;
      } else {
        int shade = (gb->memory[palette_reg] >> (color_number * 2)) & 0x3;
        switch (shade) {
          case 0:
            color = 0xffffffff;
            break;
          case 1:
            color = 0xffa8a8a8;
            break;
          case 2:
            color = 0xff555555;
            break;
          case 3:
            color = 0xff000000;
            break;
          default:
            assert(false);
        }
      }

      output[l * 8 + c] = color;
      if (TILE_TO_RGB_DEBUG)
        printf("Tile output %d, %d: %08x\n", l, c, color);
    }
  }
}

void render_tile_to_scanline(
  Gameboy* gb,
  int tile_bank_no,
  unsigned char tile_no,
  int tile_y,
  int dest_x,
  bool x_flip,
  int palette_reg,
  unsigned int* scanline_rgba
) {
  int tile_rgb[8 * 8];
  short* tile_data = (short*)(tile_bank_no ? &VIDEO_TILE_1(gb, tile_no) : &VIDEO_TILE_0(gb, tile_no));
  tile_to_rgb(gb, tile_data, tile_rgb, palette_reg);

  for (int c = 0; c < 8; c++) {
    int src_index = tile_y * 8 + (x_flip ? 7 - c : c);
    assert(src_index >= 0);
    assert(src_index < 64);

    int x = dest_x + c;

    if (x >= 0 && x < GB_SCREEN_WIDTH) {
      scanline_rgba[x] = tile_rgb[src_index];
    }
  }
}

int compare_sprite_priority(const void* a, const void* b) {
  const sprite* sprite_a = (const sprite*)a;
  const sprite* sprite_b = (const sprite*)b;

  // First x, then table order.
  if (sprite_a->xpluswidth < sprite_b->xpluswidth) {
    return -1;
  }

  if (sprite_a->xpluswidth > sprite_b->xpluswidth) {
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
  unsigned int scanline_rgba[GB_SCREEN_WIDTH];
  memset(scanline_rgba, 0xff, GB_SCREEN_WIDTH * 4);

  int LY = gb->memory[REG_LY];

  if (BIT(gb->memory[REG_LCDC], 0)) {
    // Draw background.

    int bg_tile_map_no = BIT(gb->memory[REG_LCDC], 3);
    int bg_tile_bank_no = BIT(gb->memory[REG_LCDC], 4);

    int SCX = gb->memory[REG_SCX];
    int SCY = gb->memory[REG_SCY];

    /* printf("bg_tile_map_no: %d, bg_tile_bank_no: %d\n",  bg_tile_map_no, bg_tile_bank_no); */

    // Over-render tiles by 2x to account for wrapping.
    for (int bg_col = 0; bg_col < 64; bg_col++) {
      for (int bg_row = 0; bg_row < 64; bg_row++) {
        int src_y = LY - bg_row * 8 + SCY;

        if (src_y >= 0 && src_y < 8) {
          int tile_no = VIDEO_BG_MAP(gb, bg_tile_map_no, bg_col % 32, bg_row % 32);
          /* printf( */
          /*   "bg_col: %d, bg_row: %d, src_y: %d, LY: %d, SCX: %d, SCY: %d, tile_no: %d\n", */
          /*   bg_col, */
          /*   bg_row, */
          /*   src_y, */
          /*   SCX, */
          /*   SCY, */
          /*   LY, */
          /*   tile_no */
          /* ); */
          render_tile_to_scanline(
              gb,
              bg_tile_bank_no,
              tile_no,
              src_y,
              bg_col * 8 - SCX,
              0,
              REG_BGP,
              scanline_rgba
              );
        }
      }
    }
  }

  if (BIT(gb->memory[REG_LCDC], 5)) {
    // Draw window.

    int window_tile_map_no = BIT(gb->memory[REG_LCDC], 6);
    int window_tile_bank_no = BIT(gb->memory[REG_LCDC], 4);

    int WX = gb->memory[REG_WX];
    int WY = gb->memory[REG_WY];

    for (int window_col = 0; window_col < 32; window_col++) {
      for (int window_row = 0; window_row < 32; window_row++) {
        int src_y = LY - window_row * 8 - WY;

        if (src_y >= 0 && src_y < 8) {
          unsigned char tile_no = VIDEO_BG_MAP(gb, window_tile_map_no, window_col, window_row);

          render_tile_to_scanline(
              gb,
              window_tile_bank_no,
              tile_no,
              src_y,
              window_col * 8 + WX - 7,
              0,
              REG_BGP,
              scanline_rgba
              );
        }
      }
    }
  }

  if (BIT(gb->memory[REG_LCDC], 1)) {
    // Draw sprites.

    bool large_sprites = BIT(gb->memory[REG_LCDC], 2);

    sprite sprites[VIDEO_OAM_NUM_SPRITES];
    memcpy(sprites, &gb->memory[0xfe00], VIDEO_OAM_NUM_SPRITES * 4);
    qsort(sprites, VIDEO_OAM_NUM_SPRITES, 4, compare_sprite_priority);

    unsigned char indices_to_render[10];
    memset(indices_to_render, -1, 10);

    int num_sprites_on_line = 0;

    for (int i = 0; i < VIDEO_OAM_NUM_SPRITES && num_sprites_on_line < 10; i++) {
      sprite* sprite = &sprites[i];

      int bottom_y = sprite->yplusheight - 1;
      int top_y = sprite->yplusheight - (large_sprites ? 16 : 8);

      /* printf( */
      /*   "i: %d, x + width: %d, y + height: %d, flip: %d, LY: %d, top_y: %d, bottom_y: %d\n", */
      /*   i, */
      /*   sprite->xpluswidth, */
      /*   sprite->yplusheight, */
      /*   VIDEO_OAM_Y_FLIP(sprite->attributes), */
      /*   LY, */
      /*   top_y, */
      /*   bottom_y */
      /* ); */

      if (LY >= top_y && LY <= bottom_y) {
        /* printf("Choosing to render %d\n", i); */
        indices_to_render[num_sprites_on_line] = i;
        num_sprites_on_line++;
      }
    }

    // Render indices backwards so that highest priority gets rendered last, so
    // it goes on top.

    for (int i = 9; i >= 0; i--) {
      if (indices_to_render[i] == 255)
        continue;

      sprite* sprite = &sprites[indices_to_render[i]];

      int bottom_y = sprite->yplusheight - 1;
      int top_y = sprite->yplusheight - (large_sprites ? 16 : 8);
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

      /* printf( */
      /*     "rendering: index: %d, sorted_sprite_index: %d, tile_index: %d, x plus width: %d, y plus height: %d, flip: %d, large_sprites: %d, LY: %d\n", */
      /*     i, */
      /*     indices_to_render[i], */
      /*     tile_no, */
      /*     sprite->xpluswidth, */
      /*     sprite->yplusheight, */
      /*     VIDEO_OAM_Y_FLIP(sprite->attributes), */
      /*     large_sprites, */
      /*     LY */
      /*     ); */

      // TODO: OBJ-TO-BG priority.

      render_tile_to_scanline(
        gb,
        1, // DMG only reads from Tile Bank 1.
        tile_no,
        src_y,
        sprite->xpluswidth - 8,
        VIDEO_OAM_X_FLIP(sprite->attributes),
        VIDEO_OAM_PALETTE_NUMBER(sprite->attributes) ? REG_OBP1 : REG_OBP0,
        scanline_rgba
      );
    }
  }

  unsigned int scaled_scanline_rgba[GB_SCREEN_WIDTH * GB_TO_PC_SCALE * GB_TO_PC_SCALE];

  for (int pc_y = 0; pc_y < GB_TO_PC_SCALE; pc_y++) {
    for (int pc_x = 0; pc_x < GB_SCREEN_WIDTH * GB_TO_PC_SCALE; pc_x++) {
      scaled_scanline_rgba[pc_y * GB_SCREEN_WIDTH * GB_TO_PC_SCALE + pc_x] = scanline_rgba[pc_x / GB_TO_PC_SCALE];
    }
  }

  // Blit scanline.
  SDL_Surface* scanline_surface = SDL_CreateRGBSurfaceFrom(
      scaled_scanline_rgba,
      GB_SCREEN_WIDTH * GB_TO_PC_SCALE,
      GB_TO_PC_SCALE,
      32,
      4 * GB_SCREEN_WIDTH * GB_TO_PC_SCALE,
      0x000000ff,
      0x0000ff00,
      0x00ff0000,
      0xff000000
      );
  SDL_SetSurfaceBlendMode(scanline_surface, SDL_BLENDMODE_BLEND);

  SDL_Rect dest_rect = { .x = 0, .w = GB_SCREEN_WIDTH * GB_TO_PC_SCALE, .y = LY * GB_TO_PC_SCALE, .h = GB_TO_PC_SCALE };
  sdl_assert(SDL_BlitSurface(scanline_surface, NULL, gb->window_surface, &dest_rect));

  SDL_FreeSurface(scanline_surface);
}

void gameboy_read_mem(Gameboy* gb, unsigned short address, unsigned char* output, int len) {
  if (address == REG_JOYP) {
    bool use_button_keys = !BIT(gb->memory[REG_JOYP], 5);
    bool use_direction_keys = !BIT(gb->memory[REG_JOYP], 4);

    const unsigned char* state = SDL_GetKeyboardState(NULL);
    bool down_start = (use_direction_keys && state[SDL_SCANCODE_DOWN]) || (use_button_keys && state[SDL_SCANCODE_RETURN]);
    bool up_select = (use_direction_keys && state[SDL_SCANCODE_UP]) || (use_button_keys && state[SDL_SCANCODE_BACKSPACE]);
    bool left_b = (use_direction_keys && state[SDL_SCANCODE_LEFT]) || (use_button_keys && state[SDL_SCANCODE_X]);
    bool right_a = (use_direction_keys && state[SDL_SCANCODE_RIGHT]) || (use_button_keys && state[SDL_SCANCODE_Z]);

    unsigned char reg_value = (!use_button_keys << 5) | (!use_direction_keys << 4) | (!down_start << 3) | (!up_select << 2) | (!left_b << 1) | !right_a;
    memcpy(output, &reg_value, 1);
  } else {
    memcpy(output, gb->memory + address, len);
  }
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

    if (gb->memory[REG_LY] == 144) {// && BIT(gb->memory[REG_STAT], 4)) {
      sdl_assert(SDL_UpdateWindowSurface(gb->window));

      // Trigger VBlank.
      gb->memory[REG_IF] = SET_BIT(gb->memory[REG_IF], 0);
    } else if (gb->memory[REG_LY] == gb->memory[REG_LYC] && BIT(gb->memory[REG_STAT], 6)) {
      // Trigger STAT.
      gb->memory[REG_IF] = SET_BIT(gb->memory[REG_IF], 0);
    }
  }

  // Handle interrupts.
  unsigned char IE = gb->memory[REG_IE];
  unsigned char IF = gb->memory[REG_IF];
  unsigned char triggered_interrupts = IE & IF;

  for (int bit = 0; bit <= 4; bit++) {
    if (BIT(triggered_interrupts, bit)) {
      gb->halted = false;

      if (gb->IME) {
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

void gameboy_keydown(SDL_Keycode code) {
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

  bool quit = false;

  while (!quit) {
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
      switch (event.type) {
        case SDL_QUIT:
          quit = true;
          break;
        case SDL_KEYDOWN:
          gameboy_keydown(((SDL_KeyboardEvent*)&event)->keysym.sym);
          break;
      }
    }

    gameboy_step_clock(&gb);
  }

  return 0;
}
