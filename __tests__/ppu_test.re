open Jest;
open Expect;

let ctrl_test_helper = (regs: Ppu.registers, n, reader, unset, set) => {
  test("checking CTRL " ++ string_of_int(n) ++ " bit set", () => {
    regs.control = 0;
    expect(reader(regs)) |> toEqual(unset);
  });
  test("checking CTRL " ++ string_of_int(n) ++ " bit set", () => {
    regs.control = 1 lsl n;
    expect(reader(regs)) |> toEqual(set);
  });
};

let mask_test_helper = (regs: Ppu.registers, n, reader) => {
  test("checking MASK " ++ string_of_int(n) ++ " bit set", () => {
    regs.mask = 0;
    expect(reader(regs)) |> toEqual(false);
  });
  test("checking MASK " ++ string_of_int(n) ++ " bit set", () => {
    regs.mask = 1 lsl n;
    expect(reader(regs)) |> toEqual(true);
  });
};

describe("PPU", () => {
  let regs = Ppu.build();

  describe("PPUCTRL", () => {
    ctrl_test_helper(regs, 0, Ppu.x_scroll_offset, 0, 256);
    ctrl_test_helper(regs, 1, Ppu.y_scroll_offset, 0, 240);
    ctrl_test_helper(regs, 2, Ppu.vram_step, 1, 32);
    ctrl_test_helper(regs, 3, Ppu.sprite_address, 0, 0x1000);
    ctrl_test_helper(regs, 4, Ppu.background_address, 0, 0x1000);
    ctrl_test_helper(regs, 7, Ppu.vblank_nmi, Ppu.NMIDisabled, Ppu.NMIEnabled);
  });

  describe("PPUMASK", () => {
    mask_test_helper(regs, 1, Ppu.show_background_left);
    mask_test_helper(regs, 2, Ppu.show_sprites_left);
    mask_test_helper(regs, 3, Ppu.show_background);
    mask_test_helper(regs, 4, Ppu.show_sprites);
  });
});