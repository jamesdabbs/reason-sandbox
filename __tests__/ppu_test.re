open Jest;
open Expect;
open Spec;

let nestest = rom("nestest");

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
  let ppu = Ppu.build(nestest);
  let regs = ppu.registers;

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

  describe("fetch", () => {
    let bad_addresses = [0x2000, 0x2001, 0x2003, 0x2005, 0x2006];

    testAll("fetching from write only registers", bad_addresses, (address) => {
      expect(Ppu.fetch(ppu, address)) |> toEqual(0);
    });

    test("fetching status", () => {
      regs.status = 131;
      let return_value = Ppu.fetch(ppu, 0x2002);
      // After reading PPUSTATUS, the top bit of status (vblank) is cleared.
      expect((return_value, regs.status)) |> toEqual((131, 3));
    });

    test("fetching from PPUDATA returns a buffered value", () => {
      regs.ppu_address = 0x2020;
      regs.ppu_data = 0;
      Array.set(ppu.name_table, 0x20, 42);
      let return_value = Ppu.fetch(ppu, 0x2007);
      expect((return_value, regs.ppu_data)) |> toEqual((0, 42));
    });

    test("fetching palette info through PPUDATA is unbuffered", () => {
      regs.ppu_address = 0x3f01;
      Array.set(ppu.palette_table, 1, 33);
      regs.ppu_data = 0;
      let return_value = Ppu.fetch(ppu, 0x2007);
      expect((return_value, regs.ppu_data)) |> toEqual((33, 33));
    });

    test("fetching from PPUDATA increments the ppu_address by the vram_step", () => {
      regs.ppu_address = 0x2010;
      let _ = Ppu.fetch(ppu, 0x2007);
      let small_step = regs.ppu_address;
      regs.control = 132;
      let _ = Ppu.fetch(ppu, 0x2007);
      let big_step = regs.ppu_address;
      expect((small_step, big_step)) |> toEqual((0x2011, 0x2031));
    });

  });
});