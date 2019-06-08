type registers = {
  mutable control: int,
  mutable mask: int,
  mutable status: int,
  mutable oam_address: int,
  mutable oam_data: int,
  mutable ppu_address: int,
  mutable ppu_data: int,
  mutable coarse_x: int,
  mutable coarse_y: int,
  mutable fine_x: int,
  mutable fine_y: int,
  mutable nt_index: int,
  mutable write_latch: bool
};

type vblank_nmi =
  | NMIEnabled
  | NMIDisabled;

type sprite = {
  x: int,
  y: int,
  tile: int,
  attr: int
};

let make_sprite = (~x=0, ~y=0, ~tile=0, ~attr=0, ()) => {
  {
    x,
    y,
    tile,
    attr
  };
};

type t = {
  registers: registers,
  oam: array(sprite),
  name_table: array(int),
  palette_table: array(int),
  pattern_table: Rom.t
};

let build = (rom) => {
  {
    registers: {
      control: 0,
      mask: 0,
      status: 0,
      oam_address: 0,
      oam_data: 0,
      ppu_address: 0,
      ppu_data: 0,
      coarse_x: 0,
      coarse_y: 0,
      fine_x: 0,
      fine_y: 0,
      nt_index: 0,
      write_latch: false
    },
    oam: Array.init(64, (_n) => make_sprite()),
    name_table: Array.make(0x800, 0),
    palette_table: Array.make(0x20, 0),
    pattern_table: rom
  };
};

let ctrl_helper = (n, unset, set) => {
  (regs) => {
    Util.read_bit(regs.control, n) ? set : unset;
  };
};

let mask_helper = (n) => {
  (regs) => Util.read_bit(regs.mask, n);
};

// NOTE: If we reify nt_index as a separate register, we might not wind up using the scroll offset helpers.
let x_scroll_offset = ctrl_helper(0, 0, 256);
let y_scroll_offset = ctrl_helper(1, 0, 240);
let vram_step = ctrl_helper(2, 1, 32);
let sprite_address = ctrl_helper(3, 0, 0x1000);
let background_address = ctrl_helper(4, 0, 0x1000);
let vblank_nmi = ctrl_helper(7, NMIDisabled, NMIEnabled);

let show_background_left = mask_helper(1);
let show_sprites_left = mask_helper(2);
let show_background = mask_helper(3);
let show_sprites = mask_helper(4);

let read_vram = (ppu, address) => {
  if (address < 0x2000) {
    Char.code(Bytes.get(ppu.pattern_table.chr, address));
  } else if (address < 0x3f00) {
    Array.get(ppu.name_table, address land 0x7ff);
  } else {
    Array.get(ppu.palette_table, address land 0x1f);
  };
};

let read_status = (ppu) => {
  let result = ppu.registers.status;
  ppu.registers.status = result land 0x7f;
  result;
};

let read_ppu_data = (ppu) => {
  let { ppu_address as address, ppu_data as buffer } = ppu.registers;
  let result = read_vram(ppu, address);
  ppu.registers.ppu_data = result;
  ppu.registers.ppu_address = address + vram_step(ppu.registers);
  address < 0x3f00 ? buffer : result;
};

let fetch = (ppu: t, address) => {
  switch (address land 7) {
  | 2 => read_status(ppu)
  | 7 => read_ppu_data(ppu)
  | _ => 0
  };
};
