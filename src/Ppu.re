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
  mutable write_latch: int
};

type vblank_nmi =
  | NMIEnabled
  | NMIDisabled;

type t = {
  registers: registers,
};

let build = () => {
  {
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
    write_latch: 0
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
