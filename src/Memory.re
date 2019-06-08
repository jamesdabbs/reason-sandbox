type t = {
  ram: bytes,
  rom: Rom.t,
};

let build = (rom: Rom.t): t => {
  {ram: Bytes.make(0x800, Char.chr(0)), rom};
};

let copy = (memory: t): t => {
  {...memory, ram: Bytes.copy(memory.ram)};
};

let get_byte = (mem: t, loc: int): int =>
  if (loc >= 0x8000) {
    let end_of_rom = Bytes.length(mem.rom.prg) - 1;
    let address = loc land end_of_rom;
    Char.code(Bytes.get(mem.rom.prg, address));
  } else {
    let address = loc land 0x7ff;
    Char.code(Bytes.get(mem.ram, address));
  };

let set_byte = (mem: t, loc: int, value: int) =>
  if (loc <= 0x2000) {
    Bytes.set(mem.ram, loc, Char.chr(value));
  };

let get_word = (mem: t, loc: int) => {
  let low = get_byte(mem, loc);
  let high = get_byte(mem, loc + 1);
  high lsl 8 + low;
};

let get_indirect = (mem: t, loc: int) => {
  // If an indirect fetch would wrap to the next page,
  // it instead wraps to the beginning of the current page.
  let wrapped = loc land 0xff00 + (loc + 1) land 0xff;
  let low = get_byte(mem, loc);
  let high = get_byte(mem, wrapped);
  high lsl 8 + low;
}