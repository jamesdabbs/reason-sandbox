type memory = {
  ram: bytes,
  rom: Rom.rom,
};

let build = (rom: Rom.rom): memory => {
  {ram: Bytes.make(0x800, Char.chr(0)), rom};
};

let get_byte = (mem: memory, loc: int): int =>
  if (loc >= 0x4020) {
    Char.code(Bytes.get(mem.rom.prg, loc - 0x4020));
  } else {
    let address = loc land 0x7ff;
    Char.code(Bytes.get(mem.ram, address));
  };

let set_byte = (mem: memory, loc: int, value: int) =>
  if (loc <= 0x2000) {
    Bytes.set(mem.ram, loc, Char.chr(value));
  };