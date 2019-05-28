type t = {
  ram: bytes,
  rom: Rom.rom,
};

type memory = t;

let build = (rom: Rom.rom): memory => {
  {ram: Bytes.make(0x800, Char.chr(0)), rom};
};

let get_byte = (mem: memory, loc: int): int =>
  if (loc >= 0x8000) {
    let end_of_rom = Bytes.length(mem.rom.prg) - 1;
    let address = loc land end_of_rom;
    Char.code(Bytes.get(mem.rom.prg, address));
  } else {
    let address = loc land 0x7ff;
    Char.code(Bytes.get(mem.ram, address));
  };

let set_byte = (mem: memory, loc: int, value: int) =>
  if (loc <= 0x2000) {
    Bytes.set(mem.ram, loc, Char.chr(value));
  };

let get_word = (mem, loc) => {
  let low = get_byte(mem, loc);
  let high = get_byte(mem, loc + 1);
  high lsl 8 + low;
};