type address = int;
type byte = int;

type t = {
  .
  get_prg: address => byte,
  set_prg: (address, byte) => unit,
  get_chr: address => byte,
  set_chr: (address, byte) => unit,
};

exception MapperNotImplemented(Rom.mapper);
exception NotAllowed(string);

let nrom = (rom: Rom.t): t => {
  let end_of_rom = rom.prg_size - 1;
  {
    pub get_prg = address => {
      Bytes.get(rom.prg, address land end_of_rom) |> Char.code;
    };
    pub set_prg = (_, _) => raise(NotAllowed("Cannot write to prg"));
    pub get_chr = address => {
      Bytes.get(rom.chr, address) |> Char.code;
    };
    pub set_chr = (address, byte) => {
      Char.chr(byte) |> Bytes.set(rom.chr, address);
    }
  };
};

let for_rom = (rom: Rom.t): t => {
  switch (rom.mapper) {
  | NROM => nrom(rom)
  | mapper => raise(MapperNotImplemented(mapper))
  };
};