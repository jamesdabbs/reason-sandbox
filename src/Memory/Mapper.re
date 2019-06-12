type address = int;
type byte = int;

type t = {
  .
  get: address => byte,
  set: (address, byte) => unit,
};

exception MapperNotImplemented(Rom.mapper);

let nrom = (rom: Rom.t): t => {
  let end_of_rom = rom.prg_size - 1;
  {
    pub get = address => {
      Bytes.get(rom.prg, address land end_of_rom) |> Char.code;
    };
    pub set = (_, _) => ()
  };
};

let for_rom = (rom: Rom.t): t => {
  switch (rom.mapper) {
  | NROM => nrom(rom)
  | mapper => raise(MapperNotImplemented(mapper))
  };
};