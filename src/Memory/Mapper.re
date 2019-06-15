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

let aref = (mem, address) => Bytes.get(mem, address) |> Char.code;
let set = (mem, address, byte) => Char.chr(byte) |> Bytes.set(mem, address);

let nrom = (rom: Rom.t): t => {
  {
    as _; // Mute "unused variable this warning" - see https://gist.github.com/dteoh/fb90d5730ccb6df079140b6d249d8330
    pub get_prg = address => aref(rom.prg, address land (rom.prg_size - 1));
    pub set_prg = (_, _) => raise(NotAllowed("Cannot write to prg"));
    pub get_chr = address => aref(rom.chr, address);
    pub set_chr = (address, byte) => set(rom.chr, address, byte)
  };
};

let unrom = (rom: Rom.t): t => {
  {
    val prg_bank = ref(0);
    pub get_prg = address => {
      let bank = address < 0xc000 ? prg_bank^ : rom.prg_count - 1;
      aref(rom.prg, bank * 0x4000 + address land 0x3fff);
    };
    pub set_prg = (_, value) => prg_bank := value land 0b111;
    pub get_chr = address => aref(rom.chr, address);
    pub set_chr = (address, byte) => set(rom.chr, address, byte)
  };
};

let cnrom = (rom: Rom.t): t => {
  {
    val chr_bank = ref(0);
    pub get_prg = address => aref(rom.prg, address land (rom.prg_size - 1));
    pub set_prg = (_, value) => chr_bank := value land 0b11;
    pub get_chr = address => aref(rom.chr, this#chr_addr(address));
    pub set_chr = (address, byte) =>
      set(rom.chr, this#chr_addr(address), byte);
    pri chr_addr = offset => chr_bank^ * 0x2000 + offset
  };
};

let for_rom = (rom: Rom.t): t => {
  switch (rom.mapper) {
  | NROM => nrom(rom)
  | UNROM => unrom(rom)
  | CNROM => cnrom(rom)
  | mapper => raise(MapperNotImplemented(mapper))
  };
};