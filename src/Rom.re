type mapper =
  | NROM // All 32kB ROM + 8kB VROM games, SMB
  | MMC1 // Final Fantasy, Metroid, Mega Man 2, Zelda
  | UNROM // Castlevania, Contra, Metal Gear, Mega Man
  | CNROM // Cybernoid, Gradius, PipeDream, QBert
  | MMC3 // Double Dragon II, SMB 3, SuperContra
  | UnknownRom;

type mirroring =
  | Horizontal
  | Vertical;

type rom = {
  pathname: string,
  prg: bytes,
  chr: bytes,
  prg_size: int,
  chr_size: int,
  prg_count: int,
  chr_count: int,
  mirroring,
  mapper,
};

let parse = (path: string): rom => {
  let contents = Bytes.of_string(Node.Fs.readFileSync(path, `binary));

  let header = Bytes.sub(contents, 0, 4);

  if (Bytes.to_string(header) !== "NES\x1A") {
    failwith("Malformed header");
  };

  let prg_count = Char.code(Bytes.get(contents, 4));
  let chr_count = Char.code(Bytes.get(contents, 5));

  let prg_size = 0x4000 * prg_count;
  let chr_size = 0x2000 * chr_count;

  let flag6 = Char.code(Bytes.get(contents, 6));
  let mirroring = flag6 land 0x1 == 0 ? Horizontal : Vertical;

  let prg = Bytes.sub(contents, 16, prg_size);
  let chr = Bytes.sub(contents, 16 + prg_size, chr_size);

  {
    pathname: path,
    prg,
    chr,
    prg_size,
    chr_size,
    prg_count,
    chr_count,
    mirroring,
    mapper: NROM // TODO
  };
};