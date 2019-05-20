type mapper =
  | NROM // All 32kB ROM + 8kB VROM games, SMB
  | MMC1 // Final Fantasy, Metroid, Mega Man 2, Zelda
  | UNROM // Castlevania, Contra, Metal Gear, Mega Man
  | CNROM // Cybernoid, Gradius, PipeDream, QBert
  | MMC3 // Double Dragon II, SMB 3, SuperContra
  | Unknown;

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
  mapper_id: int,
  mapper
};

let check_header = (rom: bytes) => {
  let header = Bytes.sub(rom, 0, 4);

  if (Bytes.to_string(header) !== "NES\x1A") {
    failwith("Malformed header");
  };
}

let parse = (path: string): rom => {
  let contents = Bytes.of_string(Node.Fs.readFileSync(path, `binary));
  let byte_at = (bytes, index) => Char.code(Bytes.get(bytes, index));
  check_header(contents);

  let prg_count = byte_at(contents, 4);
  let chr_count = byte_at(contents, 5);

  let prg_size = 0x4000 * prg_count;
  let chr_size = 0x2000 * chr_count;

  let flag6 = byte_at(contents, 6);
  let flag7 = byte_at(contents, 7);
  let mirroring = flag6 land 0x1 == 0 ? Horizontal : Vertical;

  let prg = Bytes.sub(contents, 16, prg_size);
  let chr = Bytes.sub(contents, 16 + prg_size, chr_size);

  let mapper_id = flag6 lsr 4 + (flag7 land 0x10);
  let mapper = switch(mapper_id) {
    | 0 => NROM
    | 1 => MMC1
    | 2 => UNROM
    | 3 => CNROM
    | 4 => MMC3
    | _ => Unknown
    };

  {
    pathname: path,
    prg,
    chr,
    prg_size,
    chr_size,
    prg_count,
    chr_count,
    mirroring,
    mapper_id,
    mapper
  };
};