let double = x => x * 2;

type mapper =
  | NROM // All 32kB ROM + 8kB VROM games, SMB
  | MMC1 // Final Fantasy, Metroid, Mega Man 2, Zelda
  | UNROM // Castlevania, Contra, Metal Gear, Mega Man
  | CNROM // Cybernoid, Gradius, PipeDream, QBert
  | MMC3; // Double Dragon II, SMB 3, SuperContra

type mirroring =
  | Horizontal
  | Vertical;

type rom = {
  pathname: string,
  prg: Buffer.t,
  chr: Buffer.t,
  prg_size: int,
  chr_size: int,
  prg_count: int,
  chr_count: int,
  mirroring,
  mapper,
};

let to_hex = (string: string) => {
  Array.init(String.length(string), index =>
    Printf.sprintf("%X", Char.code(string.[index]))
  )
  |> Js.Array.joinWith(" ");
};

let parse_rom = (path: string): rom => {
  let contents = Node.Fs.readFileSync(path, `binary);

  let header = String.sub(contents, 0, 4);

  if (to_hex(header) !== "4E 45 53 1A") {
    failwith("Malformed header");
  };

  let prg_size = Char.code(contents.[5]);

  {
    pathname: path,
    prg: Buffer.create(0),
    chr: Buffer.create(0),
    prg_size,
    chr_size: 0,
    prg_count: 0,
    chr_count: 0,
    mirroring: Horizontal,
    mapper: NROM,
  };
};