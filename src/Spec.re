let rom_path = (name: string) => {
  Util.expand_path("__tests__/roms/" ++ name ++ ".nes");
};

let rom = (name: string): Rom.t => {
  let path = rom_path(name);
  Node.Fs.readFileSync(path, `binary) |> Bytes.of_string |> Rom.parse(path);
};