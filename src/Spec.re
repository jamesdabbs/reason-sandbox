let rom_path = (name: string) => {
  Util.expand_path("__tests__/roms/" ++ name ++ ".nes");
};

let rom = (name: string): Rom.t => {
  rom_path(name) |> File.rom;
};