let rom_path = Node.Process.argv[2];

let nes = File.rom(rom_path) |> Nes.load;

Nes.step_to(nes, ~halt=() => false);