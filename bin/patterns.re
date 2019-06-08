let rom_path = Node.Process.argv[2];

let rom = File.rom(rom_path);

{
  open Pattern;

  let table = Table.load(rom);

  Array.iter(tile => Js.log(Tile.inspect(tile)), table);
};