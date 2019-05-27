open Jest;
open Expect;
open! Expect.Operators;

[@bs.val] external __dirname: string = "";

let expandPath = (path: string) => {
  Node.Path.resolve(__dirname, path);
};

let rom_path = (name: string) => {
  expandPath("./roms/" ++ name ++ ".nes");
};

describe("Memory", () => {
  let rom = Rom.parse(rom_path("nestest"));
  let memory = Memory.build(rom);

  test("it can set a value in RAM", () => {
    Memory.set_byte(memory, 3, 42);

    expect(Memory.get_byte(memory, 3)) == 42;
  });

  test("it can read from mirrored locations", () => {
    Memory.set_byte(memory, 3, 42);

    expect(Memory.get_byte(memory, 0x1803)) == 42;
  });

  test("it can read from the cartridge", () =>
    expect(Memory.get_byte(memory, 0x8000))
    == Char.code(Bytes.get(rom.prg, 0))
  );

  test("it cannot set to the cartridge", () => {
    let old = Bytes.copy(memory.ram);

    Memory.set_byte(memory, 0x8000, 13);

    expect(memory.ram) == old;
  });

  test("it can fetch a word", () => {
    let word = Memory.get_word(memory, 0xfffc);

    expect(word) == 0xc004;
  });
});