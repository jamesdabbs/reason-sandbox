open Jest;
open Expect;
open! Expect.Operators;

describe("Memory", () => {
  let rom = Spec.rom("nestest");
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

  test("it can fetch a word", () => {
    let word = Memory.get_word(memory, 0xfffc);

    expect(word) == 0xc004;
  });

  test("it can fetch a word with the page wraparound quirk", () => {
    let word = Memory.get_indirect(memory, 0xffff);

    expect(word) == 197;
  });
});