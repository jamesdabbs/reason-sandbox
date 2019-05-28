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

describe("Disassemble", () => {
  let rom = Rom.parse(rom_path("nestest"));
  let memory = Memory.build(rom);
  let instructions =
    Instruction.load(expandPath("../src/instructions.json"));

  let disassemble = Disassemble.make(instructions, memory);

  test("it can run", () => {
    let expected = {j|
8000 4C F5 C5 ;; JMP
8003 60       ;; RTS
8004 78       ;; SEI
8005 D8       ;; CLD
8006 A2 FF    ;; LDX
8008 9A       ;; TXS
8009 AD 02 20 ;; LDA
800C 10 FB    ;; BPL
800E AD 02 20 ;; LDA
8011 10 FB    ;; BPL
|j};
    expect(String.trim(disassemble(0x8000, 10))) == String.trim(expected);
  });
});