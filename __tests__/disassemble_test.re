open Jest;
open Expect;
open! Expect.Operators;

describe("Disassemble", () => {
  let rom = Spec.rom("nestest");
  let memory = Memory.build(rom);
  let instructions =
    Instruction.load(Util.expand_path("src/instructions.json"));

  let disassemble = Disassemble.make(instructions, memory);

  test("it can run", () => {
    let expected = {j|
C000 4C F5 C5 ;; JMP
C003 60       ;; RTS
C004 78       ;; SEI
C005 D8       ;; CLD
C006 A2 FF    ;; LDX
C008 9A       ;; TXS
C009 AD 02 20 ;; LDA
C00C 10 FB    ;; BPL
C00E AD 02 20 ;; LDA
C011 10 FB    ;; BPL
|j};

    expect(String.trim(disassemble(0xC000, 10))) == String.trim(expected);
  });
});