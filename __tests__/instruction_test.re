open Jest;
open Expect;
open! Expect.Operators;

module Map = Belt.Map.String;

[@bs.val] external __dirname: string = "";

let expandPath = (path: string) => {
  Node.Path.resolve(__dirname, path);
};

exception NotFound(string);

describe("Instruction", () => {
  open Instruction;

  let instructions: Map.t(Instruction.t) =
    load(expandPath("../src/instructions.json"))
    |> Array.map(instruction => (instruction.label, instruction))
    |> Map.fromArray;

  let instruction = label => {
    switch (Map.get(instructions, label)) {
    | Some(value) => value
    | None => raise(NotFound(label))
    };
  };

  test("loaded instructions", () =>
    expect(Map.size(instructions)) == 56
  );

  describe("adc", () => {
    let adc = instruction("adc");

    test("label", () =>
      expect(adc.label) == "adc"
    );

    test("description", () =>
      expect(adc.description) == "Add with Carry"
    );

    test("opcode length", () =>
      expect(Array.length(adc.opcodes)) == 8
    );
  });

  describe("jmp", () => {
    let jmp = instruction("jmp");

    test("label", () =>
      expect(jmp.label) == "jmp"
    );

    test("description", () =>
      expect(jmp.description) == "Jump Unconditional"
    );

    test("opcodes", () =>
      expect(jmp.opcodes)
      == [|
           {
             code: Char.chr(76),
             length: 3,
             timing: 3,
             addressing_mode: Some(Opcode.Absolute),
           },
           {
             code: Char.chr(108),
             length: 3,
             timing: 5,
             addressing_mode: Some(Opcode.Indirect),
           },
         |]
    );
  });
});