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

describe("CPU", () => {
  let rom = Rom.parse(rom_path("nestest"));
  let memory = Memory.build(rom);
  let initial = Cpu.build(memory);

  describe("initial state", () => {
    test("stack", () =>
      expect(initial.stack) == 253
    );

    test("status", () =>
      expect(initial.status) == 36
    );

    test("pc", () =>
      expect(initial.pc) == 0xfffc
    );
  });

  describe("reset", () => {
    let cpu = Cpu.copy(initial);
    Cpu.reset(cpu);

    test("it resets the program counter", () =>
      expect(cpu.pc) == 0xc004
    );
  });

  describe("step", () => {
    let cpu = Cpu.copy(initial);
    cpu.pc = 0xc000;

    let _ = Cpu.step(cpu);

    test("updates pc", () =>
      expect(cpu.pc) == 0xc5f5
    );

    test("updates cycles", () =>
      expect(cpu.cycles) == 3
    );
  });
});