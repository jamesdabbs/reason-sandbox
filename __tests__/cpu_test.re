open Jest;
open Expect;
open Spec;

exception LinesDoNotMatch(string, string);

describe("CPU", () => {
  let rom = rom("nestest");
  let memory = Memory.build(rom);
  let initial = Cpu.build(memory);

  describe("initial state", () => {
    test("stack", () =>
      expect(initial.stack) |> toEqual(253)
    );

    test("status", () =>
      expect(Flag.Register.to_int(initial.status)) |> toEqual(0b100100)
    );

    test("pc", () =>
      expect(initial.pc) |> toEqual(0xfffc)
    );
  });

  describe("copy", () =>
    test("it makes a shallow copy", () => {
      let a = Cpu.build(Memory.build(rom));
      let b = Cpu.copy(a);

      b.x = b.x + 1;

      expect(a.x) |> not_ |> toEqual(b.x);
    })
  );
  describe("reset", () => {
    let cpu = Cpu.copy(initial);
    Cpu.reset(cpu);

    test("it resets the program counter", () =>
      expect(cpu.pc) |> toEqual(0xc004)
    );
  });

  describe("step", () => {
    let cpu = Cpu.copy(initial);
    cpu.pc = 0xc000;

    let _ = Cpu.step(cpu);

    test("updates pc", () =>
      expect(cpu.pc) |> toEqual(0xc5f5)
    );

    test("updates cycles", () =>
      expect(cpu.cycles) |> toEqual(3)
    );
  });

  describe("debug_log", () => {
    let cpu = Cpu.copy(initial);
    cpu.pc = 0xc000;

    test("returns nestest compatible cpu state", () =>
      expect(Cpu.debug_log(cpu))
      |> toEqual("C000 A:00 X:00 Y:00 P:24 SP:FD CYC:0")
    );
  });

  describe("now", () => {
    let cpu = Cpu.copy(initial);
    cpu.pc = 0xc000;
    let actual = cpu |> Cpu.now |> String.trim;

    test("it disassembles the current instruction", () => {
      expect(actual) |> toEqual("C000 4C F5 C5 ;; JMP $C5F5");
    });
  });

  describe("nestest", () => {
    let disasm = Disassemble.make(memory);
    let cpu = Cpu.copy(initial);
    cpu.pc = 0xc000;

    let trace = line => {
      let dis = disasm(cpu.pc, 1);
      let actual = Cpu.debug_log(cpu);
      Js.log(
        {j|
        Expected: $line
        Actual:   $actual
        Disassembly: $dis
      |j},
      );
    };

    test("runs legal opcodes successfully", () => {
      let target = "C6BD";
      let tracing = false;

      let path = Util.expand_path("__tests__/roms/nestest.log");
      let lines = Node.Fs.readFileSync(path, `utf8) |> Js.String.split("\n");

      let rec run = index => {
        let line = lines[index];

        let previous_pc = cpu.pc;
        let before = Cpu.debug_log(cpu);
        Cpu.step(cpu);
        let after = Cpu.debug_log(cpu);

        if (after != line) {
          let dis = disasm(previous_pc, 1);
          Js.log(
            {j|
            Previous:    $before
            Expected:    $line
            Actual:      $after
            Disassembly: $dis
          |j},
          );
          raise(LinesDoNotMatch(after, line));
        } else if (cpu.pc != int_of_string("0x" ++ target)) {
          if (tracing) {
            trace(after);
          };
          run(index + 1);
        };
      };

      expect(() =>
        run(1)
      ) |> not_ |> toThrow;
    });
  });
});