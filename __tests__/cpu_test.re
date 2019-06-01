open Jest;
open Expect;
open Spec;

describe("CPU", () => {
  let rom = Rom.parse(rom_path("nestest"));
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

  describe("nestest", () => {
    let instrs = Instruction.load(Util.expand_path("src/instructions.json"));
    let disasm = Disassemble.make(instrs, memory);
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
      let path = Util.expand_path("__tests__/roms/nestest.log");
      let log = Node.Fs.readFileSync(path, `utf8);
      let lines = Js.String.split("\n", log);
      let target = "C52D";
      let log_at = line => Js.String2.startsWith(line, target);
      let target_log = Js.Array.find(log_at, lines) |> Util.default("");

      let count = ref(0);
      let cpu_at = target => cpu.pc == int_of_string("0x" ++ target);
      let logs_match = (cpu, line) => Cpu.debug_log(cpu) == lines[line];

      while (!cpu_at(target) && logs_match(cpu, count^)) {
        trace(lines[count^]);
        switch (Cpu.step(cpu)) {
        | Some(Cpu.AddressingModeNotImplemented(mode)) =>
          Js.Console.log(
            "Addressing mode not implemented: "
            ++ AddressingMode.inspect(mode),
          )
        | Some(Cpu.InstructionNotImplemented(instruction)) =>
          Js.Console.log("Instruction not implemented: " ++ instruction)
        | Some(Cpu.OpcodeNotFound(code)) =>
          Js.Console.log("Opcode not found: " ++ string_of_int(code))
        | _ => ()
        };
        count := count^ + 1;
      };

      expect(Cpu.debug_log(cpu)) |> toEqual(target_log);
    });
  });
});