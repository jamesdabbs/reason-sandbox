open Jest;
open Expect;

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
      expect(initial.stack) |> toEqual(253)
    );

    test("status", () =>
      expect(initial.status) |> toEqual(36)
    );

    test("pc", () =>
      expect(initial.pc) |> toEqual(0xfffc)
    );
  });

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
      expect(Cpu.debug_log(cpu)) |> toEqual("C000 A:00 X:00 Y:00 P:24 SP:FD CYC:0")
    );
  });

  describe("nestest", () => {
    let cpu = Cpu.copy(initial);
    cpu.pc = 0xc000;

    test("runs legal opcodes successfully", () => {
      let path = expandPath("./roms/nestest.log");
      let log = Node.Fs.readFileSync(path, `utf8);
      let lines = Js.String.split("\n", log);
      let finished = "C6BD A:AA X:97 Y:4E P:EF SP:F9 CYC:14572";
      let count = ref(0);
      let logs_match = (cpu, line) => Cpu.debug_log(cpu) == lines[line]

      while (cpu.pc != 0xc6bd && logs_match(cpu, count^)) {
        // Js.log(lines[count^])
        Cpu.step(cpu);
        count := count^ + 1;
      };

      expect(Cpu.debug_log(cpu)) |> toEqual(finished);
    });
  });
});