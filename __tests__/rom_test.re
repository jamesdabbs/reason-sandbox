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

describe("Demo", () =>
  describe("parse_rom", () => {
    describe("aorom", () => {
      let rom = Rom.parse(rom_path("aorom"));

      test("pathname", () =>
        expect(rom.pathname) === rom_path("aorom")
      );

      test("mapper_id", () => 
        expect(rom.mapper_id) === 10
      );

      test("mapper", () => {
        expect(rom.mapper) === Unknown
      });
    });

    describe("nestest", () => {
      let rom = Rom.parse(rom_path("nestest"));

      test("pathname", () =>
        expect(rom.pathname) === rom_path("nestest")
      );

      test("prg_count", () =>
        expect(rom.prg_count) === 1
      );

      test("chr_count", () =>
        expect(rom.chr_count) === 1
      );

      test("prg_size", () =>
        expect(rom.prg_size) === 0x4000
      );

      test("chr_size", () =>
        expect(rom.chr_size) === 0x2000
      );

      test("mirroring", () =>
        expect(rom.mirroring) === Horizontal
      );

      test("mapper_id", () => 
        expect(rom.mapper_id) === 0
      );

      test("mapper", () =>
        expect(rom.mapper) === NROM
      );
    });
  })
);