open Jest;
open Expect;
open! Expect.Operators;

describe("Demo", () =>
  describe("parse_rom(nestest.nes)", () => {
    let path = "/data/src/reason/sandbox/__tests__/roms/nestest.nes";
    let result = Demo.parse_rom(path);

    test("pathname", () =>
      expect(result.pathname) === path
    );

    test("prg_size", () =>
      expect(result.prg_size) === 1
    );
  })
);