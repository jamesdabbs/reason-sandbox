open Jest;
open Expect;
open! Expect.Operators;

describe("Demo", () =>
  describe("double", () =>
    test("5", () =>
      expect(Demo.double(5)) === 10
    )
  )
);