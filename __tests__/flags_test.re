open Jest;
open Expect;

open Flag;

describe("Flags", () => {
  test("it can set a flag", () => {
    let register = Register.build();

    Register.set(register, Zero, true);

    expect(Register.get(register, Zero)) |> toEqual(true);
  });

  describe("from_int", () => {
    let register = Register.from_int(0b100100);

    test("it round-trips", () =>
      expect(Register.to_int(register)) |> toEqual(0b100100)
    );

    test("it set flags", () =>
      expect(Register.get(register, InterruptDisable)) |> toEqual(true)
    );
  });
});