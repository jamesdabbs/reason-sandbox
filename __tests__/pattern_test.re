open Jest;
open Expect;

describe("Pattern", () => {
  let format = n => n == 0 ? "." : string_of_int(n);

  test("it can be built directly", () => {
    let tile =
      Pattern.Tile.from_codes([|
        0x41,
        0xC2,
        0x44,
        0x48,
        0x10,
        0x20,
        0x40,
        0x80,
        0x01,
        0x02,
        0x04,
        0x08,
        0x16,
        0x21,
        0x42,
        0x87,
      |]);

    expect(String.trim(Pattern.Tile.inspect(tile, format)))
    |> toEqual(
         String.trim(
           "
.1.....3
11....3.
.1...3..
.1..3...
...3.22.
..3....2
.3....2.
3....222
",
         ),
       );
  });
});