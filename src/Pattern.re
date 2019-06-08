module Tile = {
  type t = array(array(int));

  let from_bytes = (bytes: bytes, offset: int) => {
    Array.init(8, y =>
      Array.init(
        8,
        x => {
          let low = Bytes.get(bytes, 16 * offset + y) |> Char.code;
          let high = Bytes.get(bytes, 16 * offset + y + 8) |> Char.code;

          (Util.read_bit(low, 7 - x) ? 1 : 0)
          + (Util.read_bit(high, 7 - x) ? 2 : 0);
        },
      )
    );
  };

  let from_codes = (cs): t =>
    from_bytes(Bytes.init(16, i => Char.chr(cs[i])), 0);

  let inspect = (tile: t, format: int => string): string => {
    let result = ref("");

    Array.iter(
      row => {
        Array.iter(n => result := result^ ++ format(n), row);
        result := result^ ++ "\n";
      },
      tile,
    );

    result^;
  };
};

module Table = {
  type t = array(Tile.t);

  let load = (rom: Rom.t): array(Tile.t) => {
    Array.init(256, i => Tile.from_bytes(rom.chr, i));
  };
};