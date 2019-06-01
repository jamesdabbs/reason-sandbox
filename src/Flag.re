type t =
  | Carry
  | Zero
  | InterruptDisable
  | Decimal
  | Overflow
  | Negative;

let index = (flag: t): int => {
  switch (flag) {
  | Carry => 0
  | Zero => 1
  | InterruptDisable => 2
  | Decimal => 3
  | Overflow => 6
  | Negative => 7
  };
};

module Register = {
  type t = array(bool);

  let build = () => Array.make(8, false);

  let copy = Array.copy;

  let from_int = (value: int): t => {
    Array.init(
      8,
      index => {
        let mask = 1 lsl index;
        value land mask == mask;
      },
    );
  };

  let to_int = (register: t): int => {
    register
    |> Array.mapi((index, flag) => flag ? 1 lsl index : 0)
    |> Array.fold_left((a, b) => a + b, 0);
  };

  let get = (register: t, flag): bool => {
    register[index(flag)];
  };

  let set = (register: t, flag, value: bool): unit => {
    register[index(flag)] = value;
  };
};