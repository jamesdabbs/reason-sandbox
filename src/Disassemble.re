module Opcodes =
  Map.Make({
    type t = char;
    let compare = compare;
  });

let add_opcodes = (lookup, instruction: Instruction.t) =>
  Array.fold_left(
    (acc, opcode: Opcode.t) =>
      Opcodes.add(opcode.code, (instruction, opcode), acc),
    lookup,
    instruction.opcodes,
  );

let to_hex = (n: int) => Printf.sprintf("%02X", n);

let format =
    (
      start: int,
      opcode: Opcode.t,
      instruction: Instruction.t,
      args: array(int),
    )
    : string => {
  let hstart = to_hex(start);
  let hcode = to_hex(Char.code(opcode.code));
  let hargs =
    Array.fold_left((acc, arg) => acc ++ " " ++ to_hex(arg), "", args)
    |> String.trim
    |> Util.ljust(5);
  let label = String.uppercase(instruction.label);

  {j|$hstart $hcode $hargs ;; $label\n|j};
};

let make = (instructions: array(Instruction.t), memory: Memory.t) => {
  let opcodes = Array.fold_left(add_opcodes, Opcodes.empty, instructions);

  let rec run = (start: int, length: int) =>
    if (length == 0) {
      "";
    } else {
      let code = Memory.get_byte(memory, start) |> Char.chr;
      let (instruction, opcode) = Opcodes.find(code, opcodes);
      let args =
        Array.init(opcode.length - 1, n =>
          Memory.get_byte(memory, start + n + 1)
        );

      format(start, opcode, instruction, args)
      ++ run(start + opcode.length, length - 1);
    };

  run;
};