module Opcodes =
  Map.Make({
    type t = Opcode.code;
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
  let hcode = to_hex(opcode.code);
  let hargs =
    Array.fold_left((acc, arg) => acc ++ " " ++ to_hex(arg), "", args)
    |> String.trim
    |> Util.ljust(5);
  let fargs = AddressingMode.format_args(opcode.addressing_mode, args);
  let label = String.uppercase(instruction.label);
  let base = {j|$hstart $hcode $hargs ;; $label|j};
  fargs == "" ? base ++ "\n" : base ++ " " ++ fargs ++ "\n";
};

let make = (memory: Memory.t) => {
  let opcodes = Array.fold_left(add_opcodes, Opcodes.empty, Instruction.all);

  let rec run = (start: int, length: int) =>
    if (length == 0) {
      "";
    } else {
      let code = Memory.get_byte(memory, start);
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