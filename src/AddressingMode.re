type t =
  | Absolute
  | AbsoluteX
  | AbsoluteY
  | Accumulator
  | Immediate
  | Indirect
  | IndirectX
  | IndirectY
  | Relative
  | ZeroPage
  | ZeroPageX
  | ZeroPageY
  | Implicit;

exception NotImplemented(t);
exception Unrecognized(string);

let inspect = (mode: t) => {
  switch (mode) {
  | Absolute => "absolute"
  | AbsoluteX => "absoluteX"
  | AbsoluteY => "absoluteY"
  | Accumulator => "accumulator"
  | Immediate => "immediate"
  | Indirect => "indirect"
  | IndirectX => "indirectX"
  | IndirectY => "indirectY"
  | Relative => "relative"
  | ZeroPage => "zeroPage"
  | ZeroPageX => "zeroPageX"
  | ZeroPageY => "zeroPageY"
  | Implicit => "implicit"
  };
};

let decode = (json: Js.Json.t): t => {
  switch (Js.Json.decodeString(json)) {
  | None => Implicit
  | Some(value) =>
    switch (value) {
    | "absolute" => Absolute
    | "absoluteX" => AbsoluteY
    | "absoluteY" => AbsoluteY
    | "accumulator" => Accumulator
    | "immediate" => Immediate
    | "indirect" => Indirect
    | "indirectX" => IndirectX
    | "indirectY" => IndirectY
    | "relative" => Relative
    | "zeroPage" => ZeroPage
    | "zeroPageX" => ZeroPageX
    | "zeroPageY" => ZeroPageY
    | _ => raise(Unrecognized(value))
    }
  };
};

let get_argument = (cpu: Types.cpu, mode: t) => {
  switch (mode) {
  | Implicit => 0
  | Immediate => Memory.get_byte(cpu.memory, cpu.pc)
  | Absolute => Memory.get_word(cpu.memory, cpu.pc)
  | ZeroPage =>
    let addr = Memory.get_byte(cpu.memory, cpu.pc);
    Memory.get_byte(cpu.memory, addr);
  | Relative =>
    let offset = Memory.get_byte(cpu.memory, cpu.pc);
    if (Util.read_bit(offset, 7)) {
      cpu.pc - offset lxor 0xff;
    } else {
      cpu.pc + offset + 1;
    }
  | _ => raise(NotImplemented(mode))
  };
};