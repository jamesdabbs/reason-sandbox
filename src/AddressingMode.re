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

exception Unrecognized(string);

let decode = (json: Js.Json.t): t => {
  switch (Js.Json.decodeString(json)) {
  | None => Implicit
  | Some(value) =>
    switch (value) {
    | "absolute" => Absolute
    | "absoluteX" => AbsoluteX
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

type result =
  | MemIndex(int)
  | MemRange(int, int);

let get_address = (cpu: Types.cpu, mode: t): result => {
  open Memory;

  switch (mode) {
  | Implicit => MemIndex(0)
  | Accumulator => MemIndex(cpu.acc)
  | Immediate => MemIndex(cpu.pc)
  | ZeroPage => MemIndex(get_byte(cpu.memory, cpu.pc))
  | ZeroPageX => MemIndex((get_byte(cpu.memory, cpu.pc) + cpu.x) land 0xff)
  | ZeroPageY => MemIndex((get_byte(cpu.memory, cpu.pc) + cpu.y) land 0xff)
  | Absolute => MemIndex(get_word(cpu.memory, cpu.pc))
  | AbsoluteX =>
  let start = get_word(cpu.memory, cpu.pc);
  let final = (start + cpu.x) land 0xffff;
  MemRange(start, final);
  | AbsoluteY => 
  let start = get_word(cpu.memory, cpu.pc);
  let final = (start + cpu.y) land 0xffff;
  MemRange(start, final);
  | Indirect =>
    let start = get_word(cpu.memory, cpu.pc);
    MemIndex(get_indirect(cpu.memory, start));
  | IndirectX =>
    let start = get_byte(cpu.memory, cpu.pc) + cpu.x;
    let index = get_indirect(cpu.memory, start land 0xff);
    MemIndex(index);
  | IndirectY =>
    let start = get_indirect(cpu.memory, get_byte(cpu.memory, cpu.pc));
    let final = (start + cpu.y) land 0xffff;
    MemRange(start, final);
  | Relative =>
    let offset = get_byte(cpu.memory, cpu.pc);
    if (Util.read_bit(offset, 7)) {
      MemIndex(cpu.pc - offset lxor 0xff);
    } else {
      MemIndex(cpu.pc + offset + 1);
    };
  };
};