type t = {
  memory: Memory.memory,
  mutable cycles: int,
  mutable x: int,
  mutable y: int,
  mutable acc: int,
  mutable stack: int,
  mutable status: int,
  mutable pc: int,
};

type cpu = t;

module InstructionTable =
  Map.Make({
    type t = Opcode.code;
    let compare = compare;
  });

exception AddressingModeNotImplemented(Opcode.addressing_mode);
exception InstructionNotImplemented(string);
exception OpcodeNotFound(int);

let build = memory => {
  {memory, cycles: 0, x: 0, y: 0, acc: 0, stack: 253, status: 36, pc: 0xfffc};
};

let copy = cpu => {
  {
    memory: cpu.memory,
    cycles: cpu.cycles,
    x: cpu.x,
    y: cpu.y,
    acc: cpu.acc,
    stack: cpu.stack,
    status: cpu.status,
    pc: cpu.pc,
  };
};

let reset = cpu => {
  cpu.pc = Memory.get_word(cpu.memory, cpu.pc);
};

let debug_log = cpu => {
  Printf.sprintf(
    "%04X A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%i",
    cpu.pc,
    cpu.acc,
    cpu.x,
    cpu.y,
    cpu.status,
    cpu.stack,
    cpu.cycles,
  );
};

let jump = (cpu, argument) => {
  cpu.pc = argument;
};

let get_argument = (cpu: t, mode: Opcode.addressing_mode) => {
  switch (mode) {
  | Absolute => Memory.get_word(cpu.memory, cpu.pc)
  | _ => raise(AddressingModeNotImplemented(mode))
  };
};

let handle = (definition: Instruction.t, opcode: Opcode.t, cpu: t) => {
  let argument = get_argument(cpu, opcode.addressing_mode);

  let operation =
    switch (definition.label) {
    | "jmp" => jump
    | _ => raise(InstructionNotImplemented(definition.label))
    };

  operation(cpu, argument);

  cpu.cycles = cpu.cycles + opcode.timing;
};

let add_instructions = (definition: Instruction.t) =>
  Array.fold_right(
    opcode =>
      InstructionTable.add(
        (opcode: Opcode.t).code,
        handle(definition, opcode),
      ),
    definition.opcodes,
  );

let table: InstructionTable.t(t => unit) =
  Array.fold_right(
    add_instructions,
    Instruction.load(Util.expand_path("src/instructions.json")),
    InstructionTable.empty,
  );

let step = cpu => {
  let opcode = Memory.get_byte(cpu.memory, cpu.pc);
  cpu.pc = cpu.pc + 1;

  switch (InstructionTable.find(opcode, table)) {
  | command => command(cpu)
  | exception Not_found => raise(OpcodeNotFound(opcode))
  };

  ();
};