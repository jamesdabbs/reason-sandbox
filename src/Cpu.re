open Types;

type t = cpu;

module InstructionTable =
  Map.Make({
    type t = Opcode.code;
    let compare = compare;
  });

exception AddressingModeNotImplemented(AddressingMode.t);
exception InstructionNotImplemented(string);
exception OpcodeNotFound(int);

let build = memory => {
  {
    memory,
    cycles: 0,
    x: 0,
    y: 0,
    acc: 0,
    stack: 253,
    status: Flag.Register.from_int(0b100100),
    pc: 0xfffc,
  };
};

let copy = cpu => {
  ...cpu,
  memory: Memory.copy(cpu.memory),
  status: Flag.Register.copy(cpu.status),
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
    Flag.Register.to_int(cpu.status),
    cpu.stack,
    cpu.cycles,
  );
};

let set_flag = (cpu: t) => Flag.Register.set(cpu.status);

let set_flags_zn = (cpu: t, value: int) => {
  set_flag(cpu, Flag.Zero, value == 0);
  set_flag(cpu, Flag.Negative, Util.read_bit(value, 7));
};

let stack_push = (cpu: t, value: int) => {
  Memory.set_byte(cpu.memory, 0x100 + cpu.stack, value);
  cpu.stack = cpu.stack - 1;
}

let branch_on_flag = (flag, expected, cpu, argument) => {
  if (Flag.Register.get(cpu.status, flag) == expected) {
    cpu.cycles = cpu.cycles + 1;
    cpu.pc = argument;
  } else {
    cpu.pc = cpu.pc + 1;
  }

}

let clear_carry = (cpu, _argument) => {
  set_flag(cpu, Flag.Carry, false);
}

let jump = (cpu, argument) => {
  cpu.pc = argument;
};

let jump_subroutine = (cpu, argument) => {
  stack_push(cpu, cpu.pc lsr 8);
  stack_push(cpu, cpu.pc land 0xff);

  cpu.pc = argument;
};

let load_acc = (cpu, argument) => {
  cpu.acc = argument;

  set_flags_zn(cpu, cpu.acc);
};

let load_x = (cpu, argument) => {
  cpu.x = argument;

  set_flags_zn(cpu, cpu.x);
};

let nop = (_cpu, _argument) => {
  ();
}

let set_carry = (cpu, _argument) => {
  set_flag(cpu, Flag.Carry, true);
}

let store_acc = (cpu, argument) => {
  Memory.set_byte(cpu.memory, argument, cpu.acc);
}

let store_x = (cpu, argument) => {
  Memory.set_byte(cpu.memory, argument, cpu.x);
};

let step_size = (definition: Instruction.t, opcode: Opcode.t) => {
  switch (definition.access_pattern) {
  | Jump => 0
  | _ => opcode.length - 1
  };
};

let handle = (definition: Instruction.t, opcode: Opcode.t, cpu: t) => {
  let argument = AddressingMode.get_argument(cpu, opcode.addressing_mode);

  let operation =
    switch (definition.label) {
    | "bcc" => branch_on_flag(Flag.Carry, false)
    | "bcs" => branch_on_flag(Flag.Carry, true)
    | "beq" => branch_on_flag(Flag.Zero, true)
    | "bne" => branch_on_flag(Flag.Zero, false)
    | "clc" => clear_carry
    | "jmp" => jump
    | "jsr" => jump_subroutine
    | "lda" => load_acc
    | "ldx" => load_x
    | "nop" => nop
    | "sec" => set_carry
    | "sta" => store_acc
    | "stx" => store_x
    | _ => raise(InstructionNotImplemented(definition.label))
    };

  operation(cpu, argument);

  cpu.cycles = cpu.cycles + opcode.timing;
  cpu.pc = cpu.pc + step_size(definition, opcode);
};

let add_instruction = (definition: Instruction.t) =>
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
    add_instruction,
    Instruction.load(Util.expand_path("src/instructions.json")),
    InstructionTable.empty,
  );

type error =
  | AddressingModeNotImplemented(AddressingMode.t)
  | InstructionNotImplemented(string)
  | OpcodeNotFound(int);

let step = (cpu: t): option(error) => {
  let opcode = Memory.get_byte(cpu.memory, cpu.pc);
  cpu.pc = cpu.pc + 1;

  switch (InstructionTable.find(opcode, table)) {
  | command =>
    switch (command(cpu)) {
    | exception (AddressingMode.NotImplemented(mode)) =>
      Some(AddressingModeNotImplemented(mode))
    | exception (InstructionNotImplemented(ins)) =>
      Some(InstructionNotImplemented(ins))
    | _ => None
    }
  | exception Not_found => Some(OpcodeNotFound(opcode))
  };
};