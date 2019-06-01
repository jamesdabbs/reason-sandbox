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

let check_overflow = (result, acc, arg)  => {
  let result_sign = Util.read_bit(result, 7);
  let acc_sign = Util.read_bit(acc, 7);
  let arg_sign = Util.read_bit(arg, 7);
  !(result_sign == acc_sign || result_sign == arg_sign)
}

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

let set_flag = (flag, value, cpu: t, _argument) => {
  Flag.Register.set(cpu.status, flag, value);
};

let set_flags_zn = (cpu: t, value: int) => {
  Flag.Register.set(cpu.status, Flag.Zero, value == 0);
  Flag.Register.set(cpu.status, Flag.Negative, Util.read_bit(value, 7));
};

let stack_pop = (cpu: t) => {
  cpu.stack = cpu.stack + 1;
  Memory.get_byte(cpu.memory, 0x100 + cpu.stack);
};

let stack_push = (cpu: t, value: int) => {
  Memory.set_byte(cpu.memory, 0x100 + cpu.stack, value);
  cpu.stack = cpu.stack - 1;
};

let add_with_carry = (cpu, argument) => {
  let carry_bit = Flag.Register.to_int(cpu.status) land 1;
  let result = cpu.acc + argument + carry_bit;
  Flag.Register.set(cpu.status, Flag.Overflow, check_overflow(result, cpu.acc, argument));
  Flag.Register.set(cpu.status, Flag.Carry, result > 0xff);
  cpu.acc = result land 0xff;

  set_flags_zn(cpu, cpu.acc);
}

let and_with_acc = (cpu, argument) => {
  cpu.acc = cpu.acc land argument;

  set_flags_zn(cpu, cpu.acc);
};

let branch_on_flag = (flag, expected, cpu, argument) =>
  if (Flag.Register.get(cpu.status, flag) == expected) {
    cpu.cycles = cpu.cycles + 1;
    cpu.pc = argument;
  } else {
    cpu.pc = cpu.pc + 1;
  };

let compare = (cpu, argument) => {
  set_flags_zn(cpu, cpu.acc - argument);
  Flag.Register.set(cpu.status, Flag.Carry, cpu.acc >= argument);
};

let jump = (cpu, argument) => {
  cpu.pc = argument;
};

let jump_subroutine = (cpu, argument) => {
  let target = cpu.pc + 2;

  stack_push(cpu, target lsr 8);
  stack_push(cpu, target land 0xff);

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
};

let or_with_acc = (cpu, argument) => {
  cpu.acc = cpu.acc lor argument;
  set_flags_zn(cpu, cpu.acc);
}

let pop_acc = (cpu, _argument) => {
  cpu.acc = stack_pop(cpu);

  set_flags_zn(cpu, cpu.acc);
};

let pop_status = (cpu, _argument) => {
  // See https://wiki.nesdev.com/w/index.php/Status_flags#The_B_flag
  cpu.status = Flag.Register.from_int(stack_pop(cpu) lor 0x20 land 0xef);
}

let push_acc = (cpu, _argument) => {
  stack_push(cpu, cpu.acc);
}

let push_status = (cpu, _argument) => {
  // See https://wiki.nesdev.com/w/index.php/Status_flags#The_B_flag
  stack_push(cpu, Flag.Register.to_int(cpu.status) lor 0x10);
};

let return_from_subroutine = (cpu, _argument) => {
  let low = stack_pop(cpu);
  let high = stack_pop(cpu);

  cpu.pc = high lsl 8 + low;
};

let store_acc = (cpu, argument) => {
  Memory.set_byte(cpu.memory, argument, cpu.acc);
};

let store_x = (cpu, argument) => {
  Memory.set_byte(cpu.memory, argument, cpu.x);
};

let test_bits = (cpu, argument) => {
  Flag.Register.set(cpu.status, Flag.Negative, Util.read_bit(argument, 7));
  Flag.Register.set(cpu.status, Flag.Overflow, Util.read_bit(argument, 6));
  Flag.Register.set(cpu.status, Flag.Zero, argument land cpu.acc == 0);
};

let xor_with_acc = (cpu, argument) => {
  cpu.acc = cpu.acc lxor argument;
  set_flags_zn(cpu, cpu.acc);
}

let step_size = (definition: Instruction.t, opcode: Opcode.t) => {
  switch (definition.access_pattern) {
  | Jump => 0
  | _ => opcode.length - 1
  };
};

let handle = (definition: Instruction.t, opcode: Opcode.t, cpu: t) => {
  let address = AddressingMode.get_address(cpu, opcode.addressing_mode);

  let operand =
    switch (definition.access_pattern) {
    | Instruction.Read => Memory.get_byte(cpu.memory, address)
    | Instruction.Static => 0
    | _ => address
    };

  let operation =
    switch (definition.label) {
    | "adc" => add_with_carry
    | "and" => and_with_acc
    | "bcc" => branch_on_flag(Flag.Carry, false)
    | "bcs" => branch_on_flag(Flag.Carry, true)
    | "beq" => branch_on_flag(Flag.Zero, true)
    | "bit" => test_bits
    | "bmi" => branch_on_flag(Flag.Negative, true)
    | "bne" => branch_on_flag(Flag.Zero, false)
    | "bpl" => branch_on_flag(Flag.Negative, false)
    | "bvc" => branch_on_flag(Flag.Overflow, false)
    | "bvs" => branch_on_flag(Flag.Overflow, true)
    | "clc" => set_flag(Flag.Carry, false)
    | "cld" => set_flag(Flag.Decimal, false)
    | "clv" => set_flag(Flag.Overflow, false)
    | "cmp" => compare
    | "eor" => xor_with_acc
    | "jmp" => jump
    | "jsr" => jump_subroutine
    | "lda" => load_acc
    | "ldx" => load_x
    | "nop" => nop
    | "ora" => or_with_acc
    | "pha" => push_acc
    | "php" => push_status
    | "pla" => pop_acc
    | "plp" => pop_status
    | "rts" => return_from_subroutine
    | "sec" => set_flag(Flag.Carry, true)
    | "sed" => set_flag(Flag.Decimal, true)
    | "sei" => set_flag(Flag.InterruptDisable, true)
    | "sta" => store_acc
    | "stx" => store_x
    | _ => raise(InstructionNotImplemented(definition.label))
    };

  operation(cpu, operand);

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