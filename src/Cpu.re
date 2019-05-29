type cpu = {
  memory: Memory.memory,
  mutable cycles: int,
  mutable x: int,
  mutable y: int,
  mutable acc: int,
  mutable stack: int,
  mutable status: int,
  mutable pc: int,
};

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
  Printf.sprintf("%04X A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%i",
  cpu.pc, cpu.acc, cpu.x, cpu.y, cpu.status, cpu.stack, cpu.cycles);
};

exception InstructionNotImplemented(int);

let jump = cpu => {
  cpu.pc = Memory.get_word(cpu.memory, cpu.pc);
  cpu.cycles = cpu.cycles + 3;
};

let step = cpu => {
  let instruction = Memory.get_byte(cpu.memory, cpu.pc);
  cpu.pc = cpu.pc + 1;

  switch (instruction) {
  | 0x4c => jump(cpu)
  | _ => raise(InstructionNotImplemented(instruction))
  };
};