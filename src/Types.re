type cpu = {
  memory: Memory.t,
  mutable status: Flag.Register.t,
  mutable cycles: int,
  mutable x: int,
  mutable y: int,
  mutable acc: int,
  mutable stack: int,
  mutable pc: int,
};