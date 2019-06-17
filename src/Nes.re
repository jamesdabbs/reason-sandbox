type t = {
  cpu: Cpu.t,
  ppu: Ppu.t,
  rom: Rom.t,
  mutable scanline: int,
};

type frame = array(int);

type config = {
  log: string => unit,
  on_step: t => unit,
  on_frame: frame => unit,
};

let load = (rom: Rom.t): t => {
  let memory = Memory.build(rom);

  let cpu = Cpu.build(memory);
  Cpu.reset(cpu);

  let ppu = Memory.ppu(memory);

  {cpu, ppu, rom, scanline: 0};
};

let step = (nes: t) => {
  Cpu.step(nes.cpu);

  if (nes.cpu.cycles > 112) {
    nes.scanline =
      Render.handle_scanline(nes.ppu, nes.scanline, ~on_nmi=() =>
        Cpu.nmi(nes.cpu)
      );
    nes.cpu.cycles = nes.cpu.cycles mod 113;
  };
};

let step_to = (nes: t, ~halt: unit => bool) => {
  let rec go = () => {
    step(nes);
    if (!halt()) {
      go();
    };
  };

  go();
};

let step_frame = (nes: t) => {
  step_to(nes, ~halt=() => nes.scanline != 0);
  step_to(nes, ~halt=() => nes.scanline == 0);
};