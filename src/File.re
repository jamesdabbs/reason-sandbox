let rom = (path: string): Rom.t => {
  Node.Fs.readFileSync(path, `binary) |> Bytes.of_string |> Rom.parse(path);
};

let cpu = (path: string): Cpu.t => rom(path) |> Memory.build |> Cpu.build;