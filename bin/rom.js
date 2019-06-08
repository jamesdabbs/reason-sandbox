const Cpu = require('../src/cpu.bs')
const File = require('../src/file.bs')

const rom_path = process.argv[2]
if (!rom_path) {
  console.error('Path to ROM required')
  process.exit(1)
}

const cpu = File.cpu(rom_path)
Cpu.reset(cpu)

const steps = parseInt(process.argv[3] || 25)

for (let i = 0; i < steps; i++) {
  console.log(Cpu.now(cpu));
  Cpu.step(cpu);
}