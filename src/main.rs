use std::ops::{Index, IndexMut};

use stopwatch::Stopwatch;

use sixfiveohtwo::{opcode, Reset, R6502, SimpleMemory, Memory};

struct Apple1BasicMem {
    mem: SimpleMemory,
}

impl Index<u16> for Apple1BasicMem {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        &self.mem[index]
    }
}

impl IndexMut<u16> for Apple1BasicMem {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        &mut self.mem[index]
    }
}

impl Memory for Apple1BasicMem {
    fn read_byte(&mut self, addr: u16) -> u8 {
        let res = self.mem.read_byte(addr);
        println!("R: 0x{:04x} -> 0x{:02x}", addr, res);
        res
    }

    fn write_byte(&mut self, addr: u16, value: u8) {
        println!("W: 0x{:04x} <- 0x{:02x}", addr, value);
        self.mem.write_byte(addr, value)
    }
}

impl Apple1BasicMem {
    fn new() -> Self {
        Apple1BasicMem {
            mem: SimpleMemory::new()
        }
    }
}

fn main() {
    let mut mem = Apple1BasicMem::new();

    // Load 0x42 to A
    mem[0x200] = opcode::LDA_IM;
    mem[0x201] = 0x42;

    // Load (0x0201) to X  (0x42)
    mem[0x202] = opcode::LDX_ABS;
    mem[0x203] = 0x01;
    mem[0x204] = 0x02;

    // get_0x11()
    mem[0x205] = opcode::JSR_ABS;
    mem[0x206] = 0x00;
    mem[0x207] = 0x10;

    // JMP 0x0200
    mem[0x208] = opcode::JMP_ABS;
    mem[0x209] = 0x00;
    mem[0x20a] = 0x02;

    // Small subroutine: get_0x11()
    mem[0x1000] = opcode::LDA_IM;
    mem[0x1001] = 0x11;

    mem[0x1002] = opcode::ROL_ABS;
    mem[0x1003] = 0x0;
    mem[0x1004] = 0x20;
    mem[0x2000] = 0b10100101;

    mem[0x1005] = opcode::ROR_ABS;
    mem[0x1006] = 0x1;
    mem[0x1007] = 0x20;
    mem[0x2001] = 0b11100000;

    mem[0x1008] = opcode::RTS;

    let mut cpu = R6502::new(mem);

    cpu.reset();
    cpu.r.pc = 0x200;

    let cycles = 100000000;
    let s = Stopwatch::start_new();
    cpu.execute(cycles);
    let s = s.elapsed_ms();

    println!("Executed {} instructions in {}ms", cycles, s);

    println!("{}", cpu);
}
