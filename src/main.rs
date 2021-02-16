use std::ops::{Index, IndexMut};

use stopwatch::Stopwatch;

use sixfiveohtwo::{opcode, Reset, R6502, SimpleMemory, Memory, Registers};
use std::io::{Read, Write};
use std::any::Any;

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
    fn read_byte(&mut self, regs: &mut Registers, addr: u16) -> u8 {
        match addr & 0xFF1F {
            0xD010 => {
                let mut b = &mut [0u8][..];
                std::io::stdin().lock().read_exact(&mut b).expect("read from stdin");
                let c = if b[0] == 10 { 13 } else { b[0] };
                return c | 0x80;
            }

            0xD011 => {
                if regs.pc == 0xE006 { // READ
                    return 0x80;
                } else {
                    return 0;
                }
            }

            0xD012 => {
                return 0;
            }

            _ => self.mem.read_byte(regs, addr)
        }
    }

    fn write_byte(&mut self, regs: &mut Registers, addr: u16, value: u8) {
        if addr & 0xFF1F == 0xD012 {
            let value = value & 0x7f;
            let value = if value == 13 { 10 } else { value };

            // charout
            let ch = value;

            let s = regs.sp;
            let a = 1 + (self.mem[0x0100 + s as u16 + 1] as u16) | (self.mem[0x1000 + ((s as u16 + 2) & 0xff)] as u16) << 8;

            /*
             * Apple I BASIC prints every character received
             * from the terminal. UNIX terminals do this
             * anyway, so we have to avoid printing every
             * line again
             */
            if a == 0xe2a6 {
                /* character echo */
                return;
            }
            if a == 0xe2b6 {
                /* CR echo */
                return;
            }

            /*
             * Apple I BASIC prints a line break and 6 spaces
             * after every 37 characters. UNIX terminals do
             * line breaks themselves, so ignore these
             * characters
             */
            if a==0xe025 && (ch==10 || ch==b' ') {
                return;
            }

            if a == 0xe182 {  // INPUT
                // FIXME: return if stdin is not a tty
            }

            print!("{}", ch as char);
            let chb = [ch];
            std::io::stdout().write(&chb[..]);
            std::io::stdout().flush();

            return;
        }

        self.mem.write_byte(regs, addr, value)
    }
}

static APPLE_1_BASIC: &[u8; 4096] = include_bytes!("../apple1basic.bin");

impl Apple1BasicMem {
    fn new() -> Self {
        let mut mem = SimpleMemory::new();

        for (i, b) in APPLE_1_BASIC.iter().enumerate() {
            mem[(i + 0xE000) as u16] = *b;
        }

        mem[0xFFFC] = 0x00;
        mem[0xFFFD] = 0xE0;

        Apple1BasicMem {
            mem,
        }
    }
}

fn main() {
    let mut mem = Apple1BasicMem::new();

    let mut cpu = R6502::new(mem);
    cpu.reset();

    // FIXME; I have no clue how to implement reset vector handling... so... fake it?
    cpu.r.pc = 0xe000;

    let cycles = 100000000;
    let s = Stopwatch::start_new();
    cpu.execute(cycles);
    let s = s.elapsed_ms();

    println!("Executed {} instructions in {}ms", cycles, s);

    println!("{}", cpu);
}
