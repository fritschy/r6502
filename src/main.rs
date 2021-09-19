use std::ops::{Index, IndexMut};

use sixfiveohtwo::{Reset, R6502, SimpleMemory, Memory, Registers};
use std::io::{Read, Write};
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum RW {
    Read(u16, u8),
    Write(u16, u8),
    None,
}

impl Display for RW {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        // R$E3D6=$F2
        match self {
            RW::Read(a, v) => write!(f, "{}${:04X}=${:02X}", "R", *a, *v),
            RW::Write(a, v) => write!(f, "{}${:04X}=${:02X}", "W", *a, *v),
            _ => Ok(()),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum MemoryAccess {
    Write{regs: Registers, addr: u16, val: u8},
    Read{regs: Registers, addr: u16, val: u8},
}

impl Display for MemoryAccess {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            MemoryAccess::Read{addr, regs, val} => write!(f, "R {} 0x{:04x} -> {:02x}", regs, addr, val),
            MemoryAccess::Write{addr, regs, val} => write!(f, "W {} 0x{:04x} <- {:02x}", regs, addr, val),
        }
    }
}

struct Apple1BasicMem {
    mem: SimpleMemory,
    record: bool,
    accesses: Vec<MemoryAccess>,
    frame_no: u64,
    write_accesses: u64,
    dump_images: bool,
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
        let b = (|| {
            match addr & 0xFF1F {
                0xD010 => {
                    let mut b = &mut [0u8][..];
                    match std::io::stdin().lock().read_exact(&mut b) {
                        Err(_) => return 0x80,
                        Ok(_) => {
                            let c = if b[0] == 10 { 13 } else { b[0] };
                            return c | 0x80;
                        }
                    }
                }

                0xD011 => {
                    if regs.pc == 0xE006 { // READ
                        return 0x80;
                    }
                    return 0;
                }

                0xD012 => {
                    return 0;
                }

                _ => self.mem.read_byte(regs, addr)
            }
        })();
        if self.record {
            self.accesses.push(MemoryAccess::Read { addr: addr, regs: regs.clone(), val: b });
        }
        b
    }

    fn write_byte(&mut self, regs: &mut Registers, addr: u16, value: u8) {
        if self.record {
            self.accesses.push(MemoryAccess::Write { addr: addr, regs: regs.clone(), val: value });
        }

        if addr & 0xFF1F == 0xD012 {
            let value = value & 0x7f;
            let value = if value == 13 { 10 } else { value };

            // charout
            let ch = value;

            let s = regs.sp;
            let a = 1 + (self.mem[0x0100 + s as u16 + 1] as u16) | (self.mem[0x0100 + ((s as u16 + 2) & 0xff)] as u16) << 8;

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
            if a == 0xe025 && (ch == 10 || ch == b' ') {
                return;
            }

            if a == 0xe182 {  // INPUT
                // FIXME: return if stdin is not a tty
            }

            print!("{}", ch as char);
            std::io::stdout().flush().expect("flush()");

            return;
        }

        self.mem.write_byte(regs, addr, value);

        if self.write_accesses % 64 == 0 {
            self.dump_frame();
        }

        self.write_accesses += 1;
    }
}

static APPLE_1_BASIC: &[u8; 4096] = include_bytes!("../apple1basic.bin");

impl Apple1BasicMem {
    fn new(record: bool, dumps: bool) -> Self {
        let mut mem = SimpleMemory::new();

        for (i, b) in APPLE_1_BASIC.iter().enumerate() {
            mem[(i + 0xE000) as u16] = *b;
        }

        mem[0xFFFC] = 0x00;
        mem[0xFFFD] = 0xE0;

        Apple1BasicMem {
            mem,
            accesses: Vec::new(),
            record,
            frame_no: 0,
            write_accesses: 0,
            dump_images: dumps,
        }
    }

    fn dump_frame(&mut self) {
        if self.dump_images {
            std::fs::OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(format!("mem-{:05}.pgm", self.frame_no))
                .and_then(|mut f| {
                    f.write_all(b"P5\n256 256\n255\n")?;
                    f.write_all(self.mem.get_memory())
                })
                .expect("Write Image");
            self.frame_no += 1;
        }
    }
}

fn main() -> Result<(), std::io::Error> {
    let m = clap::App::new("r6502")
        .args(&[
            // clap::Arg::with_name("basic")
            //     .short("b")
            //     .long("basic")
            //     .help("Run Apple 1 BASIC")
            //     .takes_value(false),
            clap::Arg::with_name("debug")
                .short("d")
                .long("debug")
                .help("Print every instruction and its machine state"),
            clap::Arg::with_name("memory-log")
                .short("m")
                .long("memory-log")
                .help("Record every memory access, output at the end"),
            clap::Arg::with_name("memory-dumps")
                .short("M")
                .long("memory-dumps")
                .help("Record every memory access, output at the end"),
        ])
        .get_matches();

    let debug = m.is_present("debug");

    let mem = Apple1BasicMem::new(m.is_present("memory-log"), m.is_present("memory-dumps"));

    let mut cpu = R6502::new(mem);
    cpu.reset();

    // FIXME; I have no clue how to implement reset vector handling... so... fake it?
    cpu.r.pc = cpu.read_word(0xfffc);

    loop {
        let ins = cpu.step();

        if debug {
            eprintln!("halfcyc:{} phi0:_ AB:____ D:__ RnW:_ PC:{:02X} A:{:02X} X:{:02X} Y:{:02X} SP:{:02X} P:{:02X} IR:{:02X}",
                      cpu.cycle_count, cpu.r.pc, cpu.r.a, cpu.r.x, cpu.r.y, cpu.r.sp, cpu.r.sr, ins,
            );
        }

        if cpu.hlt {
            eprintln!("EXIT requested!");
            break;
        }
    }

    if m.is_present("memory-log") {
        println!("Memory accesses:");
        for a in cpu.mem.accesses.iter() {
            println!("{}", a);
        }
    }

    Ok(())
}
