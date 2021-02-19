use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};

pub mod addressing_mode;
pub mod instr;
pub mod instr_table;

pub trait Reset {
    fn reset(&mut self);
}

/*
pub trait Monitor {
    fn read_request(&mut self, addr: u16) -> Option<u8>;
    fn write_request(&mut self, addr: u16, value: u8) -> Option<()>;
    fn opcode(&mut self, i: u8);
}

pub trait Processor<Mem: Memory, Mon: Monitor>
{
    fn push(&mut self, v: u8);
    fn pop(&mut self) ->  u8;

    fn read_byte(&mut self, addr: u16) -> u8;
    fn write_byte(&mut self, addr: u16, val: u8);

    fn push_word(&mut self, v: u16) {
        self.push((v >> 8) as u8);
        self.push(v as u8);
    }

    fn pop_word(&mut self) -> u16 {
        let l = self.pop();
        let h = self.pop();
        l as u16 | (h as u16) << 8
    }

    fn read_word(&mut self, addr: u16) -> u16 {
        let l = self.read_byte(addr);
        let h = self.read_byte(addr.wrapping_add(1));
        l as u16 | (h as u16) << 8
    }

    fn write_word(&mut self, addr: u16, value: u16) {
        self.write_byte(addr, value as u8);
        self.write_byte(addr.wrapping_add(1), (value >> 8) as u8);
    }

    fn set_flag(&mut self, flag: u8, val: bool);
    fn get_flag(&mut self, flag: u8) -> u8;


    pub fn nmi(&mut self) {
        // FIXME; this is an IRQ we have to handle?
        self.got_irq = true;
    }
}
*/

#[derive(Debug, Eq, PartialEq)]
pub struct Registers {
    pub pc: u16,

    pub sp: u8,

    pub a: u8,
    pub x: u8,
    pub y: u8,

    // Processor Status Bits, from low to high
    // C: Carry
    // Z: Zero
    // I: Interrupt Disable
    // D: Decimal Mode
    // B: Break
    // V: Overflow
    // N: Negative
    pub sr: u8,
}

impl Registers {
    fn new() -> Self {
        Registers {
            pc: 0,
            sp: 0,
            a: 0,
            x: 0,
            y: 0,
            sr: 0,
        }
    }
}

impl Reset for Registers {
    fn reset(&mut self) {
        self.pc = 0xfffc;
        self.sp = 0xff; // 0x100 - the 1 is implied
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sr = status_flag::B | status_flag::I | status_flag::Z;
    }
}

impl Display for Registers {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "A:{:02x}, X:{:02x}, Y:{:02x}, SP:{:02x}, PC:{:04x}, SR:{:08b}/{:02x}",
            self.a, self.x, self.y, self.sp, self.pc, self.sr, self.sr
        )
    }
}

pub struct R6502<M>
where
    M: 'static + Memory,
{
    pub r: Registers,
    pub mem: M,

    pub got_irq: bool,

    pub cycle_count: u64,
    pub instr_count: u64,
}

impl<M: Memory> Display for R6502<M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "R6502: {}, IC:{}, CC:{}",
            self.r,
            self.instr_count,
            self.cycle_count * 2 + 22
        )
    }
}

impl<M: Memory> Reset for R6502<M> {
    fn reset(&mut self) {
        self.r.reset();
        self.got_irq = false;
        self.cycle_count = 0;
        self.instr_count = 0;
    }
}

pub fn page(addr: u16) -> u8 {
    (addr >> 8) as u8
}

pub trait Memory {
    fn read_byte(&mut self, regs: &mut Registers, addr: u16) -> u8;
    fn write_byte(&mut self, regs: &mut Registers, addr: u16, value: u8);

    fn read_word(&mut self, regs: &mut Registers, addr: u16) -> u16 {
        self.read_byte(regs, addr) as u16 | (self.read_byte(regs, addr + 1) as u16) << 8
    }
}

trait CPU {}

impl<M: Memory> R6502<M> {
    // We have 64KiB memory, of which a number of regions are special purpose:
    // 0x0000 - 0x00ff: "zero page"
    // 0x0100 - 0x01ff: "system stack"
    // 0xfffa - 0xfffb: NMI handler
    // 0xfffc - 0xfffd: power on reset location
    // 0xfffe - 0xffff: BRK/IRQ handler
    fn push(&mut self, v: u8) {
        self.write_byte(0x0100 + self.r.sp as u16, v);
        self.r.sp -= 1;
    }

    fn pop(&mut self) -> u8 {
        self.r.sp += 1;
        self.read_byte(0x0100 + self.r.sp as u16)
    }

    fn push_word(&mut self, v: u16) {
        self.push((v >> 8) as u8);
        self.push(v as u8);
    }

    fn pop_word(&mut self) -> u16 {
        let l = self.pop();
        let h = self.pop();
        l as u16 | (h as u16) << 8
    }

    pub fn read_word(&mut self, addr: u16) -> u16 {
        self.mem.read_word(&mut self.r, addr)
    }

    pub fn read_byte(&mut self, addr: u16) -> u8 {
        self.cycle_count += 1;
        self.mem.read_byte(&mut self.r, addr)
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        self.cycle_count += 1;
        self.mem.write_byte(&mut self.r, addr, val)
    }

    pub fn new(mem: M) -> Self {
        R6502 {
            r: Registers::new(),
            got_irq: false,
            cycle_count: 0,
            instr_count: 0,
            mem,
        }
    }

    pub(crate) fn fetch_byte_with_pc(&mut self) -> u8 {
        let b = self.read_byte(self.r.pc);
        self.r.pc += 1;
        b
    }

    pub(crate) fn set_flag(&mut self, flag: u8, val: bool) {
        match val {
            true => self.r.sr |= flag,
            false => self.r.sr &= !(flag | status_flag::UNUSED),
        }
    }

    pub(crate) fn get_flag(&mut self, flag: u8) -> u8 {
        assert_ne!(flag, 0);
        assert_eq!(flag.count_ones(), 1);
        (self.r.sr >> flag.trailing_zeros()) & 1
    }

    pub fn step(&mut self) -> u64 {
        // Want to calculate number of cycles at the end
        let old_count = self.cycle_count;

        let ins = self.fetch_byte_with_pc();
        self.instr_count += 1;

        let tbl = instr_table::m6502_instr_table();

        match &tbl[ins as usize] {
            (instr_table::Instr::None, _am, _fun) => eprintln!("Unhandled instr 0x{:02x}, {}", ins, self),
            (_, am, fun) => fun(self, *am),
        }

        // match ins {
        //     /////// // Undocumented instructions, needed by apple1basic?
        //     /////// opcode::ISC_YIA => instr::isc(self, AddMode::YIA),
        //     /////// opcode::RRA_ABS => instr::rra(self, AddMode::ABS),
        //     /////// 0x7a | 0xda | 0x04 | 0x1c => instr::nop(self, AddMode::IMPL),
        //     /////// opcode::LAX_ZIYI => instr::lax(self, AddMode::ZIYI),
        //     // opcode::ISC_XIA => instr::isc(self, AM::XIA),

        //     // 0x02 | 0x12 | 0x22 | 0x32 | 0x42 | 0x52 | 0x62 | 0x72 | 0x92 | 0xb2 | 0xd2 | 0xf2 => {
        //     //     // JAM
        //     //     eprintln!("JAM! {}", self);
        //     //     break;
        //     // }
        //     _ => {
        //         eprintln!("Unhandled instr 0x{:02x}, {}", ins, self);
        //     }
        // }

        if self.got_irq {
            self.r.pc = self.read_word(0xfffa);
            self.got_irq = false;
        }

        eprintln!("halfcyc:{} phi0:_ AB:____ D:__ RnW:_ PC:{:02X} A:{:02X} X:{:02X} Y:{:02X} SP:{:02X} P:{:02X} IR:{:02X} _$____=$__",
                  self.cycle_count, self.r.pc, self.r.a, self.r.x, self.r.y, self.r.sp, self.r.sr, ins
        );

        self.cycle_count - old_count
    }

    pub fn execute(&mut self, mut count: isize) {
        #[allow(unused)]
        let mut ins_count = 0;

        while count > 0 {
            count -= 1;
            self.step();
            ins_count += 1;
        }
    }

    pub fn nmi(&mut self) {
        // FIXME; this is an IRQ we have to handle?
        self.got_irq = true;
    }
}

// We have 64KiB memory, of which a number of regions are special purpose:
// 0x0000 - 0x00ff: "zero page"
// 0x0100 - 0x01ff: "system stack"
// 0xfffa - 0xfffb: NMI handler
// 0xfffc - 0xfffd: power on reset location
// 0xfffe - 0xffff: BRK/IRQ handler
pub struct SimpleMemory {
    memory: [u8; 0x10000],
}

impl SimpleMemory {
    pub fn new() -> Self {
        SimpleMemory {
            memory: [0u8; 0x10000],
        }
    }
}

impl Index<u16> for SimpleMemory {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        &self.memory[index as usize]
    }
}

impl IndexMut<u16> for SimpleMemory {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        &mut self.memory[index as usize]
    }
}

impl Memory for SimpleMemory {
    fn read_byte(&mut self, _regs: &mut Registers, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn write_byte(&mut self, _regs: &mut Registers, addr: u16, value: u8) {
        self.memory[addr as usize] = value;
    }
}

#[allow(unused)]
mod status_flag {
    // Carry
    pub const C: u8 = 0x1;

    // Zero
    pub const Z: u8 = 0x2;

    // Interrupt Disable
    pub const I: u8 = 0x4;

    // Decimal Mode
    pub const D: u8 = 0x8;

    // Break Command
    pub const B: u8 = 0x10;

    // UNUSED bit in the middle...
    pub const UNUSED: u8 = 0x20;

    // Overflow
    pub const V: u8 = 0x40;

    // Negative
    pub const N: u8 = 0x80;
}
