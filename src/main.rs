// Based on the documentation at http://www.obelisk.me.uk/6502/

use std::ops::{Index, IndexMut};
use std::hint::unreachable_unchecked;

type Byte = u8;
type Word = u16;

mod instr;

trait Reset {
    fn reset(&mut self);
}

pub struct R6502 {
    pc: Word,

    sp: Byte,

    a: Byte,
    x: Byte,
    y: Byte,

    // Processor Status
    // CF : Carry Flag
    // Z: Zero FLag
    // ID: Interrupt Disable
    // DM: Decimal Mode
    // BC: Break Command
    // OF: Overflow Flag
    // NF: Negative Flag
    sr: Byte,

    // clock counter
    count: u64,
}

impl Reset for R6502 {
    fn reset(&mut self) {
        self.pc = 0xfffc;
        self.sp = 0xff;  // 0x100 - the 1 is implied
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sr = self.sr & !status_flag::D;
    }
}

pub enum AMValue {
    Address(u16),
    Value(u8),
    Accumulator,
    None,
}

impl AMValue {
    fn to_addr(self) -> u16 {
        match self {
            AMValue::Address(a) => a,
            _ => panic!("Cannot get address of a Value"),
        }
    }

    fn to_value(self) -> u8 {
        match self {
            AMValue::Value(a) => a,
            _ => panic!("Cannot get value of an Address"),
        }
    }

    fn is_accumulator(self) -> bool {
        match self {
            AMValue::Accumulator => true,
            _ => false,
        }
    }
}

fn page(addr: u16) -> u8 {
    (addr >> 8) as u8
}

impl From<u8> for AMValue {
    fn from(v: u8) -> Self {
        AMValue::Value(v)
    }
}

impl From<u16> for AMValue {
    fn from(v: u16) -> Self {
        AMValue::Address(v)
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum am_select {
    A,
    V,
}

pub fn implied(cpu: &mut R6502, mem: &mut Memory, mode: am_select) -> AMValue {
    unreachable!("implied addressing mode should NEVER try to resolve its operand");
}

pub fn immediate(cpu: &mut R6502, mem: &mut Memory, mode: am_select) -> AMValue {
    match mode {
        am_select::A => cpu.pc.into(),
        am_select::V => cpu.fetch_byte_with_pc(mem).into(),
    }
}

pub fn accumulator(cpu: &mut R6502, mem: &mut Memory, mode: am_select) -> AMValue {
    match mode {
        am_select::V => cpu.a.into(),
        am_select::A => AMValue::Accumulator,
    }
}

pub fn absolute(cpu: &mut R6502, mem: &mut Memory, mode: am_select) -> AMValue {
    let addr = cpu.fetch_byte_with_pc(mem) as u16;
    let addr = addr | (cpu.fetch_byte_with_pc(mem) as u16) << 8;
    match mode {
        am_select::A => addr.into(),
        am_select::V => cpu.fetch_byte_with_address(mem, addr).into(),
    }
}

pub fn x_indexed_absolute(cpu: &mut R6502, mem: &mut Memory, mode: am_select) -> AMValue {
    let addr = absolute(cpu, mem, am_select::A).to_addr();
    let addr = addr.wrapping_add(cpu.x as u16);
    match mode {
        am_select::A => addr.into(),
        am_select::V => cpu.fetch_byte_with_address(mem, addr).into(),
    }
}

pub fn y_indexed_absolute(cpu: &mut R6502, mem: &mut Memory, mode: am_select) -> AMValue {
    let addr = absolute(cpu, mem, am_select::A).to_addr();
    let addr = addr.wrapping_add(cpu.y as u16);
    match mode {
        am_select::A => addr.into(),
        am_select::V => cpu.fetch_byte_with_address(mem, addr).into(),
    }
}

// This is only ever possible with JMP, hence we just set the PC
pub fn absolute_indirect(cpu: &mut R6502, mem: &mut Memory, mode: am_select) -> AMValue {
    let addr = absolute(cpu, mem, am_select::A).to_addr();
    let next = cpu.fetch_byte_with_address(mem, addr) as u16;
    let next = next | (cpu.fetch_byte_with_address(mem, addr.wrapping_add(1)) as u16) << 8;
    match mode {
        am_select::A => next.into(),
        _ => unreachable!(),
    }
}

pub fn zero_page(cpu: &mut R6502, mem: &mut Memory, mode: am_select) -> AMValue {
    let op = cpu.fetch_byte_with_pc(mem) as u16;
    match mode {
        am_select::A => op.into(),
        am_select::V => cpu.fetch_byte_with_address(mem, op).into(),
    }
}

pub fn x_indexed_zero_page(cpu: &mut R6502, mem: &mut Memory, mode: am_select) -> AMValue {
    let op = cpu.fetch_byte_with_pc(mem);
    let addr = cpu.x.wrapping_add(op) as u16;
    match mode {
        am_select::A => addr.into(),
        am_select::V => cpu.fetch_byte_with_address(mem, addr).into(),
    }
}

pub fn y_indexed_zero_page(cpu: &mut R6502, mem: &mut Memory, mode: am_select) -> AMValue {
    let op = cpu.fetch_byte_with_pc(mem);
    let addr = cpu.y.wrapping_add(op) as u16;
    match mode {
        am_select::A => addr.into(),
        am_select::V => cpu.fetch_byte_with_address(mem, addr).into(),
    }
}

pub fn x_indexed_zero_page_indirect(cpu: &mut R6502, mem: &mut Memory, mode: am_select) -> AMValue {
    let op = cpu.fetch_byte_with_pc(mem);
    let addr = cpu.x.wrapping_add(op) as u16;
    let next = cpu.fetch_byte_with_address(mem, addr) as u16;
    let next = next | (cpu.fetch_byte_with_address(mem, addr.wrapping_add(1)) as u16) << 8;
    match mode {
        am_select::A => next.into(),
        am_select::V => cpu.fetch_byte_with_address(mem, next).into(),
    }
}

pub fn zero_page_indirect_y_indexed(cpu: &mut R6502, mem: &mut Memory, mode: am_select) -> AMValue {
    let op = cpu.fetch_byte_with_pc(mem);
    let addr = op as u16;
    let next = cpu.fetch_byte_with_address(mem, addr) as u16;
    let next = next | (cpu.fetch_byte_with_address(mem, addr.wrapping_add(1)) as u16) << 8;
    let next = next.wrapping_add(cpu.y as u16);
    match mode {
        am_select::A => next.into(),
        am_select::V => cpu.fetch_byte_with_address(mem, next).into(),
    }
}

// XXX: this is only ever used by JMP
pub fn relative(cpu: &mut R6502, mem: &mut Memory, mode: am_select) -> AMValue {
    let op = cpu.fetch_byte_with_pc(mem);
    let op = op as i8;
    // XXX I am not happy with this wild around sign/unsign casting
    let pc = cpu.pc as i32;
    let pc = pc + op as i32;
    match mode {
        am_select::A => AMValue::from(pc as u16),
        _ => unreachable!(),
    }
}

impl R6502 {
    // We have 64KiB memory, of which a number of regions are special purpose:
    // 0x0000 - 0x00ff: "zero page"
    // 0x0100 - 0x01ff: "system stack"
    // 0xfffa - 0xfffb: NMI handler
    // 0xfffc - 0xfffd: power on reset location
    // 0xfffe - 0xffff: BRK/IRQ handler
    pub fn push(&mut self, mem: &mut Memory, v: Byte) {
        mem[0x0100 + self.sp as Word] = v;
        self.sp -= 1;
    }

    pub fn pop(&mut self, mem: &mut Memory) -> Byte {
        self.sp += 1;
        mem[0x0100 + self.sp as Word]
    }

    pub fn new() -> Self {
        R6502 {
            pc: 0,
            sp: 0,
            a: 0,
            x: 0,
            y: 0,
            sr: 0,
            count: 0,
        }
    }

    pub fn fetch_byte_with_pc(&mut self, mem: &mut Memory) -> Byte {
        let b = self.fetch_byte_with_address(mem, self.pc);
        self.pc += 1;
        b
    }

    pub fn fetch_byte_with_address(&mut self, mem: &Memory, addr: u16) -> Byte {
        self.count += 1;
        mem[addr]
    }

    pub fn write_byte_to_address(&mut self, mem: &mut Memory, addr: u16, value: u8) {
        self.count += 1;
        mem[addr] = value;
    }

    pub fn set_flag(&mut self, flag: Byte, val: bool) {
        match val {
            true => self.sr |= flag,
            false => self.sr &= !(flag | status_flag::UNUSED),
        }
    }

    fn instr_lda(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.a = addr_mode(self, mem, am_select::V).to_value();
        self.set_flag(status_flag::N, self.a & 0x80 != 0);
        self.set_flag(status_flag::Z, self.a == 0);
    }

    fn instr_ldx(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.x = addr_mode(self, mem, am_select::V).to_value();
        self.set_flag(status_flag::N, self.x & 0x80 != 0);
        self.set_flag(status_flag::Z, self.x == 0);
    }

    fn instr_ldy(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.y = addr_mode(self, mem, am_select::V).to_value();
        self.set_flag(status_flag::N, self.y & 0x80 != 0);
        self.set_flag(status_flag::Z, self.y == 0);
    }

    fn instr_sta(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        let addr = addr_mode(self, mem, am_select::A).to_addr();
        self.write_byte_to_address(mem, addr, self.a);
    }

    fn instr_stx(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        let addr = addr_mode(self, mem, am_select::A).to_addr();
        self.write_byte_to_address(mem, addr, self.x);
    }

    fn instr_sty(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        let addr = addr_mode(self, mem, am_select::A).to_addr();
        self.write_byte_to_address(mem, addr, self.y);
    }

    fn instr_tax(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.x = self.a;
        self.set_flag(status_flag::N, self.x & 0x80 != 0);
        self.set_flag(status_flag::Z, self.x == 0);
    }

    fn instr_tay(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.y = self.a;
        self.set_flag(status_flag::N, self.y & 0x80 != 0);
        self.set_flag(status_flag::Z, self.y == 0);
    }

    fn instr_tsx(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.x = self.sp;
        self.set_flag(status_flag::N, self.x & 0x80 != 0);
        self.set_flag(status_flag::Z, self.x == 0);
    }

    fn instr_txa(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.a = self.x;
        self.set_flag(status_flag::N, self.a & 0x80 != 0);
        self.set_flag(status_flag::Z, self.a == 0);
    }

    fn instr_txs(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.sp = self.x;
        self.set_flag(status_flag::N, self.sp & 0x80 != 0);
        self.set_flag(status_flag::Z, self.sp == 0);
    }

    fn instr_tya(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.a = self.y;
        self.set_flag(status_flag::N, self.a & 0x80 != 0);
        self.set_flag(status_flag::Z, self.a == 0);
    }

    fn instr_pha(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.push(mem, self.a);
    }

    fn instr_php(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.push(mem, self.sr);
    }

    fn instr_pla(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.a = self.pop(mem);
        self.set_flag(status_flag::N, self.a & 0x80 != 0);
        self.set_flag(status_flag::Z, self.a == 0);
    }

    fn instr_plp(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.sr = self.pop(mem);
    }

    fn instr_asl(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        let m = addr_mode(self, mem, am_select::A);
        let (o, m) = match m {
            AMValue::Accumulator => {
                let a = self.a;
                let b = a << 1;
                self.a = b;
                (a, b)
            }
            AMValue::Address(addr) => {
                let a = self.fetch_byte_with_address(mem, addr);
                let b = a << 1;
                self.write_byte_to_address(mem, addr, b);
                (a, b)
            }
            _ => unreachable!(),
        };
        self.set_flag(status_flag::C, o & 0x80 != 0);
        self.set_flag(status_flag::N, m & 0x80 != 0);
        self.set_flag(status_flag::Z, m == 0);
    }

    fn instr_lsr(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        let m = addr_mode(self, mem, am_select::A);
        let (o, m) = match m {
            AMValue::Accumulator => {
                let a = self.a;
                let b = a >> 1;
                self.a = b;
                (a, b)
            }
            AMValue::Address(addr) => {
                let a = self.fetch_byte_with_address(mem, addr);
                let b = a >> 1;
                self.write_byte_to_address(mem, addr, b);
                (a, b)
            }
            _ => unreachable!(),
        };
        self.set_flag(status_flag::C, o & 0x1 != 0);
        self.set_flag(status_flag::N, m & 0x80 != 0);
        self.set_flag(status_flag::Z, m == 0);
    }

    fn instr_rol(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        let m = addr_mode(self, mem, am_select::A);
        let (o, m) = match m {
            AMValue::Accumulator => {
                let a = self.a;
                let b = a.rotate_left(1);
                self.a = b;
                (a, b)
            }
            AMValue::Address(addr) => {
                let a = self.fetch_byte_with_address(mem, addr);
                let b = a.rotate_left(1);
                self.write_byte_to_address(mem, addr, b);
                (a, b)
            }
            _ => unreachable!(),
        };
        self.set_flag(status_flag::C, o & 0x80 != 0);
        self.set_flag(status_flag::N, m & 0x80 != 0);
        self.set_flag(status_flag::Z, m == 0);
    }

    fn instr_ror(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        let m = addr_mode(self, mem, am_select::A);
        let (o, m) = match m {
            AMValue::Accumulator => {
                let a = self.a;
                let b = a.rotate_right(1);
                self.a = b;
                (a, b)
            }
            AMValue::Address(addr) => {
                let a = self.fetch_byte_with_address(mem, addr);
                let b = a.rotate_right(1);
                self.write_byte_to_address(mem, addr, b);
                (a, b)
            }
            _ => unreachable!(),
        };
        self.set_flag(status_flag::C, o & 0x1 != 0);
        self.set_flag(status_flag::N, m & 0x80 != 0);
        self.set_flag(status_flag::Z, m == 0);
    }

    fn instr_jmp(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.pc = addr_mode(self, mem, am_select::A).to_addr();
    }

    fn instr_brk(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        let sr = self.sr;
        let pc = self.pc + 2;
        self.push(mem, (pc >> 8) as u8);
        self.push(mem, (pc & 0xff) as u8);
        self.push(mem, sr);
        self.set_flag(status_flag::I, true);
        self.pc = 0xfffe;
        self.count += 7;
    }

    fn instr_rti(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        let sr = self.pop(mem);
        let pcl = self.pop(mem);
        let pch = self.pop(mem);
        let pc = pcl as u16 | (pch as u16) << 8;
        self.sr = sr & !status_flag::B;
        self.pc = pc;
        self.count += 6;
    }

    fn instr_jsr(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        let pc = self.pc + 2;
        self.push(mem, (pc >> 8) as u8);
        self.push(mem, (pc & 0xff) as u8);
        let addr = addr_mode(self, mem, am_select::A).to_addr();
        self.pc = addr;
        self.count += 6;
    }

    fn instr_rts(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        let pcl = self.pop(mem);
        let pch = self.pop(mem);
        let pc = pcl as u16 | (pch as u16) << 8;
        self.pc = pc;
        self.count += 6;
    }

    fn instr_bcc(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        if self.sr & status_flag::C == 0 {
            self.pc = addr_mode(self, mem, am_select::A).to_addr();
        }
    }

    fn instr_bcs(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        if self.sr & status_flag::C != 0 {
            self.pc = addr_mode(self, mem, am_select::A).to_addr();
        }
    }

    fn instr_beq(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        if self.sr & status_flag::Z != 0 {
            self.pc = addr_mode(self, mem, am_select::A).to_addr();
        }
    }

    fn instr_bmi(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        if self.sr & status_flag::N != 0 {
            self.pc = addr_mode(self, mem, am_select::A).to_addr();
        }
    }

    fn instr_bne(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        if self.sr & status_flag::Z == 0 {
            self.pc = addr_mode(self, mem, am_select::A).to_addr();
        }
    }

    fn instr_bpl(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        if self.sr & status_flag::N == 0 {
            self.pc = addr_mode(self, mem, am_select::A).to_addr();
        }
    }

    fn instr_bvc(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        if self.sr & status_flag::V == 0 {
            self.pc = addr_mode(self, mem, am_select::A).to_addr();
        }
    }

    fn instr_bvs(&mut self, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        if self.sr & status_flag::V != 0 {
            self.pc = addr_mode(self, mem, am_select::A).to_addr();
        }
    }

    fn instr_clc(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.sr &= !status_flag::C;
    }

    fn instr_cld(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.sr &= !status_flag::D;
    }

    fn instr_cli(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.sr &= !status_flag::I;
    }

    fn instr_clv(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.sr &= !status_flag::V;
    }

    fn instr_sec(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.sr |= status_flag::C;
    }

    fn instr_sed(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.sr |= status_flag::D;
    }

    fn instr_sei(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
        self.sr |= status_flag::I;
    }

    fn instr_nop(&mut self, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, am_select) -> AMValue) {
    }

    fn execute(&mut self, mem: &mut Memory, mut count: isize) {
        while count > 0 {
            count -= 1;
            let ins = self.fetch_byte_with_pc(mem);

            dbg!(self.pc, ins);

            match ins {
                instr::LDA_IM => self.instr_lda(mem, immediate),
                instr::LDA_ABS => self.instr_lda(mem, absolute),
                instr::LDA_XIA => self.instr_lda(mem, x_indexed_absolute),
                instr::LDA_YIA => self.instr_lda(mem, y_indexed_absolute),
                instr::LDA_ZP => self.instr_lda(mem, zero_page),
                instr::LDA_XIZ => self.instr_lda(mem, x_indexed_zero_page),
                instr::LDA_XIZI => self.instr_lda(mem, x_indexed_zero_page_indirect),

                instr::LDX_IM => self.instr_ldx(mem, immediate),
                instr::LDX_ABS => self.instr_ldx(mem, absolute),
                instr::LDX_YIA => self.instr_ldx(mem, y_indexed_absolute),
                instr::LDX_ZP => self.instr_ldx(mem, zero_page),
                instr::LDX_YIZ => self.instr_ldx(mem, y_indexed_zero_page),

                instr::LDY_IM => self.instr_ldy(mem, immediate),
                instr::LDY_ABS => self.instr_ldy(mem, absolute),
                instr::LDY_XIA => self.instr_ldy(mem, x_indexed_absolute),
                instr::LDY_ZP => self.instr_ldy(mem, zero_page),
                instr::LDY_XIZ => self.instr_ldy(mem, x_indexed_zero_page),

                instr::STA_ABS => self.instr_sta(mem, absolute),
                instr::STA_XIA => self.instr_sta(mem, x_indexed_absolute),
                instr::STA_YIA => self.instr_sta(mem, y_indexed_absolute),
                instr::STA_ZP => self.instr_sta(mem, zero_page),
                instr::STA_XIZ => self.instr_sta(mem, x_indexed_zero_page),
                instr::STA_XIZI => self.instr_sta(mem, x_indexed_zero_page_indirect),
                instr::STA_ZIYI => self.instr_sta(mem, zero_page_indirect_y_indexed),

                instr::STX_ABS => self.instr_stx(mem, absolute),
                instr::STX_ZP => self.instr_stx(mem, zero_page),
                instr::STX_YIZ => self.instr_stx(mem, y_indexed_zero_page),

                instr::STY_ABS => self.instr_sty(mem, absolute),
                instr::STY_ZP => self.instr_sty(mem, zero_page),
                instr::STY_XIZ => self.instr_sty(mem, x_indexed_zero_page),

                instr::TAX => self.instr_tax(mem, implied),
                instr::TAY => self.instr_tay(mem, implied),
                instr::TSX => self.instr_tsx(mem, implied),
                instr::TXA => self.instr_txa(mem, implied),
                instr::TXS => self.instr_txs(mem, implied),
                instr::TYA => self.instr_tya(mem, implied),

                instr::PHA => self.instr_pha(mem, implied),
                instr::PHP => self.instr_php(mem, implied),
                instr::PLA => self.instr_pla(mem, implied),
                instr::PLP => self.instr_plp(mem, implied),

                instr::ASLA => self.instr_asl(mem, absolute),
                instr::ASL_ABS => self.instr_asl(mem, accumulator),
                instr::ASL_XIA => self.instr_asl(mem, x_indexed_absolute),
                instr::ASL_ZP => self.instr_asl(mem, zero_page),
                instr::ASL_XIZ => self.instr_asl(mem, x_indexed_zero_page),

                instr::LSRA => self.instr_lsr(mem, absolute),
                instr::LSR_ABS => self.instr_lsr(mem, accumulator),
                instr::LSR_XIA => self.instr_lsr(mem, x_indexed_absolute),
                instr::LSR_ZP => self.instr_lsr(mem, zero_page),
                instr::LSR_XIZ => self.instr_lsr(mem, x_indexed_zero_page),

                instr::ROLA => self.instr_rol(mem, accumulator),
                instr::ROL_ABS => self.instr_rol(mem, absolute),
                instr::ROL_XIA => self.instr_rol(mem, x_indexed_absolute),
                instr::ROL_ZP => self.instr_rol(mem, zero_page),
                instr::ROL_XIZ => self.instr_rol(mem, x_indexed_zero_page),

                instr::RORA => self.instr_ror(mem, accumulator),
                instr::ROR_ABS => self.instr_ror(mem, absolute),
                instr::ROR_XIA => self.instr_ror(mem, x_indexed_absolute),
                instr::ROR_ZP => self.instr_ror(mem, zero_page),
                instr::ROR_XIZ => self.instr_ror(mem, x_indexed_zero_page),

                instr::BRK => self.instr_brk(mem, implied),
                instr::JMP_ABS => self.instr_jmp(mem, absolute),
                instr::JMP_ABSI => self.instr_jmp(mem, absolute_indirect),
                instr::JSR_ABS => self.instr_jsr(mem, absolute),
                instr::RTI => self.instr_rti(mem, implied),
                instr::RTS => self.instr_rts(mem, implied),

                instr::BCC_REL => self.instr_bcc(mem, relative),
                instr::BCS_REL => self.instr_bcs(mem, relative),
                instr::BEQ_REL => self.instr_beq(mem, relative),
                instr::BMI_REL => self.instr_bmi(mem, relative),
                instr::BNE_REL => self.instr_bne(mem, relative),
                instr::BPL_REL => self.instr_bpl(mem, relative),
                instr::BVC_REL => self.instr_bvc(mem, relative),
                instr::BVS_REL => self.instr_bvs(mem, relative),

                instr::CLC => self.instr_clc(mem, implied),
                instr::CLD => self.instr_cld(mem, implied),
                instr::CLI => self.instr_cli(mem, implied),
                instr::CLV => self.instr_clv(mem, implied),

                instr::SEC => self.instr_sec(mem, implied),
                instr::SED => self.instr_sed(mem, implied),
                instr::SEI => self.instr_sei(mem, implied),

                instr::NOP => self.instr_nop(mem, implied),

                _ => unimplemented!(),
            }
        }
    }

    fn nmi(&mut self) {
        unimplemented!();
    }
}

// We have 64KiB memory, of which a number of regions are special purpose:
// 0x0000 - 0x00ff: "zero page"
// 0x0100 - 0x01ff: "system stack"
// 0xfffa - 0xfffb: NMI handler
// 0xfffc - 0xfffd: power on reset location
// 0xfffe - 0xffff: BRK/IRQ handler
pub struct Memory {
    memory: [Byte; 0xffff],
}

impl Reset for Memory {
    fn reset(&mut self) {
        self.memory.iter_mut().for_each(|x| *x = 0);
    }
}

impl Memory {
    fn new() -> Self {
        Memory {
            memory: [0u8; 0xffff],
        }
    }
}

impl Index<Word> for Memory {
    type Output = Byte;

    fn index(&self, index: Word) -> &Self::Output {
        &self.memory[index as usize]
    }
}

impl IndexMut<Word> for Memory {
    fn index_mut(&mut self, index: Word) -> &mut Self::Output {
        &mut self.memory[index as usize]
    }
}

struct Registers {}

#[allow(unused)]
mod status_flag {
    use super::Byte;

    // Carry
    pub const C: Byte = 0x1;

    // Zero
    pub const Z: Byte = 0x2;

    // Interrupt Disable
    pub const I: Byte = 0x4;

    // Decimal Mode
    pub const D: Byte = 0x8;

    // Break Command
    pub const B: Byte = 0x10;

    // UNUSED bit in the middle...
    pub const UNUSED: Byte = 0x20;

    // Overflow
    pub const V: Byte = 0x40;

    // Negative
    pub const N: Byte = 0x80;
}

fn main() {
    let mut cpu = R6502::new();
    let mut mem = Memory::new();

    cpu.reset();
    cpu.pc = 0x200;

    // Load 0x42 to A
    mem[0x200] = instr::LDA_IM;
    mem[0x201] = 0x42;

    // Load (0x0201) to X  (0x42)
    mem[0x202] = instr::LDX_ABS;
    mem[0x203] = 0x01;
    mem[0x204] = 0x02;

    // get_0x11()
    mem[0x205] = instr::JSR_ABS;
    mem[0x206] = 0x00;
    mem[0x207] = 0x10;

    // JMP 0x0200
    mem[0x208] = instr::JMP_ABS;
    mem[0x209] = 0x00;
    mem[0x20a] = 0x02;

    // Small subroutine: get_0x11()
    mem[0x1000] = instr::LDA_IM;
    mem[0x1001] = 0x11;

    mem[0x1002] = instr::ROL_ABS;
    mem[0x1003] = 0x0;
    mem[0x1004] = 0x20;
    mem[0x2000] = 0b10100101;

    mem[0x1005] = instr::RTS;

    cpu.execute(&mut mem, 1000);

    println!("Hello, world!");
}
