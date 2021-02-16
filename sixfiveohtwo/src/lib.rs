use std::ops::{Index, IndexMut};
use std::fmt::{Display, Formatter};

pub mod adressing_mode;
pub mod instr;
pub mod opcode;

pub trait Reset {
    fn reset(&mut self);
}

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
            sr: 0
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
        self.sr = self.sr & !status_flag::D;
    }
}

impl Display for Registers {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "A:{:02x}, X:{:02x}, Y:{:02x}, SP:{:04x}, PC:{:04x}, SR:{:08b}", self.a, self.x, self.y, self.sp, self.pc, self.sr)
    }
}

pub struct R6502<M> where M: Memory {
    pub r: Registers,
    pub mem: M,

    pub got_irq: bool,

    // clock counter
    pub count: u64,
}

impl<M: Memory> Display for R6502<M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "R6502: {}", self.r)
    }
}

impl<M: Memory> Reset for R6502<M> {
    fn reset(&mut self) {
        self.r.reset();
        self.got_irq = false;
        self.count = 0;
    }
}

pub fn page(addr: u16) -> u8 {
    (addr >> 8) as u8
}

pub trait Memory {
    fn read_byte(&mut self, regs: &mut Registers, addr: u16) -> u8;
    fn write_byte(&mut self, regs: &mut Registers, addr: u16, value: u8);
}

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

    pub fn read_byte(&mut self, addr: u16) -> u8 {
        self.count += 1;
        self.mem.read_byte(&mut self.r, addr)
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        self.count += 1;
        self.mem.write_byte(&mut self.r, addr, val)
    }

    pub fn new(mem: M) -> Self {
        R6502 {
            r: Registers::new(),
            got_irq: false,
            count: 0,
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

    pub fn execute(&mut self, mut count: isize) {
        while count > 0 {
            count -= 1;
            let ins = self.fetch_byte_with_pc();

            dbg!(self.count, &self.r, ins);

            use adressing_mode::*;

            match ins {
                opcode::LDA_IM => instr::lda(self, immediate),
                opcode::LDA_ABS => instr::lda(self, absolute),
                opcode::LDA_XIA => instr::lda(self, x_indexed_absolute),
                opcode::LDA_YIA => instr::lda(self, y_indexed_absolute),
                opcode::LDA_ZP => instr::lda(self, zero_page),
                opcode::LDA_XIZ => instr::lda(self, x_indexed_zero_page),
                opcode::LDA_XIZI => instr::lda(self, x_indexed_zero_page_indirect),
                opcode::LDA_ZIYI => instr::lda(self, zero_page_indirect_y_indexed),

                opcode::LDX_IM => instr::ldx(self, immediate),
                opcode::LDX_ABS => instr::ldx(self, absolute),
                opcode::LDX_YIA => instr::ldx(self, y_indexed_absolute),
                opcode::LDX_ZP => instr::ldx(self, zero_page),
                opcode::LDX_YIZ => instr::ldx(self, y_indexed_zero_page),

                opcode::LDY_IM => instr::ldy(self, immediate),
                opcode::LDY_ABS => instr::ldy(self, absolute),
                opcode::LDY_XIA => instr::ldy(self, x_indexed_absolute),
                opcode::LDY_ZP => instr::ldy(self, zero_page),
                opcode::LDY_XIZ => instr::ldy(self, x_indexed_zero_page),

                opcode::STA_ABS => instr::sta(self, absolute),
                opcode::STA_XIA => instr::sta(self, x_indexed_absolute),
                opcode::STA_YIA => instr::sta(self, y_indexed_absolute),
                opcode::STA_ZP => instr::sta(self, zero_page),
                opcode::STA_XIZ => instr::sta(self, x_indexed_zero_page),
                opcode::STA_XIZI => instr::sta(self, x_indexed_zero_page_indirect),
                opcode::STA_ZIYI => instr::sta(self, zero_page_indirect_y_indexed),

                opcode::STX_ABS => instr::stx(self, absolute),
                opcode::STX_ZP => instr::stx(self, zero_page),
                opcode::STX_YIZ => instr::stx(self, y_indexed_zero_page),

                opcode::STY_ABS => instr::sty(self, absolute),
                opcode::STY_ZP => instr::sty(self, zero_page),
                opcode::STY_XIZ => instr::sty(self, x_indexed_zero_page),

                opcode::TAX => instr::tax(self, implied),
                opcode::TAY => instr::tay(self, implied),
                opcode::TSX => instr::tsx(self, implied),
                opcode::TXA => instr::txa(self, implied),
                opcode::TXS => instr::txs(self, implied),
                opcode::TYA => instr::tya(self, implied),

                opcode::PHA => instr::pha(self, implied),
                opcode::PHP => instr::php(self, implied),
                opcode::PLA => instr::pla(self, implied),
                opcode::PLP => instr::plp(self, implied),

                opcode::ASLA => instr::asl(self, absolute),
                opcode::ASL_ABS => instr::asl(self, accumulator),
                opcode::ASL_XIA => instr::asl(self, x_indexed_absolute),
                opcode::ASL_ZP => instr::asl(self, zero_page),
                opcode::ASL_XIZ => instr::asl(self, x_indexed_zero_page),

                opcode::LSRA => instr::lsr(self, absolute),
                opcode::LSR_ABS => instr::lsr(self, accumulator),
                opcode::LSR_XIA => instr::lsr(self, x_indexed_absolute),
                opcode::LSR_ZP => instr::lsr(self, zero_page),
                opcode::LSR_XIZ => instr::lsr(self, x_indexed_zero_page),

                opcode::ROLA => instr::rol(self, accumulator),
                opcode::ROL_ABS => instr::rol(self, absolute),
                opcode::ROL_XIA => instr::rol(self, x_indexed_absolute),
                opcode::ROL_ZP => instr::rol(self, zero_page),
                opcode::ROL_XIZ => instr::rol(self, x_indexed_zero_page),

                opcode::RORA => instr::ror(self, accumulator),
                opcode::ROR_ABS => instr::ror(self, absolute),
                opcode::ROR_XIA => instr::ror(self, x_indexed_absolute),
                opcode::ROR_ZP => instr::ror(self, zero_page),
                opcode::ROR_XIZ => instr::ror(self, x_indexed_zero_page),

                opcode::AND_IM => instr::and(self, immediate),
                opcode::AND_ABS => instr::and(self, absolute),
                opcode::AND_XIA => instr::and(self, x_indexed_absolute),
                opcode::AND_YIA => instr::and(self, y_indexed_absolute),
                opcode::AND_ZP => instr::and(self, zero_page),
                opcode::AND_XIZ => instr::and(self, x_indexed_zero_page),
                opcode::AND_XIZI => instr::and(self, x_indexed_zero_page_indirect),
                opcode::AND_ZIYI => instr::and(self, zero_page_indirect_y_indexed),

                opcode::BIT_ABS => instr::bit(self, absolute),
                opcode::BIT_ZP => instr::bit(self, zero_page),

                opcode::EOR_IM => instr::eor(self, immediate),
                opcode::EOR_ABS => instr::eor(self, absolute),
                opcode::EOR_XIA => instr::eor(self, x_indexed_absolute),
                opcode::EOR_YIA => instr::eor(self, y_indexed_absolute),
                opcode::EOR_ZP => instr::eor(self, zero_page),
                opcode::EOR_XIZ => instr::eor(self, x_indexed_zero_page),
                opcode::EOR_XIZI => instr::eor(self, x_indexed_zero_page_indirect),
                opcode::EOR_ZIYI => instr::eor(self, zero_page_indirect_y_indexed),

                opcode::ORA_IM => instr::ora(self, immediate),
                opcode::ORA_ABS => instr::ora(self, absolute),
                opcode::ORA_XIA => instr::ora(self, x_indexed_absolute),
                opcode::ORA_YIA => instr::ora(self, y_indexed_absolute),
                opcode::ORA_ZP => instr::ora(self, zero_page),
                opcode::ORA_XIZ => instr::ora(self, x_indexed_zero_page),
                opcode::ORA_XIZI => instr::ora(self, x_indexed_zero_page_indirect),
                opcode::ORA_ZIYI => instr::ora(self, zero_page_indirect_y_indexed),

                opcode::ADC_IM => instr::adc(self, immediate),
                opcode::ADC_ABS => instr::adc(self, absolute),
                opcode::ADC_XIA => instr::adc(self, x_indexed_absolute),
                opcode::ADC_YIA => instr::adc(self, y_indexed_absolute),
                opcode::ADC_ZP => instr::adc(self, zero_page),
                opcode::ADC_XIZ => instr::adc(self, x_indexed_zero_page),
                opcode::ADC_XIZI => instr::adc(self, x_indexed_zero_page_indirect),
                opcode::ADC_ZIYI => instr::adc(self, zero_page_indirect_y_indexed),

                opcode::CMP_IM => instr::cmp(self, immediate),
                opcode::CMP_ABS => instr::cmp(self, absolute),
                opcode::CMP_XIA => instr::cmp(self, x_indexed_absolute),
                opcode::CMP_YIA => instr::cmp(self, y_indexed_absolute),
                opcode::CMP_ZP => instr::cmp(self, zero_page),
                opcode::CMP_XIZ => instr::cmp(self, x_indexed_zero_page),
                opcode::CMP_XIZI => instr::cmp(self, x_indexed_zero_page_indirect),
                opcode::CMP_ZIYI => instr::cmp(self, zero_page_indirect_y_indexed),

                opcode::CPX_IM => instr::cpx(self, immediate),
                opcode::CPX_ABS => instr::cpx(self, absolute),
                opcode::CPX_ZP => instr::cpx(self, zero_page),

                opcode::CPY_IM => instr::cpy(self, immediate),
                opcode::CPY_ABS => instr::cpy(self, absolute),
                opcode::CPY_ZP => instr::cpy(self, zero_page),

                opcode::SBC_IM => instr::sbc(self, immediate),
                opcode::SBC_ABS => instr::sbc(self, absolute),
                opcode::SBC_XIA => instr::sbc(self, x_indexed_absolute),
                opcode::SBC_YIA => instr::sbc(self, y_indexed_absolute),
                opcode::SBC_ZP => instr::sbc(self, zero_page),
                opcode::SBC_XIZ => instr::sbc(self, x_indexed_zero_page),
                opcode::SBC_XIZI => instr::sbc(self, x_indexed_zero_page_indirect),
                opcode::SBC_ZIYI => instr::sbc(self, zero_page_indirect_y_indexed),

                opcode::DEC_ABS => instr::dec(self, absolute),
                opcode::DEC_XIA => instr::dec(self, x_indexed_absolute),
                opcode::DEC_ZP => instr::dec(self, zero_page),
                opcode::DEC_XIZ => instr::dec(self, x_indexed_zero_page),
                opcode::DEX => instr::dex(self, implied),
                opcode::DEY => instr::dey(self, implied),

                opcode::INC_ABS => instr::inc(self, absolute),
                opcode::INC_XIA => instr::inc(self, x_indexed_absolute),
                opcode::INC_ZP => instr::inc(self, zero_page),
                opcode::INC_XIZ => instr::inc(self, x_indexed_zero_page),
                opcode::INX => instr::inx(self, implied),
                opcode::INY => instr::iny(self, implied),

                opcode::BRK => instr::brk(self, implied),
                opcode::JMP_ABS => instr::jmp(self, absolute),
                opcode::JMP_ABSI => instr::jmp(self, absolute_indirect),
                opcode::JSR_ABS => instr::jsr(self, absolute),
                opcode::RTI => instr::rti(self, implied),
                opcode::RTS => instr::rts(self, implied),

                opcode::BCC_REL => instr::bcc(self, relative),
                opcode::BCS_REL => instr::bcs(self, relative),
                opcode::BEQ_REL => instr::beq(self, relative),
                opcode::BMI_REL => instr::bmi(self, relative),
                opcode::BNE_REL => instr::bne(self, relative),
                opcode::BPL_REL => instr::bpl(self, relative),
                opcode::BVC_REL => instr::bvc(self, relative),
                opcode::BVS_REL => instr::bvs(self, relative),

                opcode::CLC => instr::clc(self, implied),
                opcode::CLD => instr::cld(self, implied),
                opcode::CLI => instr::cli(self, implied),
                opcode::CLV => instr::clv(self, implied),

                opcode::SEC => instr::sec(self, implied),
                opcode::SED => instr::sed(self, implied),
                opcode::SEI => instr::sei(self, implied),

                opcode::NOP => instr::nop(self, implied),

                _ => unimplemented!(), //format!("Unimplemented instruvtion code {:x}", ins)),
            }

            if self.got_irq {
                // handle IRQ, disregard BRK!
            } else if self.r.sr & status_flag::B != 0 {
                // Handle BRK
            }
        }
    }

    pub(crate) fn nmi(&mut self) {
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
    memory: [u8; 0xffff],
}

impl SimpleMemory {
    pub fn new() -> Self {
        SimpleMemory {
            memory: [0u8; 0xffff],
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
