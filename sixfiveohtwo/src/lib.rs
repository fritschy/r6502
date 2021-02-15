use std::ops::{Index, IndexMut};
use std::fmt::{Display, Formatter};

type Byte = u8;
type Word = u16;

pub mod adressing_mode;
pub mod instr;
pub mod opcode;

pub trait Reset {
    fn reset(&mut self);
}

pub struct R6502 {
    pub pc: Word,

    pub sp: Byte,

    pub a: Byte,
    pub x: Byte,
    pub y: Byte,

    // Processor Status
    // CF : Carry Flag
    // Z: Zero FLag
    // ID: Interrupt Disable
    // DM: Decimal Mode
    // BC: Break Command
    // OF: Overflow Flag
    // NF: Negative Flag
    pub sr: Byte,

    // clock counter
    pub count: u64,

    // allow read and write to memory addresses to be hooked
    pub read_hook: Option<Box<dyn Fn(&mut R6502, &Memory, u16) -> Option<u8>>>,
    pub write_hook: Option<Box<dyn Fn(&mut R6502, &mut Memory, u16, u8) -> bool>>,
}

impl Display for R6502 {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "R6502: A:{:02x}, X:{:02x}, Y:{:02x}, SP:{:04x}, PC:{:04x}, SR:{:08b}", self.a, self.x, self.y, self.sp, self.pc, self.sr)
    }
}

impl Reset for R6502 {
    fn reset(&mut self) {
        self.pc = 0xfffc;
        self.sp = 0xff; // 0x100 - the 1 is implied
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sr = self.sr & !status_flag::D;
    }
}

fn page(addr: u16) -> u8 {
    (addr >> 8) as u8
}

impl R6502 {
    // We have 64KiB memory, of which a number of regions are special purpose:
    // 0x0000 - 0x00ff: "zero page"
    // 0x0100 - 0x01ff: "system stack"
    // 0xfffa - 0xfffb: NMI handler
    // 0xfffc - 0xfffd: power on reset location
    // 0xfffe - 0xffff: BRK/IRQ handler
    pub(crate) fn push(&mut self, mem: &mut Memory, v: Byte) {
        mem[0x0100 + self.sp as Word] = v;
        self.sp -= 1;
    }

    pub(crate) fn pop(&mut self, mem: &mut Memory) -> Byte {
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
            write_hook: None,
            read_hook: None,
        }
    }

    fn read_handler(&mut self, mem: &Memory, addr: u16) -> Option<u8> {
        None
    }

    fn write_handler(&mut self, mem: &mut Memory, addr: u16, val: u8) -> bool {
        false
    }

    pub(crate) fn fetch_byte_with_pc(&mut self, mem: &mut Memory) -> Byte {
        let b = self.fetch_byte_with_address(mem, self.pc);
        self.pc += 1;
        b
    }

    pub(crate) fn fetch_byte_with_address(&mut self, mem: &Memory, addr: u16) -> Byte {
        self.count += 1;
        if let Some(byte) = self.read_handler(mem, addr) {
            byte
        } else {
            mem[addr]
        }
    }

    pub(crate) fn write_byte_to_address(&mut self, mem: &mut Memory, addr: u16, value: u8) {
        self.count += 1;
        if let false = self.write_handler(mem, addr, value) {
            mem[addr] = value;
        }
    }

    pub(crate) fn set_flag(&mut self, flag: Byte, val: bool) {
        match val {
            true => self.sr |= flag,
            false => self.sr &= !(flag | status_flag::UNUSED),
        }
    }

    pub fn execute(&mut self, mem: &mut Memory, mut count: isize) {
        while count > 0 {
            count -= 1;
            let ins = self.fetch_byte_with_pc(mem);

            use adressing_mode::*;

            match ins {
                opcode::LDA_IM => instr::lda(self, mem, immediate),
                opcode::LDA_ABS => instr::lda(self, mem, absolute),
                opcode::LDA_XIA => instr::lda(self, mem, x_indexed_absolute),
                opcode::LDA_YIA => instr::lda(self, mem, y_indexed_absolute),
                opcode::LDA_ZP => instr::lda(self, mem, zero_page),
                opcode::LDA_XIZ => instr::lda(self, mem, x_indexed_zero_page),
                opcode::LDA_XIZI => instr::lda(self, mem, x_indexed_zero_page_indirect),
                opcode::LDA_ZIYI => instr::lda(self, mem, zero_page_indirect_y_indexed),

                opcode::LDX_IM => instr::ldx(self, mem, immediate),
                opcode::LDX_ABS => instr::ldx(self, mem, absolute),
                opcode::LDX_YIA => instr::ldx(self, mem, y_indexed_absolute),
                opcode::LDX_ZP => instr::ldx(self, mem, zero_page),
                opcode::LDX_YIZ => instr::ldx(self, mem, y_indexed_zero_page),

                opcode::LDY_IM => instr::ldy(self, mem, immediate),
                opcode::LDY_ABS => instr::ldy(self, mem, absolute),
                opcode::LDY_XIA => instr::ldy(self, mem, x_indexed_absolute),
                opcode::LDY_ZP => instr::ldy(self, mem, zero_page),
                opcode::LDY_XIZ => instr::ldy(self, mem, x_indexed_zero_page),

                opcode::STA_ABS => instr::sta(self, mem, absolute),
                opcode::STA_XIA => instr::sta(self, mem, x_indexed_absolute),
                opcode::STA_YIA => instr::sta(self, mem, y_indexed_absolute),
                opcode::STA_ZP => instr::sta(self, mem, zero_page),
                opcode::STA_XIZ => instr::sta(self, mem, x_indexed_zero_page),
                opcode::STA_XIZI => instr::sta(self, mem, x_indexed_zero_page_indirect),
                opcode::STA_ZIYI => instr::sta(self, mem, zero_page_indirect_y_indexed),

                opcode::STX_ABS => instr::stx(self, mem, absolute),
                opcode::STX_ZP => instr::stx(self, mem, zero_page),
                opcode::STX_YIZ => instr::stx(self, mem, y_indexed_zero_page),

                opcode::STY_ABS => instr::sty(self, mem, absolute),
                opcode::STY_ZP => instr::sty(self, mem, zero_page),
                opcode::STY_XIZ => instr::sty(self, mem, x_indexed_zero_page),

                opcode::TAX => instr::tax(self, mem, implied),
                opcode::TAY => instr::tay(self, mem, implied),
                opcode::TSX => instr::tsx(self, mem, implied),
                opcode::TXA => instr::txa(self, mem, implied),
                opcode::TXS => instr::txs(self, mem, implied),
                opcode::TYA => instr::tya(self, mem, implied),

                opcode::PHA => instr::pha(self, mem, implied),
                opcode::PHP => instr::php(self, mem, implied),
                opcode::PLA => instr::pla(self, mem, implied),
                opcode::PLP => instr::plp(self, mem, implied),

                opcode::ASLA => instr::asl(self, mem, absolute),
                opcode::ASL_ABS => instr::asl(self, mem, accumulator),
                opcode::ASL_XIA => instr::asl(self, mem, x_indexed_absolute),
                opcode::ASL_ZP => instr::asl(self, mem, zero_page),
                opcode::ASL_XIZ => instr::asl(self, mem, x_indexed_zero_page),

                opcode::LSRA => instr::lsr(self, mem, absolute),
                opcode::LSR_ABS => instr::lsr(self, mem, accumulator),
                opcode::LSR_XIA => instr::lsr(self, mem, x_indexed_absolute),
                opcode::LSR_ZP => instr::lsr(self, mem, zero_page),
                opcode::LSR_XIZ => instr::lsr(self, mem, x_indexed_zero_page),

                opcode::ROLA => instr::rol(self, mem, accumulator),
                opcode::ROL_ABS => instr::rol(self, mem, absolute),
                opcode::ROL_XIA => instr::rol(self, mem, x_indexed_absolute),
                opcode::ROL_ZP => instr::rol(self, mem, zero_page),
                opcode::ROL_XIZ => instr::rol(self, mem, x_indexed_zero_page),

                opcode::RORA => instr::ror(self, mem, accumulator),
                opcode::ROR_ABS => instr::ror(self, mem, absolute),
                opcode::ROR_XIA => instr::ror(self, mem, x_indexed_absolute),
                opcode::ROR_ZP => instr::ror(self, mem, zero_page),
                opcode::ROR_XIZ => instr::ror(self, mem, x_indexed_zero_page),

                opcode::AND_IM => instr::and(self, mem, immediate),
                opcode::AND_ABS => instr::and(self, mem, absolute),
                opcode::AND_XIA => instr::and(self, mem, x_indexed_absolute),
                opcode::AND_YIA => instr::and(self, mem, y_indexed_absolute),
                opcode::AND_ZP => instr::and(self, mem, zero_page),
                opcode::AND_XIZ => instr::and(self, mem, x_indexed_zero_page),
                opcode::AND_XIZI => instr::and(self, mem, x_indexed_zero_page_indirect),
                opcode::AND_ZIYI => instr::and(self, mem, zero_page_indirect_y_indexed),

                opcode::BIT_ABS => instr::bit(self, mem, absolute),
                opcode::BIT_ZP => instr::bit(self, mem, zero_page),

                opcode::EOR_IM => instr::eor(self, mem, immediate),
                opcode::EOR_ABS => instr::eor(self, mem, absolute),
                opcode::EOR_XIA => instr::eor(self, mem, x_indexed_absolute),
                opcode::EOR_YIA => instr::eor(self, mem, y_indexed_absolute),
                opcode::EOR_ZP => instr::eor(self, mem, zero_page),
                opcode::EOR_XIZ => instr::eor(self, mem, x_indexed_zero_page),
                opcode::EOR_XIZI => instr::eor(self, mem, x_indexed_zero_page_indirect),
                opcode::EOR_ZIYI => instr::eor(self, mem, zero_page_indirect_y_indexed),

                opcode::ORA_IM => instr::ora(self, mem, immediate),
                opcode::ORA_ABS => instr::ora(self, mem, absolute),
                opcode::ORA_XIA => instr::ora(self, mem, x_indexed_absolute),
                opcode::ORA_YIA => instr::ora(self, mem, y_indexed_absolute),
                opcode::ORA_ZP => instr::ora(self, mem, zero_page),
                opcode::ORA_XIZ => instr::ora(self, mem, x_indexed_zero_page),
                opcode::ORA_XIZI => instr::ora(self, mem, x_indexed_zero_page_indirect),
                opcode::ORA_ZIYI => instr::ora(self, mem, zero_page_indirect_y_indexed),

                opcode::ADC_IM => instr::adc(self, mem, immediate),
                opcode::ADC_ABS => instr::adc(self, mem, absolute),
                opcode::ADC_XIA => instr::adc(self, mem, x_indexed_absolute),
                opcode::ADC_YIA => instr::adc(self, mem, y_indexed_absolute),
                opcode::ADC_ZP => instr::adc(self, mem, zero_page),
                opcode::ADC_XIZ => instr::adc(self, mem, x_indexed_zero_page),
                opcode::ADC_XIZI => instr::adc(self, mem, x_indexed_zero_page_indirect),
                opcode::ADC_ZIYI => instr::adc(self, mem, zero_page_indirect_y_indexed),

                opcode::CMP_IM => instr::cmp(self, mem, immediate),
                opcode::CMP_ABS => instr::cmp(self, mem, absolute),
                opcode::CMP_XIA => instr::cmp(self, mem, x_indexed_absolute),
                opcode::CMP_YIA => instr::cmp(self, mem, y_indexed_absolute),
                opcode::CMP_ZP => instr::cmp(self, mem, zero_page),
                opcode::CMP_XIZ => instr::cmp(self, mem, x_indexed_zero_page),
                opcode::CMP_XIZI => instr::cmp(self, mem, x_indexed_zero_page_indirect),
                opcode::CMP_ZIYI => instr::cmp(self, mem, zero_page_indirect_y_indexed),

                opcode::CPX_IM => instr::cpx(self, mem, immediate),
                opcode::CPX_ABS => instr::cpx(self, mem, absolute),
                opcode::CPX_ZP => instr::cpx(self, mem, zero_page),

                opcode::CPY_IM => instr::cpy(self, mem, immediate),
                opcode::CPY_ABS => instr::cpy(self, mem, absolute),
                opcode::CPY_ZP => instr::cpy(self, mem, zero_page),

                opcode::SBC_IM => instr::sbc(self, mem, immediate),
                opcode::SBC_ABS => instr::sbc(self, mem, absolute),
                opcode::SBC_XIA => instr::sbc(self, mem, x_indexed_absolute),
                opcode::SBC_YIA => instr::sbc(self, mem, y_indexed_absolute),
                opcode::SBC_ZP => instr::sbc(self, mem, zero_page),
                opcode::SBC_XIZ => instr::sbc(self, mem, x_indexed_zero_page),
                opcode::SBC_XIZI => instr::sbc(self, mem, x_indexed_zero_page_indirect),
                opcode::SBC_ZIYI => instr::sbc(self, mem, zero_page_indirect_y_indexed),

                opcode::DEC_ABS => instr::dec(self, mem, absolute),
                opcode::DEC_XIA => instr::dec(self, mem, x_indexed_absolute),
                opcode::DEC_ZP => instr::dec(self, mem, zero_page),
                opcode::DEC_XIZ => instr::dec(self, mem, x_indexed_zero_page),
                opcode::DEX => instr::dex(self, mem, implied),
                opcode::DEY => instr::dey(self, mem, implied),

                opcode::INC_ABS => instr::inc(self, mem, absolute),
                opcode::INC_XIA => instr::inc(self, mem, x_indexed_absolute),
                opcode::INC_ZP => instr::inc(self, mem, zero_page),
                opcode::INC_XIZ => instr::inc(self, mem, x_indexed_zero_page),
                opcode::INX => instr::inx(self, mem, implied),
                opcode::INY => instr::iny(self, mem, implied),

                opcode::BRK => instr::brk(self, mem, implied),
                opcode::JMP_ABS => instr::jmp(self, mem, absolute),
                opcode::JMP_ABSI => instr::jmp(self, mem, absolute_indirect),
                opcode::JSR_ABS => instr::jsr(self, mem, absolute),
                opcode::RTI => instr::rti(self, mem, implied),
                opcode::RTS => instr::rts(self, mem, implied),

                opcode::BCC_REL => instr::bcc(self, mem, relative),
                opcode::BCS_REL => instr::bcs(self, mem, relative),
                opcode::BEQ_REL => instr::beq(self, mem, relative),
                opcode::BMI_REL => instr::bmi(self, mem, relative),
                opcode::BNE_REL => instr::bne(self, mem, relative),
                opcode::BPL_REL => instr::bpl(self, mem, relative),
                opcode::BVC_REL => instr::bvc(self, mem, relative),
                opcode::BVS_REL => instr::bvs(self, mem, relative),

                opcode::CLC => instr::clc(self, mem, implied),
                opcode::CLD => instr::cld(self, mem, implied),
                opcode::CLI => instr::cli(self, mem, implied),
                opcode::CLV => instr::clv(self, mem, implied),

                opcode::SEC => instr::sec(self, mem, implied),
                opcode::SED => instr::sed(self, mem, implied),
                opcode::SEI => instr::sei(self, mem, implied),

                opcode::NOP => instr::nop(self, mem, implied),

                _ => unimplemented!(), //format!("Unimplemented instruvtion code {:x}", ins)),
            }
        }
    }

    pub(crate) fn nmi(&mut self) {
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
    pub fn new() -> Self {
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
