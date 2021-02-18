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

    fn read_word(&mut self, regs: &mut Registers, addr: u16) -> u16 {
        self.read_byte(regs, addr) as u16 | (self.read_byte(regs, addr + 1) as u16) << 8
    }
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
        self.count += 2;
        self.mem.read_word(&mut self.r, addr)
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

    pub(crate) fn get_flag(&mut self, flag: u8) -> u8 {
        assert_ne!(flag, 0);
        assert_eq!(flag.count_ones(), 1);
        (self.r.sr >> flag.trailing_zeros()) & 1
    }

    pub fn execute(&mut self, mut count: isize) {
        #[allow(unused)]
        let mut ins_count = 0;

        while count > 0 {
            count -= 1;
            let ins = self.fetch_byte_with_pc();
            ins_count += 1;

            // eprintln!("instr 0x{:02x}, {}, {}", ins, self, ins_count);

            use adressing_mode::*;

            match ins {
                opcode::LDA_IM => instr::lda(self, AM::IM),
                opcode::LDA_ABS => instr::lda(self, AM::ABS),
                opcode::LDA_XIA => instr::lda(self, AM::XIA),
                opcode::LDA_YIA => instr::lda(self, AM::YIA),
                opcode::LDA_ZP => instr::lda(self, AM::ZP),
                opcode::LDA_XIZ => instr::lda(self, AM::XIZ),
                opcode::LDA_XIZI => instr::lda(self, AM::XIZI),
                opcode::LDA_ZIYI => instr::lda(self, AM::ZIYI),

                opcode::LDX_IM => instr::ldx(self, AM::IM),
                opcode::LDX_ABS => instr::ldx(self, AM::ABS),
                opcode::LDX_YIA => instr::ldx(self, AM::YIA),
                opcode::LDX_ZP => instr::ldx(self, AM::ZP),
                opcode::LDX_YIZ => instr::ldx(self, AM::YIZ),

                opcode::LDY_IM => instr::ldy(self, AM::IM),
                opcode::LDY_ABS => instr::ldy(self, AM::ABS),
                opcode::LDY_XIA => instr::ldy(self, AM::XIA),
                opcode::LDY_ZP => instr::ldy(self, AM::ZP),
                opcode::LDY_XIZ => instr::ldy(self, AM::XIZ),

                opcode::STA_ABS => instr::sta(self, AM::ABS),
                opcode::STA_XIA => instr::sta(self, AM::XIA),
                opcode::STA_YIA => instr::sta(self, AM::YIA),
                opcode::STA_ZP => instr::sta(self, AM::ZP),
                opcode::STA_XIZ => instr::sta(self, AM::XIZ),
                opcode::STA_XIZI => instr::sta(self, AM::XIZI),
                opcode::STA_ZIYI => instr::sta(self, AM::ZIYI),

                opcode::STX_ABS => instr::stx(self, AM::ABS),
                opcode::STX_ZP => instr::stx(self, AM::ZP),
                opcode::STX_YIZ => instr::stx(self, AM::YIZ),

                opcode::STY_ABS => instr::sty(self, AM::ABS),
                opcode::STY_ZP => instr::sty(self, AM::ZP),
                opcode::STY_XIZ => instr::sty(self, AM::XIZ),

                opcode::TAX => instr::tax(self, AM::IMPL),
                opcode::TAY => instr::tay(self, AM::IMPL),
                opcode::TSX => instr::tsx(self, AM::IMPL),
                opcode::TXA => instr::txa(self, AM::IMPL),
                opcode::TXS => instr::txs(self, AM::IMPL),
                opcode::TYA => instr::tya(self, AM::IMPL),

                opcode::PHA => instr::pha(self, AM::IMPL),
                opcode::PHP => instr::php(self, AM::IMPL),
                opcode::PLA => instr::pla(self, AM::IMPL),
                opcode::PLP => instr::plp(self, AM::IMPL),

                opcode::ASL_ACC => instr::asl(self, AM::ACC),
                opcode::ASL_ABS => instr::asl(self, AM::ABS),
                opcode::ASL_XIA => instr::asl(self, AM::XIA),
                opcode::ASL_ZP => instr::asl(self, AM::ZP),
                opcode::ASL_XIZ => instr::asl(self, AM::XIZ),

                opcode::LSR_ACC => instr::lsr(self, AM::ACC),
                opcode::LSR_ABS => instr::lsr(self, AM::ABS),
                opcode::LSR_XIA => instr::lsr(self, AM::XIA),
                opcode::LSR_ZP => instr::lsr(self, AM::ZP),
                opcode::LSR_XIZ => instr::lsr(self, AM::XIZ),

                opcode::ROL_ACC => instr::rol(self, AM::ACC),
                opcode::ROL_ABS => instr::rol(self, AM::ABS),
                opcode::ROL_XIA => instr::rol(self, AM::XIA),
                opcode::ROL_ZP => instr::rol(self, AM::ZP),
                opcode::ROL_XIZ => instr::rol(self, AM::XIZ),

                opcode::ROR_ACC => instr::ror(self, AM::ACC),
                opcode::ROR_ABS => instr::ror(self, AM::ABS),
                opcode::ROR_XIA => instr::ror(self, AM::XIA),
                opcode::ROR_ZP => instr::ror(self, AM::ZP),
                opcode::ROR_XIZ => instr::ror(self, AM::XIZ),

                opcode::AND_IM => instr::and(self, AM::IM),
                opcode::AND_ABS => instr::and(self, AM::ABS),
                opcode::AND_XIA => instr::and(self, AM::XIA),
                opcode::AND_YIA => instr::and(self, AM::YIA),
                opcode::AND_ZP => instr::and(self, AM::ZP),
                opcode::AND_XIZ => instr::and(self, AM::XIZ),
                opcode::AND_XIZI => instr::and(self, AM::XIZI),
                opcode::AND_ZIYI => instr::and(self, AM::ZIYI),

                opcode::BIT_ABS => instr::bit(self, AM::ABS),
                opcode::BIT_ZP => instr::bit(self, AM::ZP),

                opcode::EOR_IM => instr::eor(self, AM::IM),
                opcode::EOR_ABS => instr::eor(self, AM::ABS),
                opcode::EOR_XIA => instr::eor(self, AM::XIA),
                opcode::EOR_YIA => instr::eor(self, AM::YIA),
                opcode::EOR_ZP => instr::eor(self, AM::ZP),
                opcode::EOR_XIZ => instr::eor(self, AM::XIZ),
                opcode::EOR_XIZI => instr::eor(self, AM::XIZI),
                opcode::EOR_ZIYI => instr::eor(self, AM::ZIYI),

                opcode::ORA_IM => instr::ora(self, AM::IM),
                opcode::ORA_ABS => instr::ora(self, AM::ABS),
                opcode::ORA_XIA => instr::ora(self, AM::XIA),
                opcode::ORA_YIA => instr::ora(self, AM::YIA),
                opcode::ORA_ZP => instr::ora(self, AM::ZP),
                opcode::ORA_XIZ => instr::ora(self, AM::XIZ),
                opcode::ORA_XIZI => instr::ora(self, AM::XIZI),
                opcode::ORA_ZIYI => instr::ora(self, AM::ZIYI),

                opcode::ADC_IM => instr::adc(self, AM::IM),
                opcode::ADC_ABS => instr::adc(self, AM::ABS),
                opcode::ADC_XIA => instr::adc(self, AM::XIA),
                opcode::ADC_YIA => instr::adc(self, AM::YIA),
                opcode::ADC_ZP => instr::adc(self, AM::ZP),
                opcode::ADC_XIZ => instr::adc(self, AM::XIZ),
                opcode::ADC_XIZI => instr::adc(self, AM::XIZI),
                opcode::ADC_ZIYI => instr::adc(self, AM::ZIYI),

                opcode::CMP_IM => instr::cmp(self, AM::IM),
                opcode::CMP_ABS => instr::cmp(self, AM::ABS),
                opcode::CMP_XIA => instr::cmp(self, AM::XIA),
                opcode::CMP_YIA => instr::cmp(self, AM::YIA),
                opcode::CMP_ZP => instr::cmp(self, AM::ZP),
                opcode::CMP_XIZ => instr::cmp(self, AM::XIZ),
                opcode::CMP_XIZI => instr::cmp(self, AM::XIZI),
                opcode::CMP_ZIYI => instr::cmp(self, AM::ZIYI),

                opcode::CPX_IM => instr::cpx(self, AM::IM),
                opcode::CPX_ABS => instr::cpx(self, AM::ABS),
                opcode::CPX_ZP => instr::cpx(self, AM::ZP),

                opcode::CPY_IM => instr::cpy(self, AM::IM),
                opcode::CPY_ABS => instr::cpy(self, AM::ABS),
                opcode::CPY_ZP => instr::cpy(self, AM::ZP),

                opcode::SBC_IM => instr::sbc(self, AM::IM),
                opcode::SBC_ABS => instr::sbc(self, AM::ABS),
                opcode::SBC_XIA => instr::sbc(self, AM::XIA),
                opcode::SBC_YIA => instr::sbc(self, AM::YIA),
                opcode::SBC_ZP => instr::sbc(self, AM::ZP),
                opcode::SBC_XIZ => instr::sbc(self, AM::XIZ),
                opcode::SBC_XIZI => instr::sbc(self, AM::XIZI),
                opcode::SBC_ZIYI => instr::sbc(self, AM::ZIYI),

                opcode::DEC_ABS => instr::dec(self, AM::ABS),
                opcode::DEC_XIA => instr::dec(self, AM::XIA),
                opcode::DEC_ZP => instr::dec(self, AM::ZP),
                opcode::DEC_XIZ => instr::dec(self, AM::XIZ),
                opcode::DEX => instr::dex(self, AM::IMPL),
                opcode::DEY => instr::dey(self, AM::IMPL),

                opcode::INC_ABS => instr::inc(self, AM::ABS),
                opcode::INC_XIA => instr::inc(self, AM::XIA),
                opcode::INC_ZP => instr::inc(self, AM::ZP),
                opcode::INC_XIZ => instr::inc(self, AM::XIZ),
                opcode::INX => instr::inx(self, AM::IMPL),
                opcode::INY => instr::iny(self, AM::IMPL),

                opcode::BRK => instr::brk(self, AM::IMPL),
                opcode::JMP_ABS => instr::jmp(self, AM::ABS),
                opcode::JMP_ABSI => instr::jmp(self, AM::ABSI),
                opcode::JSR_ABS => instr::jsr(self, AM::ABS),
                opcode::RTI => instr::rti(self, AM::IMPL),
                opcode::RTS => instr::rts(self, AM::IMPL),

                opcode::BCC_REL => instr::bcc(self, AM::REL),
                opcode::BCS_REL => instr::bcs(self, AM::REL),
                opcode::BEQ_REL => instr::beq(self, AM::REL),
                opcode::BMI_REL => instr::bmi(self, AM::REL),
                opcode::BNE_REL => instr::bne(self, AM::REL),
                opcode::BPL_REL => instr::bpl(self, AM::REL),
                opcode::BVC_REL => instr::bvc(self, AM::REL),
                opcode::BVS_REL => instr::bvs(self, AM::REL),

                opcode::CLC => instr::clc(self, AM::IMPL),
                opcode::CLD => instr::cld(self, AM::IMPL),
                opcode::CLI => instr::cli(self, AM::IMPL),
                opcode::CLV => instr::clv(self, AM::IMPL),

                opcode::SEC => instr::sec(self, AM::IMPL),
                opcode::SED => instr::sed(self, AM::IMPL),
                opcode::SEI => instr::sei(self, AM::IMPL),

                opcode::NOP => instr::nop(self, AM::IMPL),

                /////// // Undocumented instructions, needed by apple1basic?
                /////// opcode::ISC_YIA => instr::isc(self, AddMode::YIA),
                /////// opcode::RRA_ABS => instr::rra(self, AddMode::ABS),
                /////// 0x7a | 0xda | 0x04 | 0x1c => instr::nop(self, AddMode::IMPL),
                /////// opcode::LAX_ZIYI => instr::lax(self, AddMode::ZIYI),
                // opcode::ISC_XIA => instr::isc(self, AM::XIA),

                // 0x02 | 0x12 | 0x22 | 0x32 | 0x42 | 0x52 | 0x62 | 0x72 | 0x92 | 0xb2 | 0xd2 | 0xf2 => {
                //     // JAM
                //     eprintln!("JAM! {}", self);
                //     break;
                // }

                _ => {
                    eprintln!("Unhandled instr 0x{:02x}, {}", ins, self);
                }
            }

            if self.got_irq {
                self.r.pc = self.read_word(0xfffe);
                self.got_irq = false;
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
