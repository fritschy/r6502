use std::ops::{Index, IndexMut};
use stopwatch::Stopwatch;

type Byte = u8;
type Word = u16;

mod instr;
mod opcode;
mod adressing_mode;

trait Reset {
    fn reset(&mut self);
}

#[derive(Debug)]
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

    fn execute(&mut self, mem: &mut Memory, mut count: isize) {
        while count > 0 {
            count -= 1;
            let ins = self.fetch_byte_with_pc(mem);

            use adressing_mode::*;

            match ins {
                opcode::LDA_IM => instr::instr_lda(self, mem, immediate),
                opcode::LDA_ABS => instr::instr_lda(self, mem, absolute),
                opcode::LDA_XIA => instr::instr_lda(self, mem, x_indexed_absolute),
                opcode::LDA_YIA => instr::instr_lda(self, mem, y_indexed_absolute),
                opcode::LDA_ZP => instr::instr_lda(self, mem, zero_page),
                opcode::LDA_XIZ => instr::instr_lda(self, mem, x_indexed_zero_page),
                opcode::LDA_XIZI => instr::instr_lda(self, mem, x_indexed_zero_page_indirect),
                opcode::LDA_ZIYI => instr::instr_lda(self, mem, zero_page_indirect_y_indexed),

                opcode::LDX_IM => instr::instr_ldx(self, mem, immediate),
                opcode::LDX_ABS => instr::instr_ldx(self, mem, absolute),
                opcode::LDX_YIA => instr::instr_ldx(self, mem, y_indexed_absolute),
                opcode::LDX_ZP => instr::instr_ldx(self, mem, zero_page),
                opcode::LDX_YIZ => instr::instr_ldx(self, mem, y_indexed_zero_page),

                opcode::LDY_IM => instr::instr_ldy(self, mem, immediate),
                opcode::LDY_ABS => instr::instr_ldy(self, mem, absolute),
                opcode::LDY_XIA => instr::instr_ldy(self, mem, x_indexed_absolute),
                opcode::LDY_ZP => instr::instr_ldy(self, mem, zero_page),
                opcode::LDY_XIZ => instr::instr_ldy(self, mem, x_indexed_zero_page),

                opcode::STA_ABS => instr::instr_sta(self, mem, absolute),
                opcode::STA_XIA => instr::instr_sta(self, mem, x_indexed_absolute),
                opcode::STA_YIA => instr::instr_sta(self, mem, y_indexed_absolute),
                opcode::STA_ZP => instr::instr_sta(self, mem, zero_page),
                opcode::STA_XIZ => instr::instr_sta(self, mem, x_indexed_zero_page),
                opcode::STA_XIZI => instr::instr_sta(self, mem, x_indexed_zero_page_indirect),
                opcode::STA_ZIYI => instr::instr_sta(self, mem, zero_page_indirect_y_indexed),

                opcode::STX_ABS => instr::instr_stx(self, mem, absolute),
                opcode::STX_ZP => instr::instr_stx(self, mem, zero_page),
                opcode::STX_YIZ => instr::instr_stx(self, mem, y_indexed_zero_page),

                opcode::STY_ABS => instr::instr_sty(self, mem, absolute),
                opcode::STY_ZP => instr::instr_sty(self, mem, zero_page),
                opcode::STY_XIZ => instr::instr_sty(self, mem, x_indexed_zero_page),

                opcode::TAX => instr::instr_tax(self, mem, implied),
                opcode::TAY => instr::instr_tay(self, mem, implied),
                opcode::TSX => instr::instr_tsx(self, mem, implied),
                opcode::TXA => instr::instr_txa(self, mem, implied),
                opcode::TXS => instr::instr_txs(self, mem, implied),
                opcode::TYA => instr::instr_tya(self, mem, implied),

                opcode::PHA => instr::instr_pha(self, mem, implied),
                opcode::PHP => instr::instr_php(self, mem, implied),
                opcode::PLA => instr::instr_pla(self, mem, implied),
                opcode::PLP => instr::instr_plp(self, mem, implied),

                opcode::ASLA => instr::instr_asl(self, mem, absolute),
                opcode::ASL_ABS => instr::instr_asl(self, mem, accumulator),
                opcode::ASL_XIA => instr::instr_asl(self, mem, x_indexed_absolute),
                opcode::ASL_ZP => instr::instr_asl(self, mem, zero_page),
                opcode::ASL_XIZ => instr::instr_asl(self, mem, x_indexed_zero_page),

                opcode::LSRA => instr::instr_lsr(self, mem, absolute),
                opcode::LSR_ABS => instr::instr_lsr(self, mem, accumulator),
                opcode::LSR_XIA => instr::instr_lsr(self, mem, x_indexed_absolute),
                opcode::LSR_ZP => instr::instr_lsr(self, mem, zero_page),
                opcode::LSR_XIZ => instr::instr_lsr(self, mem, x_indexed_zero_page),

                opcode::ROLA => instr::instr_rol(self, mem, accumulator),
                opcode::ROL_ABS => instr::instr_rol(self, mem, absolute),
                opcode::ROL_XIA => instr::instr_rol(self, mem, x_indexed_absolute),
                opcode::ROL_ZP => instr::instr_rol(self, mem, zero_page),
                opcode::ROL_XIZ => instr::instr_rol(self, mem, x_indexed_zero_page),

                opcode::RORA => instr::instr_ror(self, mem, accumulator),
                opcode::ROR_ABS => instr::instr_ror(self, mem, absolute),
                opcode::ROR_XIA => instr::instr_ror(self, mem, x_indexed_absolute),
                opcode::ROR_ZP => instr::instr_ror(self, mem, zero_page),
                opcode::ROR_XIZ => instr::instr_ror(self, mem, x_indexed_zero_page),

                opcode::AND_IM => instr::instr_and(self, mem, immediate),
                opcode::AND_ABS => instr::instr_and(self, mem, absolute),
                opcode::AND_XIA => instr::instr_and(self, mem, x_indexed_absolute),
                opcode::AND_YIA => instr::instr_and(self, mem, y_indexed_absolute),
                opcode::AND_ZP => instr::instr_and(self, mem, zero_page),
                opcode::AND_XIZ => instr::instr_and(self, mem, x_indexed_zero_page),
                opcode::AND_XIZI => instr::instr_and(self, mem, x_indexed_zero_page_indirect),
                opcode::AND_ZIYI => instr::instr_and(self, mem, zero_page_indirect_y_indexed),

                opcode::BIT_ABS => instr::instr_bit(self, mem, absolute),
                opcode::BIT_ZP => instr::instr_bit(self, mem, zero_page),

                opcode::EOR_IM => instr::instr_eor(self, mem, immediate),
                opcode::EOR_ABS => instr::instr_eor(self, mem, absolute),
                opcode::EOR_XIA => instr::instr_eor(self, mem, x_indexed_absolute),
                opcode::EOR_YIA => instr::instr_eor(self, mem, y_indexed_absolute),
                opcode::EOR_ZP => instr::instr_eor(self, mem, zero_page),
                opcode::EOR_XIZ => instr::instr_eor(self, mem, x_indexed_zero_page),
                opcode::EOR_XIZI => instr::instr_eor(self, mem, x_indexed_zero_page_indirect),
                opcode::EOR_ZIYI => instr::instr_eor(self, mem, zero_page_indirect_y_indexed),

                opcode::ORA_IM => instr::instr_ora(self, mem, immediate),
                opcode::ORA_ABS => instr::instr_ora(self, mem, absolute),
                opcode::ORA_XIA => instr::instr_ora(self, mem, x_indexed_absolute),
                opcode::ORA_YIA => instr::instr_ora(self, mem, y_indexed_absolute),
                opcode::ORA_ZP => instr::instr_ora(self, mem, zero_page),
                opcode::ORA_XIZ => instr::instr_ora(self, mem, x_indexed_zero_page),
                opcode::ORA_XIZI => instr::instr_ora(self, mem, x_indexed_zero_page_indirect),
                opcode::ORA_ZIYI => instr::instr_ora(self, mem, zero_page_indirect_y_indexed),

                opcode::ADC_IM => instr::instr_adc(self, mem, immediate),
                opcode::ADC_ABS => instr::instr_adc(self, mem, absolute),
                opcode::ADC_XIA => instr::instr_adc(self, mem, x_indexed_absolute),
                opcode::ADC_YIA => instr::instr_adc(self, mem, y_indexed_absolute),
                opcode::ADC_ZP => instr::instr_adc(self, mem, zero_page),
                opcode::ADC_XIZ => instr::instr_adc(self, mem, x_indexed_zero_page),
                opcode::ADC_XIZI => instr::instr_adc(self, mem, x_indexed_zero_page_indirect),
                opcode::ADC_ZIYI => instr::instr_adc(self, mem, zero_page_indirect_y_indexed),

                opcode::CMP_IM => instr::instr_cmp(self, mem, immediate),
                opcode::CMP_ABS => instr::instr_cmp(self, mem, absolute),
                opcode::CMP_XIA => instr::instr_cmp(self, mem, x_indexed_absolute),
                opcode::CMP_YIA => instr::instr_cmp(self, mem, y_indexed_absolute),
                opcode::CMP_ZP => instr::instr_cmp(self, mem, zero_page),
                opcode::CMP_XIZ => instr::instr_cmp(self, mem, x_indexed_zero_page),
                opcode::CMP_XIZI => instr::instr_cmp(self, mem, x_indexed_zero_page_indirect),
                opcode::CMP_ZIYI => instr::instr_cmp(self, mem, zero_page_indirect_y_indexed),

                opcode::CPX_IM => instr::instr_cpx(self, mem, immediate),
                opcode::CPX_ABS => instr::instr_cpx(self, mem, absolute),
                opcode::CPX_ZP => instr::instr_cpx(self, mem, zero_page),

                opcode::CPY_IM => instr::instr_cpy(self, mem, immediate),
                opcode::CPY_ABS => instr::instr_cpy(self, mem, absolute),
                opcode::CPY_ZP => instr::instr_cpy(self, mem, zero_page),

                opcode::SBC_IM => instr::instr_sbc(self, mem, immediate),
                opcode::SBC_ABS => instr::instr_sbc(self, mem, absolute),
                opcode::SBC_XIA => instr::instr_sbc(self, mem, x_indexed_absolute),
                opcode::SBC_YIA => instr::instr_sbc(self, mem, y_indexed_absolute),
                opcode::SBC_ZP => instr::instr_sbc(self, mem, zero_page),
                opcode::SBC_XIZ => instr::instr_sbc(self, mem, x_indexed_zero_page),
                opcode::SBC_XIZI => instr::instr_sbc(self, mem, x_indexed_zero_page_indirect),
                opcode::SBC_ZIYI => instr::instr_sbc(self, mem, zero_page_indirect_y_indexed),

                opcode::DEC_ABS => instr::instr_dec(self, mem, absolute),
                opcode::DEC_XIA => instr::instr_dec(self, mem, x_indexed_absolute),
                opcode::DEC_ZP => instr::instr_dec(self, mem, zero_page),
                opcode::DEC_XIZ => instr::instr_dec(self, mem, x_indexed_zero_page),
                opcode::DEX => instr::instr_dex(self, mem, implied),
                opcode::DEY => instr::instr_dey(self, mem, implied),

                opcode::INC_ABS => instr::instr_inc(self, mem, absolute),
                opcode::INC_XIA => instr::instr_inc(self, mem, x_indexed_absolute),
                opcode::INC_ZP => instr::instr_inc(self, mem, zero_page),
                opcode::INC_XIZ => instr::instr_inc(self, mem, x_indexed_zero_page),
                opcode::INX => instr::instr_inx(self, mem, implied),
                opcode::INY => instr::instr_iny(self, mem, implied),

                opcode::BRK => instr::instr_brk(self, mem, implied),
                opcode::JMP_ABS => instr::instr_jmp(self, mem, absolute),
                opcode::JMP_ABSI => instr::instr_jmp(self, mem, absolute_indirect),
                opcode::JSR_ABS => instr::instr_jsr(self, mem, absolute),
                opcode::RTI => instr::instr_rti(self, mem, implied),
                opcode::RTS => instr::instr_rts(self, mem, implied),

                opcode::BCC_REL => instr::instr_bcc(self, mem, relative),
                opcode::BCS_REL => instr::instr_bcs(self, mem, relative),
                opcode::BEQ_REL => instr::instr_beq(self, mem, relative),
                opcode::BMI_REL => instr::instr_bmi(self, mem, relative),
                opcode::BNE_REL => instr::instr_bne(self, mem, relative),
                opcode::BPL_REL => instr::instr_bpl(self, mem, relative),
                opcode::BVC_REL => instr::instr_bvc(self, mem, relative),
                opcode::BVS_REL => instr::instr_bvs(self, mem, relative),

                opcode::CLC => instr::instr_clc(self, mem, implied),
                opcode::CLD => instr::instr_cld(self, mem, implied),
                opcode::CLI => instr::instr_cli(self, mem, implied),
                opcode::CLV => instr::instr_clv(self, mem, implied),

                opcode::SEC => instr::instr_sec(self, mem, implied),
                opcode::SED => instr::instr_sed(self, mem, implied),
                opcode::SEI => instr::instr_sei(self, mem, implied),

                opcode::NOP => instr::instr_nop(self, mem, implied),

                _ => unimplemented!(), //format!("Unimplemented instruvtion code {:x}", ins)),
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

    let cycles = 100000000;
    let s = Stopwatch::start_new();
    cpu.execute(&mut mem, cycles);
    let s = s.elapsed_ms();

    println!("Executed {} instructions in {}ms", cycles, s);

    println!("{:#?}", cpu);
    println!("0x2000: {:b}", mem[0x2000]);
    println!("0x2001: {:b}", mem[0x2001]);
}
