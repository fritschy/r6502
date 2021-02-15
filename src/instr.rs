use crate::{Byte, Word};
use crate::{R6502, Memory};

// An instr is composed of
// * "base" instruction
// * addressing mode
// * operand (if any)

pub const BRK: u8 = 0x00;
pub const ORA_XIZI: u8 = 0x01;
pub const ORA_ZP: u8 = 0x05;
pub const ASL_ZP: u8 = 0x06;
pub const PHP: u8 = 0x08;
pub const ORA_IM: u8 = 0x09;
pub const ASLA: u8 = 0x0A;
pub const ORA_ABS: u8 = 0x0D;
pub const ASL_ABS: u8 = 0x0E;
pub const BPL_REL: u8 = 0x10;
pub const ORA_ZIYI: u8 = 0x11;
pub const ORA_XIZ: u8 = 0x15;
pub const ASL_XIZ: u8 = 0x16;
pub const CLC: u8 = 0x18;
pub const ORA_YIA: u8 = 0x19;
pub const ORA_XIA: u8 = 0x1D;
pub const ASL_XIA: u8 = 0x1E;
pub const JSR_ABS: u8 = 0x20;
pub const AND_XIZI: u8 = 0x21;
pub const BIT_ZP: u8 = 0x24;
pub const AND_ZP: u8 = 0x25;
pub const ROL_ZP: u8 = 0x26;
pub const PLP: u8 = 0x28;
pub const AND_IM: u8 = 0x29;
pub const ROLA: u8 = 0x2A;
pub const BIT_ABS: u8 = 0x2C;
pub const AND_ABS: u8 = 0x2D;
pub const ROL_ABS: u8 = 0x2E;
pub const BMI_REL: u8 = 0x30;
pub const AND_ZIYI: u8 = 0x31;
pub const AND_XIZ: u8 = 0x35;
pub const ROL_XIZ: u8 = 0x36;
pub const SEC: u8 = 0x38;
pub const AND_YIA: u8 = 0x39;
pub const AND_XIA: u8 = 0x3D;
pub const ROL_XIA: u8 = 0x3E;
pub const RTI: u8 = 0x40;
pub const EOR_XIZI: u8 = 0x41;
pub const EOR_ZP: u8 = 0x45;
pub const LSR_ZP: u8 = 0x46;
pub const PHA: u8 = 0x48;
pub const EOR_IM: u8 = 0x49;
pub const LSRA: u8 = 0x4A;
pub const JMP_ABS: u8 = 0x4C;
pub const EOR_ABS: u8 = 0x4D;
pub const LSR_ABS: u8 = 0x4E;
pub const BVC_REL: u8 = 0x50;
pub const EOR_ZIYI: u8 = 0x51;
pub const EOR_XIZ: u8 = 0x55;
pub const LSR_XIZ: u8 = 0x56;
pub const CLI: u8 = 0x58;
pub const EOR_YIA: u8 = 0x59;
pub const EOR_XIA: u8 = 0x5D;
pub const LSR_XIA: u8 = 0x5E;
pub const RTS: u8 = 0x60;
pub const ADC_XIZI: u8 = 0x61;
pub const ADC_ZP: u8 = 0x65;
pub const ROR_ZP: u8 = 0x66;
pub const PLA: u8 = 0x68;
pub const ADC_IM: u8 = 0x69;
pub const RORA: u8 = 0x6A;
pub const JMP_ABSI: u8 = 0x6C;
pub const ADC_ABS: u8 = 0x6D;
pub const ROR_ABS: u8 = 0x6E;
pub const BVS_REL: u8 = 0x70;
pub const ADC_ZIYI: u8 = 0x71;
pub const ADC_XIZ: u8 = 0x75;
pub const ROR_XIZ: u8 = 0x76;
pub const SEI: u8 = 0x78;
pub const ADC_YIA: u8 = 0x79;
pub const ADC_XIA: u8 = 0x7D;
pub const ROR_XIA: u8 = 0x7E;
pub const STA_XIZI: u8 = 0x81;
pub const STY_ZP: u8 = 0x84;
pub const STA_ZP: u8 = 0x85;
pub const STX_ZP: u8 = 0x86;
pub const DEY: u8 = 0x88;
pub const TXA: u8 = 0x8A;
pub const STY_ABS: u8 = 0x8C;
pub const STA_ABS: u8 = 0x8D;
pub const STX_ABS: u8 = 0x8E;
pub const BCC_REL: u8 = 0x90;
pub const STA_ZIYI: u8 = 0x91;
pub const STY_XIZ: u8 = 0x94;
pub const STA_XIZ: u8 = 0x95;
pub const STX_YIZ: u8 = 0x96;
pub const TYA: u8 = 0x98;
pub const STA_YIA: u8 = 0x99;
pub const TXS: u8 = 0x9A;
pub const STA_XIA: u8 = 0x9D;
pub const LDY_IM: u8 = 0xA0;
pub const LDA_XIZI: u8 = 0xA1;
pub const LDX_IM: u8 = 0xA2;
pub const LDY_ZP: u8 = 0xA4;
pub const LDA_ZP: u8 = 0xA5;
pub const LDX_ZP: u8 = 0xA6;
pub const TAY: u8 = 0xA8;
pub const LDA_IM: u8 = 0xA9;
pub const TAX: u8 = 0xAA;
pub const LDY_ABS: u8 = 0xAC;
pub const LDA_ABS: u8 = 0xAD;
pub const LDX_ABS: u8 = 0xAE;
pub const BCS_REL: u8 = 0xB0;
pub const LDA_ZIYI: u8 = 0xB1;
pub const LDY_XIZ: u8 = 0xB4;
pub const LDA_XIZ: u8 = 0xB5;
pub const LDX_YIZ: u8 = 0xB6;
pub const CLV: u8 = 0xB8;
pub const LDA_YIA: u8 = 0xB9;
pub const TSX: u8 = 0xBA;
pub const LDY_XIA: u8 = 0xBC;
pub const LDA_XIA: u8 = 0xBD;
pub const LDX_YIA: u8 = 0xBE;
pub const CPY_IM: u8 = 0xC0;
pub const CMP_XIZI: u8 = 0xC1;
pub const CPY_ZP: u8 = 0xC4;
pub const CMP_ZP: u8 = 0xC5;
pub const DEC_ZP: u8 = 0xC6;
pub const INY: u8 = 0xC8;
pub const CMP_IM: u8 = 0xC9;
pub const DEX: u8 = 0xCA;
pub const CPY_ABS: u8 = 0xCC;
pub const CMP_ABS: u8 = 0xCD;
pub const DEC_ABS: u8 = 0xCE;
pub const BNE_REL: u8 = 0xD0;
pub const CMP_ZIYI: u8 = 0xD1;
pub const CMP_XIZ: u8 = 0xD5;
pub const DEC_XIZ: u8 = 0xD6;
pub const CLD: u8 = 0xD8;
pub const CMP_YIA: u8 = 0xD9;
pub const CMP_XIA: u8 = 0xDD;
pub const DEC_XIA: u8 = 0xDE;
pub const CPX_IM: u8 = 0xE0;
pub const SBC_XIZI: u8 = 0xE1;
pub const CPX_ZP: u8 = 0xE4;
pub const SBC_ZP: u8 = 0xE5;
pub const INC_ZP: u8 = 0xE6;
pub const INX: u8 = 0xE8;
pub const SBC_IM: u8 = 0xE9;
pub const NOP: u8 = 0xEA;
pub const CPX_ABS: u8 = 0xEC;
pub const SBC_ABS: u8 = 0xED;
pub const INC_ABS: u8 = 0xEE;
pub const BEQ_REL: u8 = 0xF0;
pub const SBC_ZIYI: u8 = 0xF1;
pub const SBC_XIZ: u8 = 0xF5;
pub const INC_XIZ: u8 = 0xF6;
pub const SED: u8 = 0xF8;
pub const SBC_YIA: u8 = 0xF9;
pub const SBC_XIA: u8 = 0xFD;
pub const INC_XIA: u8 = 0xFE;

use crate::status_flag;

pub fn clock(cpu: &mut R6502, mem: &mut Memory) {
    loop {
        let cur = cpu.fetch_byte_with_pc(mem);

        match cur {
            0x00 => {  // BRK impl
                cpu.push(mem, mem[cpu.pc + 2]);
                cpu.push(mem, cpu.sr);
                cpu.sr |= status_flag::I;
                cpu.count += 7;
            }

            0x01 => {  // ORA X,ind
                let a = cpu.a;
                let x = cpu.x;
            }

            // the following are invalid
            0x02 => { unimplemented!() }
            0x03 => { unimplemented!() }
            0x04 => { unimplemented!() }

            0x05 => {  // ORA zpg
                let p = cpu.fetch_byte_with_pc(mem);
                let a = cpu.a | p;
                if (a as i8) < 0 {
                    cpu.sr |= status_flag::N;
                } else if a == 0 {
                    cpu.sr |= status_flag::Z;
                }
                cpu.a = a;
                unimplemented!()
            }
            0x06 => { unimplemented!() }
            0x07 => { unimplemented!() }
            0x08 => { unimplemented!() }
            0x09 => { unimplemented!() }
            0x0a => { unimplemented!() }
            0x0b => { unimplemented!() }
            0x0c => { unimplemented!() }
            0x0d => { unimplemented!() }
            0x0e => { unimplemented!() }
            0x0f => { unimplemented!() }
            0x10 => { unimplemented!() }
            0x11 => { unimplemented!() }
            0x12 => { unimplemented!() }
            0x13 => { unimplemented!() }
            0x14 => { unimplemented!() }
            0x15 => { unimplemented!() }
            0x16 => { unimplemented!() }
            0x17 => { unimplemented!() }
            0x18 => { unimplemented!() }
            0x19 => { unimplemented!() }
            0x1a => { unimplemented!() }
            0x1b => { unimplemented!() }
            0x1c => { unimplemented!() }
            0x1d => { unimplemented!() }
            0x1e => { unimplemented!() }
            0x1f => { unimplemented!() }
            0x20 => { unimplemented!() }
            0x21 => { unimplemented!() }
            0x22 => { unimplemented!() }
            0x23 => { unimplemented!() }
            0x24 => { unimplemented!() }
            0x25 => { unimplemented!() }
            0x26 => { unimplemented!() }
            0x27 => { unimplemented!() }
            0x28 => { unimplemented!() }
            0x29 => { unimplemented!() }
            0x2a => { unimplemented!() }
            0x2b => { unimplemented!() }
            0x2c => { unimplemented!() }
            0x2d => { unimplemented!() }
            0x2e => { unimplemented!() }
            0x2f => { unimplemented!() }
            0x30 => { unimplemented!() }
            0x31 => { unimplemented!() }
            0x32 => { unimplemented!() }
            0x33 => { unimplemented!() }
            0x34 => { unimplemented!() }
            0x35 => { unimplemented!() }
            0x36 => { unimplemented!() }
            0x37 => { unimplemented!() }
            0x38 => { unimplemented!() }
            0x39 => { unimplemented!() }
            0x3a => { unimplemented!() }
            0x3b => { unimplemented!() }
            0x3c => { unimplemented!() }
            0x3d => { unimplemented!() }
            0x3e => { unimplemented!() }
            0x3f => { unimplemented!() }
            0x40 => { unimplemented!() }
            0x41 => { unimplemented!() }
            0x42 => { unimplemented!() }
            0x43 => { unimplemented!() }
            0x44 => { unimplemented!() }
            0x45 => { unimplemented!() }
            0x46 => { unimplemented!() }
            0x47 => { unimplemented!() }
            0x48 => { unimplemented!() }
            0x49 => { unimplemented!() }
            0x4a => { unimplemented!() }
            0x4b => { unimplemented!() }
            0x4c => {  // JMP direct
                let (al, ah) = (cpu.fetch_byte_with_pc(mem), cpu.fetch_byte_with_pc(mem));
                cpu.pc = ((al as Word) << 8 | ah as Word);
                cpu.count += 3;
            }
            0x4d => { unimplemented!() }
            0x4e => { unimplemented!() }
            0x4f => { unimplemented!() }
            0x50 => { unimplemented!() }
            0x51 => { unimplemented!() }
            0x52 => { unimplemented!() }
            0x53 => { unimplemented!() }
            0x54 => { unimplemented!() }
            0x55 => { unimplemented!() }
            0x56 => { unimplemented!() }
            0x57 => { unimplemented!() }
            0x58 => { unimplemented!() }
            0x59 => { unimplemented!() }
            0x5a => { unimplemented!() }
            0x5b => { unimplemented!() }
            0x5c => { unimplemented!() }
            0x5d => { unimplemented!() }
            0x5e => { unimplemented!() }
            0x5f => { unimplemented!() }
            0x60 => { unimplemented!() }
            0x61 => { unimplemented!() }
            0x62 => { unimplemented!() }
            0x63 => { unimplemented!() }
            0x64 => { unimplemented!() }
            0x65 => { unimplemented!() }
            0x66 => { unimplemented!() }
            0x67 => { unimplemented!() }
            0x68 => { unimplemented!() }
            0x69 => {  // ADC immediate
                let op = cpu.fetch_byte_with_pc(mem);
                let c = if cpu.sr & status_flag::C != 0 {
                    1
                } else {
                    0
                };
                let a = cpu.a;
                let (a, v) = a.overflowing_add(op);
                let (a, v2) = a.overflowing_add(c);
                if v || v2 {
                    cpu.sr |= status_flag::C;
                }
            }
            0x6a => { unimplemented!() }
            0x6b => { unimplemented!() }
            0x6c => { unimplemented!() }
            0x6d => { unimplemented!() }
            0x6e => { unimplemented!() }
            0x6f => { unimplemented!() }
            0x70 => { unimplemented!() }
            0x71 => { unimplemented!() }
            0x72 => { unimplemented!() }
            0x73 => { unimplemented!() }
            0x74 => { unimplemented!() }
            0x75 => { unimplemented!() }
            0x76 => { unimplemented!() }
            0x77 => { unimplemented!() }
            0x78 => { unimplemented!() }
            0x79 => { unimplemented!() }
            0x7a => { unimplemented!() }
            0x7b => { unimplemented!() }
            0x7c => { unimplemented!() }
            0x7d => { unimplemented!() }
            0x7e => { unimplemented!() }
            0x7f => { unimplemented!() }
            0x80 => { unimplemented!() }
            0x81 => { unimplemented!() }
            0x82 => { unimplemented!() }
            0x83 => { unimplemented!() }
            0x84 => { unimplemented!() }
            0x85 => { unimplemented!() }
            0x86 => { unimplemented!() }
            0x87 => { unimplemented!() }
            0x88 => { unimplemented!() }
            0x89 => { unimplemented!() }
            0x8a => { unimplemented!() }
            0x8b => { unimplemented!() }
            0x8c => { unimplemented!() }
            0x8d => { unimplemented!() }
            0x8e => { unimplemented!() }
            0x8f => { unimplemented!() }
            0x90 => { unimplemented!() }
            0x91 => { unimplemented!() }
            0x92 => { unimplemented!() }
            0x93 => { unimplemented!() }
            0x94 => { unimplemented!() }
            0x95 => { unimplemented!() }
            0x96 => { unimplemented!() }
            0x97 => { unimplemented!() }
            0x98 => { unimplemented!() }
            0x99 => { unimplemented!() }
            0x9a => { unimplemented!() }
            0x9b => { unimplemented!() }
            0x9c => { unimplemented!() }
            0x9d => { unimplemented!() }
            0x9e => { unimplemented!() }
            0x9f => { unimplemented!() }
            0xa0 => { unimplemented!() }
            0xa1 => { unimplemented!() }
            0xa2 => { unimplemented!() }
            0xa3 => { unimplemented!() }
            0xa4 => { unimplemented!() }
            0xa5 => { unimplemented!() }
            0xa6 => { unimplemented!() }
            0xa7 => { unimplemented!() }
            0xa8 => { unimplemented!() }
            0xa9 => {  // LDA immediate
                let m = mem[cpu.pc];
                cpu.pc += 1;
                if (m as i8) < 0 {
                    cpu.sr |= status_flag::N;
                } else if m == 0 {
                    cpu.sr |= status_flag::Z;
                }
                cpu.a = m;
            }
            0xaa => { unimplemented!() }
            0xab => { unimplemented!() }
            0xac => { unimplemented!() }
            0xad => { unimplemented!() }
            0xae => { unimplemented!() }
            0xaf => { unimplemented!() }
            0xb0 => { unimplemented!() }
            0xb1 => { unimplemented!() }
            0xb2 => { unimplemented!() }
            0xb3 => { unimplemented!() }
            0xb4 => { unimplemented!() }
            0xb5 => { unimplemented!() }
            0xb6 => { unimplemented!() }
            0xb7 => { unimplemented!() }
            0xb8 => { unimplemented!() }
            0xb9 => { unimplemented!() }
            0xba => { unimplemented!() }
            0xbb => { unimplemented!() }
            0xbc => { unimplemented!() }
            0xbd => { unimplemented!() }
            0xbe => { unimplemented!() }
            0xbf => { unimplemented!() }
            0xc0 => { unimplemented!() }
            0xc1 => { unimplemented!() }
            0xc2 => { unimplemented!() }
            0xc3 => { unimplemented!() }
            0xc4 => { unimplemented!() }
            0xc5 => { unimplemented!() }
            0xc6 => { unimplemented!() }
            0xc7 => { unimplemented!() }
            0xc8 => { unimplemented!() }
            0xc9 => { unimplemented!() }
            0xca => { unimplemented!() }
            0xcb => { unimplemented!() }
            0xcc => { unimplemented!() }
            0xcd => { unimplemented!() }
            0xce => { unimplemented!() }
            0xcf => { unimplemented!() }
            0xd0 => { unimplemented!() }
            0xd1 => { unimplemented!() }
            0xd2 => { unimplemented!() }
            0xd3 => { unimplemented!() }
            0xd4 => { unimplemented!() }
            0xd5 => { unimplemented!() }
            0xd6 => { unimplemented!() }
            0xd7 => { unimplemented!() }
            0xd8 => { unimplemented!() }
            0xd9 => { unimplemented!() }
            0xda => { unimplemented!() }
            0xdb => { unimplemented!() }
            0xdc => { unimplemented!() }
            0xdd => { unimplemented!() }
            0xde => { unimplemented!() }
            0xdf => { unimplemented!() }
            0xe0 => { unimplemented!() }
            0xe1 => { unimplemented!() }
            0xe2 => { unimplemented!() }
            0xe3 => { unimplemented!() }
            0xe4 => { unimplemented!() }
            0xe5 => { unimplemented!() }
            0xe6 => { unimplemented!() }
            0xe7 => { unimplemented!() }
            0xe8 => { unimplemented!() }
            0xe9 => { unimplemented!() }
            0xea => { unimplemented!() }
            0xeb => { unimplemented!() }
            0xec => { unimplemented!() }
            0xed => { unimplemented!() }
            0xee => { unimplemented!() }
            0xef => { unimplemented!() }
            0xf0 => { unimplemented!() }
            0xf1 => { unimplemented!() }
            0xf2 => { unimplemented!() }
            0xf3 => { unimplemented!() }
            0xf4 => { unimplemented!() }
            0xf5 => { unimplemented!() }
            0xf6 => { unimplemented!() }
            0xf7 => { unimplemented!() }
            0xf8 => { unimplemented!() }
            0xf9 => { unimplemented!() }
            0xfa => { unimplemented!() }
            0xfb => { unimplemented!() }
            0xfc => { unimplemented!() }
            0xfd => { unimplemented!() }
            0xfe => { unimplemented!() }
            0xff => { unimplemented!() }
        }

        if cpu.sr & status_flag::I != 0 {
            // interrupt
            unimplemented!();
        }
    }
}

// https://www.pagetable.com/c64ref/6502/?cpu=65c02&tab=3
enum AddressingMode {
    Implied,
    Accumulator,
    Immediate(Byte),
    Absolute(u16),
    XIndexedAbsolute(u16),
    YIndexedAbsolute(u16),
    AbsoluteIndirect(u16),
    AbsoluteXIndexedIndirect(u16),
    ZeroPage(Byte),
    XIndexedZeroPage(Byte),
    YIndexedZeroPage(Byte),
    ZeroPageIndirect(Byte),
    XIndexedZeroPageIndirect(Byte),
    ZeroPageIndirectYIndexed(Byte),
    Relative(u16),
}

impl AddressingMode {
    fn map(&self, cpu: &R6502, mem: &Memory) {}

    fn bytes(&self) -> Byte {
        match self {
            AddressingMode::Implied => 1,
            AddressingMode::Accumulator => 1,
            AddressingMode::Immediate(_) => 2,
            AddressingMode::Absolute(_) => 3,
            AddressingMode::XIndexedAbsolute(_) => 3,
            AddressingMode::YIndexedAbsolute(_) => 3,
            AddressingMode::AbsoluteIndirect(_) => 3,
            AddressingMode::AbsoluteXIndexedIndirect(_) => 3,
            AddressingMode::ZeroPage(_) => 2,
            AddressingMode::XIndexedZeroPage(_) => 2,
            AddressingMode::YIndexedZeroPage(_) => 2,
            AddressingMode::ZeroPageIndirect(_) => 2,
            AddressingMode::XIndexedZeroPageIndirect(_) => 2,
            AddressingMode::ZeroPageIndirectYIndexed(_) => 2,
            AddressingMode::Relative(_) => 2,
        }
    }
}
