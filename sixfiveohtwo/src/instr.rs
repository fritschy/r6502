use crate::{
    Memory,
    R6502,
    status_flag,
};

use crate::adressing_mode::{
    AMValue,
    AMSelect,
};

use std::ops::{BitAnd, BitXor, BitOr};

pub fn instr_lda(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.a = addr_mode(cpu, mem, AMSelect::V).to_value();
    cpu.set_flag(status_flag::N, cpu.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.a == 0);
}

pub fn instr_ldx(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.x = addr_mode(cpu, mem, AMSelect::V).to_value();
    cpu.set_flag(status_flag::N, cpu.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.x == 0);
}

pub fn instr_ldy(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.y = addr_mode(cpu, mem, AMSelect::V).to_value();
    cpu.set_flag(status_flag::N, cpu.y & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.y == 0);
}

pub fn instr_sta(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let addr = addr_mode(cpu, mem, AMSelect::A).to_addr();
    cpu.write_byte_to_address(mem, addr, cpu.a);
}

pub fn instr_stx(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let addr = addr_mode(cpu, mem, AMSelect::A).to_addr();
    cpu.write_byte_to_address(mem, addr, cpu.x);
}

pub fn instr_sty(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let addr = addr_mode(cpu, mem, AMSelect::A).to_addr();
    cpu.write_byte_to_address(mem, addr, cpu.y);
}

pub fn instr_tax(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.x = cpu.a;
    cpu.set_flag(status_flag::N, cpu.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.x == 0);
}

pub fn instr_tay(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.y = cpu.a;
    cpu.set_flag(status_flag::N, cpu.y & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.y == 0);
}

pub fn instr_tsx(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.x = cpu.sp;
    cpu.set_flag(status_flag::N, cpu.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.x == 0);
}

pub fn instr_txa(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.a = cpu.x;
    cpu.set_flag(status_flag::N, cpu.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.a == 0);
}

pub fn instr_txs(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.sp = cpu.x;
    cpu.set_flag(status_flag::N, cpu.sp & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.sp == 0);
}

pub fn instr_tya(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.a = cpu.y;
    cpu.set_flag(status_flag::N, cpu.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.a == 0);
}

pub fn instr_pha(cpu: &mut R6502, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.push(mem, cpu.a);
}

pub fn instr_php(cpu: &mut R6502, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.push(mem, cpu.sr);
}

pub fn instr_pla(cpu: &mut R6502, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.a = cpu.pop(mem);
    cpu.set_flag(status_flag::N, cpu.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.a == 0);
}

pub fn instr_plp(cpu: &mut R6502, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.sr = cpu.pop(mem);
}

pub fn instr_asl(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let m = addr_mode(cpu, mem, AMSelect::A);
    let (o, m) = match m {
        AMValue::Accumulator => {
            let a = cpu.a;
            let b = a << 1;
            cpu.a = b;
            (a, b)
        }
        AMValue::Address(addr) => {
            let a = cpu.fetch_byte_with_address(mem, addr);
            let b = a << 1;
            cpu.write_byte_to_address(mem, addr, b);
            (a, b)
        }
        _ => unreachable!(),
    };
    cpu.set_flag(status_flag::C, o & 0x80 != 0);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn instr_lsr(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let m = addr_mode(cpu, mem, AMSelect::A);
    let (o, m) = match m {
        AMValue::Accumulator => {
            let a = cpu.a;
            let b = a >> 1;
            cpu.a = b;
            (a, b)
        }
        AMValue::Address(addr) => {
            let a = cpu.fetch_byte_with_address(mem, addr);
            let b = a >> 1;
            cpu.write_byte_to_address(mem, addr, b);
            (a, b)
        }
        _ => unreachable!(),
    };
    cpu.set_flag(status_flag::C, o & 0x1 != 0);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn instr_rol(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let m = addr_mode(cpu, mem, AMSelect::A);
    let (o, m) = match m {
        AMValue::Accumulator => {
            let a = cpu.a;
            let b = a.rotate_left(1);
            cpu.a = b;
            (a, b)
        }
        AMValue::Address(addr) => {
            let a = cpu.fetch_byte_with_address(mem, addr);
            let b = a.rotate_left(1);
            cpu.write_byte_to_address(mem, addr, b);
            (a, b)
        }
        _ => unreachable!(),
    };
    cpu.set_flag(status_flag::C, o & 0x80 != 0);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn instr_ror(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let m = addr_mode(cpu, mem, AMSelect::A);
    let (o, m) = match m {
        AMValue::Accumulator => {
            let a = cpu.a;
            let b = a.rotate_right(1);
            cpu.a = b;
            (a, b)
        }
        AMValue::Address(addr) => {
            let a = cpu.fetch_byte_with_address(mem, addr);
            let b = a.rotate_right(1);
            cpu.write_byte_to_address(mem, addr, b);
            (a, b)
        }
        _ => unreachable!(),
    };
    cpu.set_flag(status_flag::C, o & 0x1 != 0);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn instr_and(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    cpu.a = cpu.a.bitand(m);
    cpu.set_flag(status_flag::N, cpu.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.a == 0);
}

pub fn instr_bit(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    let a = cpu.a.bitand(m);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::V, m & 0x40 != 0);
    cpu.set_flag(status_flag::Z, a == 0);
}

pub fn instr_eor(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    cpu.a = cpu.a.bitxor(m);
    cpu.set_flag(status_flag::N, cpu.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.a == 0);
}

pub fn instr_ora(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    cpu.a = cpu.a.bitor(m);
    cpu.set_flag(status_flag::N, cpu.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.a == 0);
}

pub fn instr_adc(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    let c = cpu.sr & 0x1;
    if cpu.sr & status_flag::D == 0 {
        let a = cpu.a as u16 + m as u16 + c as u16;
        let oa = cpu.a;
        cpu.a = a as u8;
        cpu.set_flag(status_flag::N, cpu.a & 0x80 != 0);
        cpu.set_flag(status_flag::Z, cpu.a == 0);
        cpu.set_flag(status_flag::C, a & 0x100 != 0);
        cpu.set_flag(status_flag::V, oa & 0x80 != (a as u8) & 0x80);
    } else {
        unimplemented!("Decimal mode is unimplemented!");
    }
}

pub fn instr_cmp(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    let a = cpu.a as i16 - m as i16;
    cpu.set_flag(status_flag::N, a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, a == 0);
    cpu.set_flag(status_flag::C, a >= 0);
}

pub fn instr_cpx(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    let a = cpu.x as i16 - m as i16;
    cpu.set_flag(status_flag::N, a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, a == 0);
    cpu.set_flag(status_flag::C, a >= 0);
}

pub fn instr_cpy(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    let a = cpu.y as i16 - m as i16;
    cpu.set_flag(status_flag::N, a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, a == 0);
    cpu.set_flag(status_flag::C, a >= 0);
}

pub fn instr_sbc(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    let c = cpu.sr & 0x1;
    let c = if c == 1 { 0 } else { 1 };
    if cpu.sr & status_flag::D == 0 {
        let a = cpu.a as i16 - m as i16 - c as i16;
        cpu.a = a as u8;
        cpu.set_flag(status_flag::N, cpu.a & 0x80 != 0);
        cpu.set_flag(status_flag::Z, cpu.a == 0);
        cpu.set_flag(status_flag::C, a >= 0);
        cpu.set_flag(status_flag::V, a < -127 || a > 127);
    } else {
        unimplemented!("Decimal mode is unimplemented!");
    }
}

pub fn instr_dec(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let addr = addr_mode(cpu, mem, AMSelect::A).to_addr();
    let m = cpu.fetch_byte_with_address(mem, addr).wrapping_sub(1);
    cpu.write_byte_to_address(mem, addr, m);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn instr_dex(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.x = cpu.x.wrapping_sub(1);
    cpu.set_flag(status_flag::N, cpu.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.x == 0);
}

pub fn instr_dey(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.y = cpu.y.wrapping_sub(1);
    cpu.set_flag(status_flag::N, cpu.y & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.y == 0);
}

pub fn instr_inc(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let addr = addr_mode(cpu, mem, AMSelect::A).to_addr();
    let m = cpu.fetch_byte_with_address(mem, addr).wrapping_add(1);
    cpu.write_byte_to_address(mem, addr, m);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn instr_inx(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.x = cpu.x.wrapping_add(1);
    cpu.set_flag(status_flag::N, cpu.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.x == 0);
}

pub fn instr_iny(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.y = cpu.y.wrapping_add(1);
    cpu.set_flag(status_flag::N, cpu.y & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.y == 0);
}

pub fn instr_jmp(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
}

pub fn instr_brk(cpu: &mut R6502, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let sr = cpu.sr;
    let pc = cpu.pc + 2;
    cpu.push(mem, (pc >> 8) as u8);
    cpu.push(mem, (pc & 0xff) as u8);
    cpu.push(mem, sr);
    cpu.set_flag(status_flag::I, true);
    cpu.pc = 0xfffe;
    cpu.count += 7;
}

pub fn instr_rti(cpu: &mut R6502, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let sr = cpu.pop(mem);
    let pcl = cpu.pop(mem);
    let pch = cpu.pop(mem);
    let pc = pcl as u16 | (pch as u16) << 8;
    cpu.sr = sr & !status_flag::B;
    cpu.pc = pc;
    cpu.count += 6;
}

pub fn instr_jsr(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let pc = cpu.pc + 2;
    cpu.push(mem, (pc >> 8) as u8);
    cpu.push(mem, (pc & 0xff) as u8);
    let addr = addr_mode(cpu, mem, AMSelect::A).to_addr();
    cpu.pc = addr;
    cpu.count += 6;
}

pub fn instr_rts(cpu: &mut R6502, mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    let pcl = cpu.pop(mem);
    let pch = cpu.pop(mem);
    let pc = pcl as u16 | (pch as u16) << 8;
    cpu.pc = pc;
    cpu.count += 6;
}

pub fn instr_bcc(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    if cpu.sr & status_flag::C == 0 {
        cpu.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn instr_bcs(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    if cpu.sr & status_flag::C != 0 {
        cpu.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn instr_beq(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    if cpu.sr & status_flag::Z != 0 {
        cpu.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn instr_bmi(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    if cpu.sr & status_flag::N != 0 {
        cpu.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn instr_bne(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    if cpu.sr & status_flag::Z == 0 {
        cpu.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn instr_bpl(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    if cpu.sr & status_flag::N == 0 {
        cpu.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn instr_bvc(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    if cpu.sr & status_flag::V == 0 {
        cpu.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn instr_bvs(cpu: &mut R6502, mem: &mut Memory, addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    if cpu.sr & status_flag::V != 0 {
        cpu.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn instr_clc(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.sr &= !status_flag::C;
}

pub fn instr_cld(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.sr &= !status_flag::D;
}

pub fn instr_cli(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.sr &= !status_flag::I;
}

pub fn instr_clv(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.sr &= !status_flag::V;
}

pub fn instr_sec(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.sr |= status_flag::C;
}

pub fn instr_sed(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.sr |= status_flag::D;
}

pub fn instr_sei(cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
    cpu.sr |= status_flag::I;
}

pub fn instr_nop(_cpu: &mut R6502, _mem: &mut Memory, _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue) {
}