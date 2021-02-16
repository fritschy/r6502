use crate::{status_flag, Memory, R6502};

use crate::adressing_mode::{AMSelect, AMValue};

use std::ops::{BitAnd, BitOr, BitXor};

pub fn lda(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.a = addr_mode(cpu, mem, AMSelect::V).to_value();
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn ldx(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.x = addr_mode(cpu, mem, AMSelect::V).to_value();
    cpu.set_flag(status_flag::N, cpu.r.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.x == 0);
}

pub fn ldy(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.y = addr_mode(cpu, mem, AMSelect::V).to_value();
    cpu.set_flag(status_flag::N, cpu.r.y & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.y == 0);
}

pub fn sta(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let addr = addr_mode(cpu, mem, AMSelect::A).to_addr();
    cpu.write_byte_to_address(mem, addr, cpu.r.a);
}

pub fn stx(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let addr = addr_mode(cpu, mem, AMSelect::A).to_addr();
    cpu.write_byte_to_address(mem, addr, cpu.r.x);
}

pub fn sty(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let addr = addr_mode(cpu, mem, AMSelect::A).to_addr();
    cpu.write_byte_to_address(mem, addr, cpu.r.y);
}

pub fn tax(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.x = cpu.r.a;
    cpu.set_flag(status_flag::N, cpu.r.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.x == 0);
}

pub fn tay(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.y = cpu.r.a;
    cpu.set_flag(status_flag::N, cpu.r.y & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.y == 0);
}

pub fn tsx(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.x = cpu.r.sp;
    cpu.set_flag(status_flag::N, cpu.r.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.x == 0);
}

pub fn txa(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.a = cpu.r.x;
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn txs(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.sp = cpu.r.x;
    cpu.set_flag(status_flag::N, cpu.r.sp & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.sp == 0);
}

pub fn tya(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.a = cpu.r.y;
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn pha(
    cpu: &mut R6502,
    mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.push(mem, cpu.r.a);
}

pub fn php(
    cpu: &mut R6502,
    mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.push(mem, cpu.r.sr);
}

pub fn pla(
    cpu: &mut R6502,
    mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.a = cpu.pop(mem);
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn plp(
    cpu: &mut R6502,
    mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.sr = cpu.pop(mem);
}

pub fn asl(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, mem, AMSelect::A);
    let (o, m) = match m {
        AMValue::Accumulator => {
            let a = cpu.r.a;
            let b = a << 1;
            cpu.r.a = b;
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

pub fn lsr(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, mem, AMSelect::A);
    let (o, m) = match m {
        AMValue::Accumulator => {
            let a = cpu.r.a;
            let b = a >> 1;
            cpu.r.a = b;
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

pub fn rol(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, mem, AMSelect::A);
    let (o, m) = match m {
        AMValue::Accumulator => {
            let a = cpu.r.a;
            let b = a.rotate_left(1);
            cpu.r.a = b;
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

pub fn ror(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, mem, AMSelect::A);
    let (o, m) = match m {
        AMValue::Accumulator => {
            let a = cpu.r.a;
            let b = a.rotate_right(1);
            cpu.r.a = b;
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

pub fn and(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    cpu.r.a = cpu.r.a.bitand(m);
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn bit(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    let a = cpu.r.a.bitand(m);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::V, m & 0x40 != 0);
    cpu.set_flag(status_flag::Z, a == 0);
}

pub fn eor(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    cpu.r.a = cpu.r.a.bitxor(m);
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn ora(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    cpu.r.a = cpu.r.a.bitor(m);
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn adc(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    let c = cpu.r.sr & 0x1;
    if cpu.r.sr & status_flag::D == 0 {
        let a = cpu.r.a as u16 + m as u16 + c as u16;
        let oa = cpu.r.a;
        cpu.r.a = a as u8;
        cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
        cpu.set_flag(status_flag::Z, cpu.r.a == 0);
        cpu.set_flag(status_flag::C, a & 0x100 != 0);
        cpu.set_flag(status_flag::V, oa & 0x80 != (a as u8) & 0x80);
    } else {
        unimplemented!("Decimal mode is unimplemented!");
    }
}

pub fn cmp(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    let a = cpu.r.a as i16 - m as i16;
    cpu.set_flag(status_flag::N, a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, a == 0);
    cpu.set_flag(status_flag::C, a >= 0);
}

pub fn cpx(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    let a = cpu.r.x as i16 - m as i16;
    cpu.set_flag(status_flag::N, a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, a == 0);
    cpu.set_flag(status_flag::C, a >= 0);
}

pub fn cpy(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    let a = cpu.r.y as i16 - m as i16;
    cpu.set_flag(status_flag::N, a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, a == 0);
    cpu.set_flag(status_flag::C, a >= 0);
}

pub fn sbc(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, mem, AMSelect::V).to_value();
    let c = cpu.r.sr & 0x1;
    let c = if c == 1 { 0 } else { 1 };
    if cpu.r.sr & status_flag::D == 0 {
        let a = cpu.r.a as i16 - m as i16 - c as i16;
        cpu.r.a = a as u8;
        cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
        cpu.set_flag(status_flag::Z, cpu.r.a == 0);
        cpu.set_flag(status_flag::C, a >= 0);
        cpu.set_flag(status_flag::V, a < -127 || a > 127);
    } else {
        unimplemented!("Decimal mode is unimplemented!");
    }
}

pub fn dec(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let addr = addr_mode(cpu, mem, AMSelect::A).to_addr();
    let m = cpu.fetch_byte_with_address(mem, addr).wrapping_sub(1);
    cpu.write_byte_to_address(mem, addr, m);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn dex(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.x = cpu.r.x.wrapping_sub(1);
    cpu.set_flag(status_flag::N, cpu.r.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.x == 0);
}

pub fn dey(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.y = cpu.r.y.wrapping_sub(1);
    cpu.set_flag(status_flag::N, cpu.r.y & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.y == 0);
}

pub fn inc(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let addr = addr_mode(cpu, mem, AMSelect::A).to_addr();
    let m = cpu.fetch_byte_with_address(mem, addr).wrapping_add(1);
    cpu.write_byte_to_address(mem, addr, m);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn inx(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.x = cpu.r.x.wrapping_add(1);
    cpu.set_flag(status_flag::N, cpu.r.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.x == 0);
}

pub fn iny(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.y = cpu.r.y.wrapping_add(1);
    cpu.set_flag(status_flag::N, cpu.r.y & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.y == 0);
}

pub fn jmp(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
}

pub fn brk(
    cpu: &mut R6502,
    mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let sr = cpu.r.sr;
    let pc = cpu.r.pc + 2;
    cpu.push(mem, (pc >> 8) as u8);
    cpu.push(mem, (pc & 0xff) as u8);
    cpu.push(mem, sr);
    cpu.set_flag(status_flag::I, true);
    cpu.r.pc = 0xfffe;
    cpu.count += 7;
}

pub fn rti(
    cpu: &mut R6502,
    mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let sr = cpu.pop(mem);
    let pcl = cpu.pop(mem);
    let pch = cpu.pop(mem);
    let pc = pcl as u16 | (pch as u16) << 8;
    cpu.r.sr = sr & !status_flag::B;
    cpu.r.pc = pc;
    cpu.count += 6;
}

pub fn jsr(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let pc = cpu.r.pc + 2;
    cpu.push(mem, (pc >> 8) as u8);
    cpu.push(mem, (pc & 0xff) as u8);
    let addr = addr_mode(cpu, mem, AMSelect::A).to_addr();
    cpu.r.pc = addr;
    cpu.count += 6;
}

pub fn rts(
    cpu: &mut R6502,
    mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    let pcl = cpu.pop(mem);
    let pch = cpu.pop(mem);
    let pc = pcl as u16 | (pch as u16) << 8;
    cpu.r.pc = pc;
    cpu.count += 6;
}

pub fn bcc(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::C == 0 {
        cpu.r.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn bcs(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::C != 0 {
        cpu.r.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn beq(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::Z != 0 {
        cpu.r.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn bmi(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::N != 0 {
        cpu.r.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn bne(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::Z == 0 {
        cpu.r.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn bpl(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::N == 0 {
        cpu.r.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn bvc(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::V == 0 {
        cpu.r.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn bvs(
    cpu: &mut R6502,
    mem: &mut Memory,
    addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::V != 0 {
        cpu.r.pc = addr_mode(cpu, mem, AMSelect::A).to_addr();
    }
}

pub fn clc(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.sr &= !status_flag::C;
}

pub fn cld(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.sr &= !status_flag::D;
}

pub fn cli(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.sr &= !status_flag::I;
}

pub fn clv(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.sr &= !status_flag::V;
}

pub fn sec(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.sr |= status_flag::C;
}

pub fn sed(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.sr |= status_flag::D;
}

pub fn sei(
    cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
    cpu.r.sr |= status_flag::I;
}

pub fn nop(
    _cpu: &mut R6502,
    _mem: &mut Memory,
    _addr_mode: impl Fn(&mut R6502, &mut Memory, AMSelect) -> AMValue,
) {
}
