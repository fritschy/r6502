use crate::{status_flag, Memory, Reset, R6502};

use crate::adressing_mode::{AMSelect, AMValue};

use std::ops::{BitAnd, BitOr, BitXor};

pub fn lda<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.a = addr_mode(cpu, AMSelect::V).to_value();
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn ldx<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.x = addr_mode(cpu, AMSelect::V).to_value();
    cpu.set_flag(status_flag::N, cpu.r.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.x == 0);
}

pub fn ldy<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.y = addr_mode(cpu, AMSelect::V).to_value();
    cpu.set_flag(status_flag::N, cpu.r.y & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.y == 0);
}

pub fn sta<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let addr = addr_mode(cpu, AMSelect::A).to_addr();
    cpu.write_byte(addr, cpu.r.a);
}

pub fn stx<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let addr = addr_mode(cpu, AMSelect::A).to_addr();
    cpu.write_byte(addr, cpu.r.x);
}

pub fn sty<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let addr = addr_mode(cpu, AMSelect::A).to_addr();
    cpu.write_byte(addr, cpu.r.y);
}

pub fn tax<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.x = cpu.r.a;
    cpu.set_flag(status_flag::N, cpu.r.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.x == 0);
}

pub fn tay<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.y = cpu.r.a;
    cpu.set_flag(status_flag::N, cpu.r.y & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.y == 0);
}

pub fn tsx<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.x = cpu.r.sp;
    cpu.set_flag(status_flag::N, cpu.r.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.x == 0);
}

pub fn txa<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.a = cpu.r.x;
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn txs<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.sp = cpu.r.x;
    cpu.set_flag(status_flag::N, cpu.r.sp & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.sp == 0);
}

pub fn tya<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.a = cpu.r.y;
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn pha<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.push(cpu.r.a);
}

pub fn php<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.push(cpu.r.sr);
}

pub fn pla<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.a = cpu.pop();
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn plp<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.sr = cpu.pop();
}

pub fn asl<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, AMSelect::A);
    let (o, m) = match m {
        AMValue::Accumulator => {
            let a = cpu.r.a;
            let b = a << 1;
            cpu.r.a = b;
            (a, b)
        }
        AMValue::Address(addr) => {
            let a = cpu.read_byte(addr);
            let b = a << 1;
            cpu.write_byte(addr, b);
            (a, b)
        }
        _ => unreachable!(),
    };
    cpu.set_flag(status_flag::C, o & 0x80 != 0);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn lsr<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, AMSelect::A);
    let (o, m) = match m {
        AMValue::Accumulator => {
            let a = cpu.r.a;
            let b = a >> 1;
            cpu.r.a = b;
            (a, b)
        }
        AMValue::Address(addr) => {
            let a = cpu.read_byte(addr);
            let b = a >> 1;
            cpu.write_byte(addr, b);
            (a, b)
        }
        _ => unreachable!(),
    };
    cpu.set_flag(status_flag::C, o & 0x1 != 0);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn rol<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, AMSelect::A);
    let (o, m) = match m {
        AMValue::Accumulator => {
            let a = cpu.r.a;
            let b = a.rotate_left(1);
            cpu.r.a = b;
            (a, b)
        }
        AMValue::Address(addr) => {
            let a = cpu.read_byte(addr);
            let b = a.rotate_left(1);
            cpu.write_byte(addr, b);
            (a, b)
        }
        _ => unreachable!(),
    };
    cpu.set_flag(status_flag::C, o & 0x80 != 0);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn ror<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, AMSelect::A);
    let (o, m) = match m {
        AMValue::Accumulator => {
            let a = cpu.r.a;
            let b = a.rotate_right(1);
            cpu.r.a = b;
            (a, b)
        }
        AMValue::Address(addr) => {
            let a = cpu.read_byte(addr);
            let b = a.rotate_right(1);
            cpu.write_byte(addr, b);
            (a, b)
        }
        _ => unreachable!(),
    };
    cpu.set_flag(status_flag::C, o & 0x1 != 0);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn and<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, AMSelect::V).to_value();
    cpu.r.a = cpu.r.a.bitand(m);
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn bit<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, AMSelect::V).to_value();
    let a = cpu.r.a.bitand(m);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::V, m & 0x40 != 0);
    cpu.set_flag(status_flag::Z, a == 0);
}

pub fn eor<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, AMSelect::V).to_value();
    cpu.r.a = cpu.r.a.bitxor(m);
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn ora<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, AMSelect::V).to_value();
    cpu.r.a = cpu.r.a.bitor(m);
    cpu.set_flag(status_flag::N, cpu.r.a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.a == 0);
}

pub fn adc<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, AMSelect::V).to_value();
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

pub fn cmp<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, AMSelect::V).to_value();
    let a = cpu.r.a as i16 - m as i16;
    cpu.set_flag(status_flag::N, a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, a == 0);
    cpu.set_flag(status_flag::C, a >= 0);
}

pub fn cpx<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, AMSelect::V).to_value();
    let a = cpu.r.x as i16 - m as i16;
    cpu.set_flag(status_flag::N, a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, a == 0);
    cpu.set_flag(status_flag::C, a >= 0);
}

pub fn cpy<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, AMSelect::V).to_value();
    let a = cpu.r.y as i16 - m as i16;
    cpu.set_flag(status_flag::N, a & 0x80 != 0);
    cpu.set_flag(status_flag::Z, a == 0);
    cpu.set_flag(status_flag::C, a >= 0);
}

pub fn sbc<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let m = addr_mode(cpu, AMSelect::V).to_value();
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

pub fn dec<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let addr = addr_mode(cpu, AMSelect::A).to_addr();
    let m = cpu.read_byte(addr).wrapping_sub(1);
    cpu.write_byte(addr, m);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn dex<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.x = cpu.r.x.wrapping_sub(1);
    cpu.set_flag(status_flag::N, cpu.r.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.x == 0);
}

pub fn dey<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.y = cpu.r.y.wrapping_sub(1);
    cpu.set_flag(status_flag::N, cpu.r.y & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.y == 0);
}

pub fn inc<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let addr = addr_mode(cpu, AMSelect::A).to_addr();
    let m = cpu.read_byte(addr).wrapping_add(1);
    cpu.write_byte(addr, m);
    cpu.set_flag(status_flag::N, m & 0x80 != 0);
    cpu.set_flag(status_flag::Z, m == 0);
}

pub fn inx<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.x = cpu.r.x.wrapping_add(1);
    cpu.set_flag(status_flag::N, cpu.r.x & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.x == 0);
}

pub fn iny<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.y = cpu.r.y.wrapping_add(1);
    cpu.set_flag(status_flag::N, cpu.r.y & 0x80 != 0);
    cpu.set_flag(status_flag::Z, cpu.r.y == 0);
}

pub fn jmp<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.pc = addr_mode(cpu, AMSelect::A).to_addr();
}

pub fn brk<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let sr = cpu.r.sr;
    let pc = cpu.r.pc + 2;
    cpu.push((pc >> 8) as u8);
    cpu.push((pc & 0xff) as u8);
    cpu.push(sr);
    cpu.set_flag(status_flag::I, true);
    cpu.r.pc = 0xfffe;
    cpu.count += 7;
}

pub fn rti<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let sr = cpu.pop();
    let pcl = cpu.pop();
    let pch = cpu.pop();
    let pc = pcl as u16 | (pch as u16) << 8;
    cpu.r.sr = sr & !status_flag::B;
    cpu.r.pc = pc;
    cpu.count += 6;
}

pub fn jsr<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let pc = cpu.r.pc + 2;
    cpu.push((pc >> 8) as u8);
    cpu.push((pc & 0xff) as u8);
    let addr = addr_mode(cpu, AMSelect::A).to_addr();
    cpu.r.pc = addr;
    cpu.count += 6;
}

pub fn rts<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    let pcl = cpu.pop();
    let pch = cpu.pop();
    let pc = pcl as u16 | (pch as u16) << 8;
    cpu.r.pc = pc;
    cpu.count += 6;
}

pub fn bcc<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::C == 0 {
        cpu.r.pc = addr_mode(cpu, AMSelect::A).to_addr();
    }
}

pub fn bcs<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::C != 0 {
        cpu.r.pc = addr_mode(cpu, AMSelect::A).to_addr();
    }
}

pub fn beq<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::Z != 0 {
        cpu.r.pc = addr_mode(cpu, AMSelect::A).to_addr();
    }
}

pub fn bmi<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::N != 0 {
        cpu.r.pc = addr_mode(cpu, AMSelect::A).to_addr();
    }
}

pub fn bne<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::Z == 0 {
        cpu.r.pc = addr_mode(cpu, AMSelect::A).to_addr();
    }
}

pub fn bpl<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::N == 0 {
        cpu.r.pc = addr_mode(cpu, AMSelect::A).to_addr();
    }
}

pub fn bvc<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::V == 0 {
        cpu.r.pc = addr_mode(cpu, AMSelect::A).to_addr();
    }
}

pub fn bvs<M: Memory>(
    cpu: &mut R6502<M>,
    addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    if cpu.r.sr & status_flag::V != 0 {
        cpu.r.pc = addr_mode(cpu, AMSelect::A).to_addr();
    }
}

pub fn clc<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.sr &= !status_flag::C;
}

pub fn cld<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.sr &= !status_flag::D;
}

pub fn cli<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.sr &= !status_flag::I;
}

pub fn clv<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.sr &= !status_flag::V;
}

pub fn sec<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.sr |= status_flag::C;
}

pub fn sed<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.sr |= status_flag::D;
}

pub fn sei<M: Memory>(
    cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
    cpu.r.sr |= status_flag::I;
}

pub fn nop<M: Memory>(
    _cpu: &mut R6502<M>,
    _addr_mode: impl Fn(&mut R6502<M>, AMSelect) -> AMValue,
) {
}
