use crate::{Memory, Reset, R6502};

pub enum AMValue {
    Address(u16),
    Value(u8),
    Accumulator,
}

impl AMValue {
    pub fn to_addr(self) -> u16 {
        match self {
            AMValue::Address(a) => a,
            _ => panic!("Cannot get address of a Value"),
        }
    }

    pub fn to_value(self) -> u8 {
        match self {
            AMValue::Value(a) => a,
            _ => panic!("Cannot get value of an Address"),
        }
    }
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
pub enum AMSelect {
    A,
    V,
}

pub fn implied<M: Memory>(_cpu: &mut R6502<M>, _mode: AMSelect) -> AMValue {
    unreachable!("implied addressing mode should NEVER try to resolve its operand");
}

pub fn immediate<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    match mode {
        AMSelect::A => cpu.r.pc.into(),
        AMSelect::V => cpu.fetch_byte_with_pc().into(),
    }
}

pub fn accumulator<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    match mode {
        AMSelect::V => cpu.r.a.into(),
        AMSelect::A => AMValue::Accumulator,
    }
}

pub fn absolute<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let addr = cpu.fetch_byte_with_pc() as u16;
    let addr = addr | (cpu.fetch_byte_with_pc() as u16) << 8;
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.read_byte(addr).into(),
    }
}

pub fn x_indexed_absolute<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let addr = absolute(cpu, AMSelect::A).to_addr();
    let addr = addr.wrapping_add(cpu.r.x as u16);
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.read_byte(addr).into(),
    }
}

pub fn y_indexed_absolute<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let addr = absolute(cpu, AMSelect::A).to_addr();
    let addr = addr.wrapping_add(cpu.r.y as u16);
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.read_byte(addr).into(),
    }
}

// This is only ever possible with JMP, hence we just set the PC
pub fn absolute_indirect<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let addr = absolute(cpu, AMSelect::A).to_addr();
    let next = cpu.read_byte(addr) as u16;
    let next = next | (cpu.read_byte(addr.wrapping_add(1)) as u16) << 8;
    match mode {
        AMSelect::A => next.into(),
        _ => unreachable!(),
    }
}

pub fn zero_page<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc() as u16;
    match mode {
        AMSelect::A => op.into(),
        AMSelect::V => cpu.read_byte(op).into(),
    }
}

pub fn x_indexed_zero_page<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc();
    let addr = cpu.r.x.wrapping_add(op) as u16;
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.read_byte(addr).into(),
    }
}

pub fn y_indexed_zero_page<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc();
    let addr = cpu.r.y.wrapping_add(op) as u16;
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.read_byte(addr).into(),
    }
}

pub fn x_indexed_zero_page_indirect<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc();
    let addr = cpu.r.x.wrapping_add(op) as u16;
    let next = cpu.read_byte(addr) as u16;
    let next = next | (cpu.read_byte(addr.wrapping_add(1)) as u16) << 8;
    match mode {
        AMSelect::A => next.into(),
        AMSelect::V => cpu.read_byte(next).into(),
    }
}

pub fn zero_page_indirect_y_indexed<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc();
    let addr = op as u16;
    let next = cpu.read_byte(addr) as u16;
    let next = next | (cpu.read_byte(addr.wrapping_add(1)) as u16) << 8;
    let next = next.wrapping_add(cpu.r.y as u16);
    match mode {
        AMSelect::A => next.into(),
        AMSelect::V => cpu.read_byte(next).into(),
    }
}

// XXX: this is only ever used by JMP
pub fn relative<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc();
    let op = op as i8;
    // XXX I am not happy with this wild around sign/unsign casting
    let pc = cpu.r.pc as i32;
    let pc = pc + op as i32;
    match mode {
        AMSelect::A => AMValue::from(pc as u16),
        _ => unreachable!(),
    }
}
