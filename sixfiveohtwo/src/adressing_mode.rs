use crate::{Memory, R6502};

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

pub fn implied(_cpu: &mut R6502, _mem: &mut Memory, _mode: AMSelect) -> AMValue {
    unreachable!("implied addressing mode should NEVER try to resolve its operand");
}

pub fn immediate(cpu: &mut R6502, mem: &mut Memory, mode: AMSelect) -> AMValue {
    match mode {
        AMSelect::A => cpu.pc.into(),
        AMSelect::V => cpu.fetch_byte_with_pc(mem).into(),
    }
}

pub fn accumulator(cpu: &mut R6502, _mem: &mut Memory, mode: AMSelect) -> AMValue {
    match mode {
        AMSelect::V => cpu.a.into(),
        AMSelect::A => AMValue::Accumulator,
    }
}

pub fn absolute(cpu: &mut R6502, mem: &mut Memory, mode: AMSelect) -> AMValue {
    let addr = cpu.fetch_byte_with_pc(mem) as u16;
    let addr = addr | (cpu.fetch_byte_with_pc(mem) as u16) << 8;
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.fetch_byte_with_address(mem, addr).into(),
    }
}

pub fn x_indexed_absolute(cpu: &mut R6502, mem: &mut Memory, mode: AMSelect) -> AMValue {
    let addr = absolute(cpu, mem, AMSelect::A).to_addr();
    let addr = addr.wrapping_add(cpu.x as u16);
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.fetch_byte_with_address(mem, addr).into(),
    }
}

pub fn y_indexed_absolute(cpu: &mut R6502, mem: &mut Memory, mode: AMSelect) -> AMValue {
    let addr = absolute(cpu, mem, AMSelect::A).to_addr();
    let addr = addr.wrapping_add(cpu.y as u16);
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.fetch_byte_with_address(mem, addr).into(),
    }
}

// This is only ever possible with JMP, hence we just set the PC
pub fn absolute_indirect(cpu: &mut R6502, mem: &mut Memory, mode: AMSelect) -> AMValue {
    let addr = absolute(cpu, mem, AMSelect::A).to_addr();
    let next = cpu.fetch_byte_with_address(mem, addr) as u16;
    let next = next | (cpu.fetch_byte_with_address(mem, addr.wrapping_add(1)) as u16) << 8;
    match mode {
        AMSelect::A => next.into(),
        _ => unreachable!(),
    }
}

pub fn zero_page(cpu: &mut R6502, mem: &mut Memory, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc(mem) as u16;
    match mode {
        AMSelect::A => op.into(),
        AMSelect::V => cpu.fetch_byte_with_address(mem, op).into(),
    }
}

pub fn x_indexed_zero_page(cpu: &mut R6502, mem: &mut Memory, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc(mem);
    let addr = cpu.x.wrapping_add(op) as u16;
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.fetch_byte_with_address(mem, addr).into(),
    }
}

pub fn y_indexed_zero_page(cpu: &mut R6502, mem: &mut Memory, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc(mem);
    let addr = cpu.y.wrapping_add(op) as u16;
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.fetch_byte_with_address(mem, addr).into(),
    }
}

pub fn x_indexed_zero_page_indirect(cpu: &mut R6502, mem: &mut Memory, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc(mem);
    let addr = cpu.x.wrapping_add(op) as u16;
    let next = cpu.fetch_byte_with_address(mem, addr) as u16;
    let next = next | (cpu.fetch_byte_with_address(mem, addr.wrapping_add(1)) as u16) << 8;
    match mode {
        AMSelect::A => next.into(),
        AMSelect::V => cpu.fetch_byte_with_address(mem, next).into(),
    }
}

pub fn zero_page_indirect_y_indexed(cpu: &mut R6502, mem: &mut Memory, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc(mem);
    let addr = op as u16;
    let next = cpu.fetch_byte_with_address(mem, addr) as u16;
    let next = next | (cpu.fetch_byte_with_address(mem, addr.wrapping_add(1)) as u16) << 8;
    let next = next.wrapping_add(cpu.y as u16);
    match mode {
        AMSelect::A => next.into(),
        AMSelect::V => cpu.fetch_byte_with_address(mem, next).into(),
    }
}

// XXX: this is only ever used by JMP
pub fn relative(cpu: &mut R6502, mem: &mut Memory, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc(mem);
    let op = op as i8;
    // XXX I am not happy with this wild around sign/unsign casting
    let pc = cpu.pc as i32;
    let pc = pc + op as i32;
    match mode {
        AMSelect::A => AMValue::from(pc as u16),
        _ => unreachable!(),
    }
}
