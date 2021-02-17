use crate::{Memory, R6502};

/// A value from an addressing mode, can either be the value that was
/// read from memory, or only its address, in which case, no memory access
/// is done (i.e. perhaps you need the address for writing to it)
pub enum AMValue {
    Address(u16),
    Value(u8),
    /// Sometimes we need to differentiate between a value in memory or
    /// an accumulator, as certain instructions can work on values in memory
    /// or the accumulator.
    Accumulator,
}

impl AMValue {
    pub fn to_addr(&self) -> u16 {
        match *self {
            AMValue::Address(a) => a,
            _ => panic!("Cannot get address of a Value"),
        }
    }

    pub fn to_value(&self) -> u8 {
        match *self {
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
    /// want to compute the address only (no memory read, useful for writing to memory)
    A,
    /// want to read the value from memory
    V,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum AM {
    /// implied
    IMPL,
    /// immediate
    IM,
    /// accumulator
    ACC,
    /// absolute
    ABS,
    /// x indexed absolute
    XIA,
    /// y indexed absolute
    YIA,
    /// absolute indirect
    ABSI,
    /// zero page
    ZP,
    /// x indexed zero page
    XIZ,
    /// y indexed zero page
    YIZ,
    /// x indirexed zero page indirect
    XIZI,
    /// zero page indirect y indexed
    ZIYI,
    /// relative
    REL,
}

impl AM {
    /// return length of operand in bytes
    pub fn length(self) -> u16 {
        match self {
            AM::IMPL | AM::ACC => 0,
            AM::ABS | AM::ABSI | AM::XIA | AM::YIA => 2,
            _ => 1,
        }
    }

    /// return addressing mode implementation function
    pub fn dispatch<M: Memory>(self) -> impl Fn(&mut R6502<M>, AMSelect) -> AMValue {
        match self {
            AM::IMPL => implied,
            AM::IM   => immediate,
            AM::ACC => accumulator,
            AM::ABS  => absolute,
            AM::XIA  => x_indexed_absolute,
            AM::YIA  => y_indexed_absolute,
            AM::ABSI => absolute_indirect,
            AM::ZP => zero_page,
            AM::XIZ  => x_indexed_zero_page,
            AM::YIZ  => y_indexed_zero_page,
            AM::XIZI => x_indexed_zero_page_indirect,
            AM::ZIYI => zero_page_indirect_y_indexed,
            AM::REL  => relative,
        }
    }
}

fn implied<M: Memory>(_cpu: &mut R6502<M>, _mode: AMSelect) -> AMValue {
    unreachable!("implied addressing mode should NEVER try to resolve its operand");
}

fn immediate<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    match mode {
        AMSelect::A => cpu.r.pc.into(),
        AMSelect::V => cpu.fetch_byte_with_pc().into(),
    }
}

fn accumulator<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    match mode {
        AMSelect::V => cpu.r.a.into(),
        AMSelect::A => AMValue::Accumulator,
    }
}

fn absolute<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let addr = cpu.fetch_byte_with_pc() as u16;
    let addr = addr | (cpu.fetch_byte_with_pc() as u16) << 8;
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.read_byte(addr).into(),
    }
}

fn x_indexed_absolute<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let addr = absolute(cpu, AMSelect::A).to_addr();
    let addr = addr.wrapping_add(cpu.r.x as u16);
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.read_byte(addr).into(),
    }
}

fn y_indexed_absolute<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let addr = absolute(cpu, AMSelect::A).to_addr();
    let addr = addr.wrapping_add(cpu.r.y as u16);
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.read_byte(addr).into(),
    }
}

// This is only ever possible with JMP, hence we just set the PC
fn absolute_indirect<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let addr = absolute(cpu, AMSelect::A).to_addr();
    let next = cpu.read_byte(addr) as u16;
    let next = next | (cpu.read_byte(addr.wrapping_add(1)) as u16) << 8;
    match mode {
        AMSelect::A => next.into(),
        _ => unreachable!(),
    }
}

fn zero_page<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc() as u16;
    match mode {
        AMSelect::A => op.into(),
        AMSelect::V => cpu.read_byte(op).into(),
    }
}

fn x_indexed_zero_page<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc();
    let addr = cpu.r.x.wrapping_add(op) as u16;
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.read_byte(addr).into(),
    }
}

fn y_indexed_zero_page<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc();
    let addr = cpu.r.y.wrapping_add(op) as u16;
    match mode {
        AMSelect::A => addr.into(),
        AMSelect::V => cpu.read_byte(addr).into(),
    }
}

fn x_indexed_zero_page_indirect<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
    let op = cpu.fetch_byte_with_pc();
    let addr = cpu.r.x.wrapping_add(op) as u16;
    let next = cpu.read_byte(addr) as u16;
    let next = next | (cpu.read_byte(addr.wrapping_add(1)) as u16) << 8;
    match mode {
        AMSelect::A => next.into(),
        AMSelect::V => cpu.read_byte(next).into(),
    }
}

fn zero_page_indirect_y_indexed<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
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
fn relative<M: Memory>(cpu: &mut R6502<M>, mode: AMSelect) -> AMValue {
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
