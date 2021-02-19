use super::*;
use crate::*;

#[test]
fn adc_im() {
    let mut mem = SimpleMemory::new();
    mem[0xE000] = 0x69;
    mem[0xE001] = 0x3;
    mem[0xE002] = 0x69;
    mem[0xE003] = 0xff;

    let mut cpu = R6502::new(mem);
    cpu.reset();

    cpu.r.pc = 0xE000;
    cpu.r.a = 2;

    let ins = cpu.step();
    assert_eq!(cpu.r.pc, 0xE002);
    assert_eq!(ins, 0x69);
    assert_eq!(cpu.r.a, 0x5);
    assert!(cpu.get_flag(status_flag::C) == 0);

    let ins = cpu.step();
    assert_eq!(ins, 0x69);
    assert_eq!(cpu.r.a, 0xffu8.wrapping_add(0x5));
    assert!(cpu.get_flag(status_flag::C) == 1);
}
