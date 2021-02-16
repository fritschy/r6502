use stopwatch::Stopwatch;

use sixfiveohtwo::{opcode, Memory, Reset, R6502, Registers};

fn main() {
    let mut cpu = R6502::new();
    let mut mem = Memory::new();

    cpu.reset();
    cpu.r.pc = 0x200;

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

    println!("{}", cpu);
    println!("0x2000: {:b}", mem[0x2000]);
    println!("0x2001: {:b}", mem[0x2001]);
}
