use std::ops::{Index, IndexMut};

use stopwatch::Stopwatch;

use sixfiveohtwo::{opcode, Reset, R6502, SimpleMemory, Memory, Registers};
use std::io::{Read, Write};
use std::any::Any;

struct Apple1BasicMem {
    mem: SimpleMemory,
}

impl Index<u16> for Apple1BasicMem {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        &self.mem[index]
    }
}

impl IndexMut<u16> for Apple1BasicMem {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        &mut self.mem[index]
    }
}

impl Memory for Apple1BasicMem {
    fn read_byte(&mut self, regs: &mut Registers, addr: u16) -> u8 {
        match addr & 0xFF1F {
            0xD010 => {
                let mut b = &mut [0u8][..];
                std::io::stdin().lock().read_exact(&mut b).expect("read from stdin");
                let c = if b[0] == 10 { 13 } else { b[0] };
                return c | 0x80;
            }

            0xD011 => {
                if regs.pc == 0xE006 { // READ
                    return 0x80;
                } else {
                    return 0;
                }
            }

            0xD012 => {
                return 0;
            }

            _ => self.mem.read_byte(regs, addr)
        }
    }

    fn write_byte(&mut self, regs: &mut Registers, addr: u16, value: u8) {
        if addr & 0xFF1F == 0xD012 {
            let value = value & 0x7f;
            let value = if value == 13 { 10 } else { value };

            // charout
            let ch = value;

            let s = regs.sp;
            let a = 1 + (self.mem[0x0100 + s as u16 + 1] as u16) | (self.mem[0x1000 + ((s as u16 + 2) & 0xff)] as u16) << 8;

            /*
             * Apple I BASIC prints every character received
             * from the terminal. UNIX terminals do this
             * anyway, so we have to avoid printing every
             * line again
             */
            if a == 0xe2a6 {
                /* character echo */
                return;
            }
            if a == 0xe2b6 {
                /* CR echo */
                return;
            }

            /*
             * Apple I BASIC prints a line break and 6 spaces
             * after every 37 characters. UNIX terminals do
             * line breaks themselves, so ignore these
             * characters
             */
            if a==0xe025 && (ch==10 || ch==b' ') {
                return;
            }

            if a == 0xe182 {  // INPUT
                // FIXME: return if stdin is not a tty
            }

            print!("{}", ch as char);
            let chb = [ch];
            std::io::stdout().write(&chb[..]);
            std::io::stdout().flush();

            return;
        }

        self.mem.write_byte(regs, addr, value)
    }
}

static APPLE_1_BASIC: &[u8; 4096] = include_bytes!("../apple1basic.bin");

impl Apple1BasicMem {
    fn new() -> Self {
        let mut mem = SimpleMemory::new();

        for (i, b) in APPLE_1_BASIC.iter().enumerate() {
            mem[(i + 0xE000) as u16] = *b;
        }

        mem[0xFFFC] = 0x00;
        mem[0xFFFD] = 0xE0;

        Apple1BasicMem {
            mem,
        }
    }
}

const WOZ_MONITOR: [u8; 256] = [
    /* FF00 */ 0xD8,             /* RESET       CLD             Clear decimal arithmetic mode.  */
    /* FF01 */ 0x58,             /*             CLI  */
    /* FF02 */ 0xA0, 0x7F,       /*             LDY #$7F        Mask for DSP data direction register  */
    /* FF04 */ 0x8C, 0x12, 0xD0, /*             STY DSP         Set it up.  */
    /* FF07 */ 0xA9, 0xA7,       /*             LDA #$A7        KBD and DSP control register mask.  */
    /* FF09 */ 0x8D, 0x11, 0xD0, /*             STA KBD CR      Enable interrupts, set CA1, CB1, for  */
    /* FF0C */ 0x8C, 0x13, 0xD0, /*             STY DSP CR       postitive edge sense/output mode.  */
    /* FF0F */ 0xC9, 0xDF,       /* NOTCR       CMP #$DF        "<-"?  */
    /* FF11 */ 0xF0, 0x13,       /*             BEQ BACKSAPCE   Yes.  */
    /* FF13 */ 0xC9, 0x9B,       /*             CMP #$9B        ESC?  */
    /* FF15 */ 0xF0, 0x03,       /*             BEQ ESCAPE      Yes.  */
    /* FF17 */ 0xC8,             /*             INY             Advance text index.  */
    /* FF18 */ 0x10, 0x0F,       /*             BPL NEXTCHAR    Auto ESC if > 127.  */
    /* FF1A */ 0xA9, 0xDC,       /* ESCAPE      LDA #$DC        "\".  */
    /* FF1C */ 0x20, 0xEF, 0xFF, /*             JSR ECHO        Output it.  */
    /* FF1F */ 0xA9, 0x8D,       /* GETLINE     LDA #$8D        CR.  */
    /* FF21 */ 0x20, 0xEF, 0xFF, /*             JSR ECHO        Output it.  */
    /* FF24 */ 0xA0, 0x01,       /*             LDY #$01        Initiallize text index.  */
    /* FF26 */ 0x88,             /* BACKSPACE   DEY             Backup text index.  */
    /* FF27 */ 0x30, 0xF6,       /*             BMI GETLINE     Beyond start of line, reinitialize  */
    /* FF29 */ 0xAD, 0x11, 0xD0, /* NEXTCHAR    LDA KBD CR      Key ready?  */
    /* FF2C */ 0x10, 0xFB,       /*             BPL NEXTCHAR    Loop until ready.  */
    /* FF2E */ 0xAD, 0x10, 0xD0, /*             LDA KBD         Load character. B7 shoul be '1'  */
    /* FF31 */ 0x99, 0x00, 0x02, /*             STA IN,Y        Add to text buffer.  */
    /* FF34 */ 0x20, 0xEF, 0xFF, /*             JSR ECHO        Display character.  */
    /* FF37 */ 0xC9, 0x8D,       /*             CMP #$8D        CR?  */
    /* FF39 */ 0xD0, 0xD4,       /*             BNE NOTCR       No.  */
    /* FF3B */ 0xA0, 0xFF,       /*             LDY #$FF        Reset text index.  */
    /* FF3D */ 0xA9, 0x00,       /*             LDA #$00        For XAM mode.  */
    /* FF3F */ 0xAA,             /*             TAX             0->X.  */
    /* FF40 */ 0x0A,             /* SETSTOR     ASL             Leaves $7B if setting STOR mode.  */
    /* FF41 */ 0x85, 0x2B,       /* SETMODE     STA MODE        $00 = XAM, $7B = STOR, $A3 = BLOK XAM  */
    /* FF43 */ 0xC8,             /* BLSKIP      INY             Advance text index.  */
    /* FF44 */ 0xB9, 0x00, 0x02, /* NEXTITEM    LDA IN,Y        Get character.  */
    /* FF47 */ 0xC9, 0x8B,       /*             CMP #$8D        CR?  */
    /* FF49 */ 0xF0, 0xD4,       /*             BEQ GETLINE     Yes, done this line.  */
    /* FF4B */ 0xC9, 0xA3,       /*             CMP #$A3        "."?  */
    /* FF4D */ 0x90, 0xF4,       /*             BCC BLSKIP      Skip delimiter.  */
    /* FF4F */ 0xF0, 0xF0,       /*             BEQ SETMODE     Set BLOCK XAM mode.  */
    /* FF51 */ 0xC9, 0xBA,       /*             CMP #$BA        ":"?  */
    /* FF53 */ 0xF0, 0xEB,       /*             BEQ SETSTOR     Yes, set STOR mode.  */
    /* FF55 */ 0xC9, 0xD2,       /*             CMP #$D2        "R"?  */
    /* FF57 */ 0xF0, 0x3B,       /*             BEQ RUN         Yes, run user program.  */
    /* FF59 */ 0x86, 0x28,       /*             STX L           $00->L.  */
    /* FF5B */ 0x86, 0x29,       /*             STX H            and H.  */
    /* FF5D */ 0x84, 0x2A,       /*             STY YSAV        Save Y for comparison.  */
    /* FF5F */ 0xB9, 0x00, 0x02, /* NEXTHEX     LDA IN,Y        Get character for hex test.  */
    /* FF62 */ 0x49, 0xB0,       /*             EOR #$B0        Map digits to $0-9.  */
    /* FF64 */ 0xC9, 0x0A,       /*             CMP #$0A        Digit?  */
    /* FF66 */ 0x90, 0x06,       /*             BCC DIG         Yes.  */
    /* FF68 */ 0x69, 0x88,       /*             ASC #$88        Map letter "A"-"F" to $FA-FF.  */
    /* FF6A */ 0xC9, 0xFA,       /*             CMP #$FA        Hex letter?  */
    /* FF6C */ 0x90, 0x11,       /*             BCC NOHEX       No, character not hex.  */
    /* FF63 */ 0x0A,             /* DIG         ASL  */
    /* FF6F */ 0x0A,             /*             ASL             Hex digit to MSD of A.  */
    /* FF70 */ 0x0A,             /*             ASL  */
    /* FF71 */ 0x0A,             /*             ASL  */
    /* FF72 */ 0xA2, 0x04,       /*             LDX #$04        Shift count.  */
    /* FF74 */ 0x0A,             /* HEXSHIFT    ASL             Hex digit left, MSDB to carry.  */
    /* FF75 */ 0x26, 0x28,       /*             ROL L           Rotate into LSD.  */
    /* FF77 */ 0x26, 0x29,       /*             ROL H           Rotate into MSD's.  */
    /* FF70 */ 0xCA,             /*             DEX             Done 4 shifts?  */
    /* FF7A */ 0xD0, 0xF8,       /*             BNE HEXSHIFT    No, loop.  */
    /* FF7C */ 0xC8,             /*             INY             Advance text index.  */
    /* FF7D */ 0xD0, 0xE0,       /*             BNE NEXTHEX     Always taken. Check next character for hex.  */
    /* FF7F */ 0xC4, 0x2A,       /* NOTHEX      CPY YSAV        Check if L, H empty (no hex digits)  */
    /* FF81 */ 0xF0, 0x97,       /*             BEQ ESCAPE      Yes, generate ESC sequence.  */
    /* FF83 */ 0x24, 0x2B,       /*             BIT MODE        Test MODE byte.  */
    /* FF85 */ 0x50, 0x10,       /*             BVC NOTSTOR     B6 = 0 for STOR, 1 for XAM and BLOCK XAM  */
    /* FF87 */ 0xA5, 0x28,       /*             LDA L           LSD's of hex data.  */
    /* FF89 */ 0x81, 0x26,       /*             STA (STL,X)     Store at current 'store index'.  */
    /* FF8B */ 0xE6, 0x26,       /*             INC STL         Increment store index.  */
    /* FFBD */ 0xD0, 0xB5,       /*             BNE NEXTITEM    Get next item. (no carry).  */
    /* FF8F */ 0xE6, 0x27,       /*             INC STH         Add carry to 'store index' high order.  */
    /* FF91 */ 0x4C, 0x44, 0xFF, /* TONEXTITEM  JMP NEXTITEM    Get next command item.  */
    /* FF94 */ 0x6C, 0x24, 0x00, /* RUN         JMP (XAML)      Run at current XAM index.  */
    /* FF97 */ 0x30, 0x2B,       /* NOTSTOR     BMI XAMNEXT     B7 = 0 for XAM, 1 for BLOCK XAM.  */
    /* FF99 */ 0xA2, 0x02,       /*             LDX #$02        Byte count.  */
    /* FF9B */ 0xB5, 0x27,       /* SETADR      LDA L-1,X       Copy hex data to  */
    /* FF9D */ 0x95, 0x25,       /*             STA STL-1,X      'store index'.  */
    /* FF9F */ 0x95, 0x23,       /*             STA XAML-1,X    And to 'XAM index'.  */
    /* FFA1 */ 0xCA,             /*             DEX             Next of 2 bytes.  */
    /* FFA2 */ 0xD0, 0xF7,       /*             BNE SETADR      Loop unless X = 0.  */
    /* FFA4 */ 0xD0, 0x14,       /* NXTPRNT     BNE PRDATA      NE means no address to print.  */
    /* FFA7 */ 0xA9, 0xBD,       /*             LDA #$8D        CR.  */
    /* FFA8 */ 0x20, 0xEF, 0xFF, /*             JSR ECHO        Output it.  */
    /* FFAB */ 0xA5, 0x25,       /*             LDA XAMH        'Examine index' high-order byte.  */
    /* FFAD */ 0x20, 0xDC, 0xFF, /*             JSR PRBYTE      Output it in hex format.  */
    /* FFB0 */ 0xA5, 0x25,       /*             LDA XAML        Low-order 'examine index' byte.  */
    /* FFB2 */ 0x20, 0xDC, 0xFF, /*             JSR PRBYTE      Output it in hex format.  */
    /* FFB5 */ 0xA9, 0xBA,       /*             LDA #$BA        ":".  */
    /* FFB7 */ 0x20, 0xEF, 0xFF, /*             JSR ECHO        Output it.  */
    /* FFBA */ 0xA9, 0xA0,       /* PRDATA      LDA #$A0        Blank.  */
    /* FFBC */ 0x20, 0xEF, 0xFF, /*             JSR ECHO        Output it.  */
    /* FFBF */ 0xA1, 0x24,       /*             LDA (XAML,X)    Get data byte at 'examine index'  */
    /* FFC1 */ 0x20, 0xDC, 0xFF, /*             JSR PRBYTE      Output it in hex format.  */
    /* FFC4 */ 0x86, 0x2B,       /* XAMNEXT     STX MODE        0->MODE (XAM mode).  */
    /* FFC7 */ 0xA5, 0x25,       /*             LDA XAML  */
    /* FFC8 */ 0xC5, 0x28,       /*             CMP L           Comapre 'examine index' to hex data.  */
    /* FFCA */ 0xA5, 0x25,       /*             LDA XAMH  */
    /* FFCC */ 0xE5, 0x29,       /*             SBC H  */
    /* FFCE */ 0xB0, 0xC1,       /*             BCS TONEXTITEM  Not less, so no more data to output.  */
    /* FFD0 */ 0x36, 0x24,       /*             INC XAML  */
    /* FFD2 */ 0xD0, 0x02,       /*             BND MOD8CHK     Increment 'examine index'.  */
    /* FFD4 */ 0x36, 0x25,       /*             INC XAMH  */
    /* FFD6 */ 0xA5, 0x24,       /* MOD8CHK     LDA XAML        Check low-order 'examine index' byte  */
    /* FFD7 */ 0x29, 0x07,       /*             AND #$07         For MOD 8=0  */
    /* FFDA */ 0x10, 0xC8,       /*             BPL NXTPRNT     Always taken.  */
    /* FFDC */ 0x48,             /* PRBYTE      PHA             Save A for LSD.  */
    /* FFDD */ 0x4A,             /*             LSR  */
    /* FFDE */ 0x4A,             /*             LSR  */
    /* FFDF */ 0x4A,             /*             LSR             MSD to LSD position.  */
    /* FFE0 */ 0x4A,             /*             LSR  */
    /* FFE1 */ 0x20, 0xE5, 0xFF, /*             JSR PRHEX       Output hex digit.  */
    /* FFE4 */ 0x67,             /*             PLA             Restore A.  */
    /* FFE5 */ 0x29, 0x0F,       /* PRHEX       AND #$0F        Make LSD for hex print.  */
    /* FFE7 */ 0x09, 0xB0,       /*             ORA #$B0        Add "0".  */
    /* FFE9 */ 0xC9, 0xBA,       /*             CMP #$BA        Digit?  */
    /* FFEB */ 0x90, 0x02,       /*             BCC ECHO        Yes, output it.  */
    /* FFED */ 0x69, 0x06,       /*             ADC #$06        Add offset for letter.  */
    /* FFEF */ 0x2C, 0x12, 0xD0, /* ECHO        BIT DSP         DA bit (B7) cleared yet?  */
    /* FFF2 */ 0x30, 0xFB,       /*             BMI ECHO        No, wait for display  */
    /* FFF4 */ 0x8D, 0x1D, 0xD0, /*             STA DSP         Output character. Sets DA.  */
    /* FFF7 */ 0x60,             /*             RTS             Return.  */
    /* FFF8 */ 0x00, 0x00,       /* (unused)  */
    /* FFFA */ 0x00, 0x0F,       /* (NMI)  */
    /* FFFC */ 0x00, 0xFF,       /* (RESET)  */
    /* FFFE */ 0x00, 0x00,       /* (IRQ)  */
];

fn main() {
    let mut mem = Apple1BasicMem::new();

    // for (i, b) in WOZ_MONITOR.iter().enumerate() {
    //     mem[i as u16 + 0xff00] = *b;
    // }

    let mut cpu = R6502::new(mem);
    cpu.reset();

    // FIXME; I have no clue how to implement reset vector handling... so... fake it?
    cpu.r.pc = cpu.read_word(0xfffc);

    let cycles = 100000000;
    let s = Stopwatch::start_new();
    cpu.execute(cycles);
    let s = s.elapsed_ms();

    println!("Executed {} instructions in {}ms", cycles, s);

    println!("{}", cpu);
}
