#![allow(dead_code)]

struct Cpu6502{
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    status: u8,
    memory: [u8; 65536],
}

impl Cpu6502{
    const N_FLAG: u8 = 0b1000_0000;
    const V_FLAG: u8 = 0b0100_0000;
    const B_FLAG: u8 = 0b0001_0000;
    const D_FLAG: u8 = 0b0000_1000;
    const I_FLAG: u8 = 0b0000_0100;
    const Z_FLAG: u8 = 0b0000_0010;
    const C_FLAG: u8 = 0b0000_0001;

    fn new() -> Self {
        Cpu6502 {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0xFD,
            status: 0x24,
            memory: [0; 65536]
        }
    }
    
    fn reset(&mut self) {
        let lo = self.memory[0xfffc] as u16;
        let hi = self.memory[0xfffd] as u16;
        self.pc = (hi << 8) | lo;
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = 0xFD;
        self.status = 0x24;
    }
    
    fn set_flag(&mut self, flag_mask: u8, d: bool){
        if d {
            self.status |= flag_mask;
        } else {
            self.status &= !flag_mask;
        }
    }

    fn push_u8(&mut self, d: u8){
        self.memory[self.sp as usize] = d;
        self.sp -= 1u8;
    }

    fn push_u16(&mut self, d: u16){
        self.memory[self.sp as usize] = ((d >> 8) & 0xffu16) as u8;
        self.memory[self.sp as usize - 1usize] = d as u8;
        self.sp -= 2u8;
    }

    fn run(&mut self) {
        loop {
            let old_pc: u16 = self.pc;
            let opcode: u8  = self.memory[self.pc as usize];
            let b: u8       = self.memory[self.pc as usize + 1usize];
            let hw: u16    = ((self.memory[self.pc as usize + 2usize] as u16) 
                           << 8u16) | 
                self.memory[self.pc as usize + 1usize] as u16;
            match opcode {
                0x00 => { // BRK
                    self.push_u16(hw);
                    self.set_flag(Self::B_FLAG, true);
                    self.push_u8(self.status);
                    self.set_flag(Self::I_FLAG, true);
                    self.pc = (self.memory[0xfffeusize] as u16) << 8
                        | (self.memory[0xffffusize] as u16);
                    self.pc += 1;
                },
                0x01 => { // ORA X, ind
                    let addr: u16 = hw; 
                    self.set_flag(Self::N_FLAG, addr > 0b1000_0000);
                    self.set_flag(Self::Z_FLAG, addr == 0);
                    self.a |= self.memory[addr as usize];
                    self.pc += 2;
                }
                0x05 => { // ORA zpg
                    let d = self.memory[b as usize];
                    self.set_flag(Self::N_FLAG, d > 0b1000_0000);
                    self.set_flag(Self::Z_FLAG, d == 0);
                    self.a |= self.memory[d as usize];
                    self.pc += 2;
                },
                0x06 => { // ASL zpg
                    let mut d: u8 = self.memory[b as usize];
                    self.set_flag(Self::C_FLAG, (d & 0b1000_0000) != 0);
                    d <<= 1;
                    self.set_flag(Self::Z_FLAG, d == 0);
                    self.set_flag(Self::N_FLAG, d > 0b1000_0000);
                    self.memory[b as usize] = d;
                    self.pc += 2;

                },
                0x08 => { // PHP
                    self.push_u8(self.status);
                    self.pc += 1;
                }
                0x09 => { // ORA #
                    self.set_flag(Self::N_FLAG, b > 0b1000_0000);
                    self.set_flag(Self::Z_FLAG, b == 0);
                    self.a |= b;
                    self.pc += 2;
                },
                0x0A => { // ASL A
                    self.set_flag(Self::C_FLAG, (self.a & 0b1000_0000) != 0);
                    self.a <<= 1u8;
                    self.set_flag(Self::Z_FLAG, self.a == 0);
                    self.set_flag(Self::N_FLAG, self.a > 0b1000_0000);
                    self.pc += 1;
                },
                0x0D => { // ORA abs
                    self.a |= self.memory[hw as usize];
                    self.set_flag(Self::Z_FLAG, self.a == 0);
                    self.set_flag(Self::N_FLAG, self.a > 0b1000_0000);
                    self.pc += 3;
                },
                0x0E => { // ASL abs
                    self.set_flag(Self::C_FLAG, 
                                  (hw & 0b1000_0000_0000_0000) != 0);
                    self.memory[hw as usize] <<= 1;
                    self.pc += 3;
                },
                0x10 => { // BPL rel
                    if self.status & Self::N_FLAG != 0{
                        self.pc += self.memory[b as usize] as u16;
                    } else {
                        self.pc += 2;
                    }
                },
                0x11 => { // ORA ind,Y
                    let mut data: u8 = self.memory[b as usize] + self.y;
                    if self.status & Self::C_FLAG != 0{
                        data += 1;
                    }
                    self.a |= data;
                    self.set_flag(Self::Z_FLAG, self.a == 0);
                    self.set_flag(Self::N_FLAG, self.a & 0b1000_0000 != 0);
                    self.pc += 2;
                },
                0x15 => { // ORA zpg,X
                    let data: u8 = self.memory[b as usize + self.x as usize];
                    self.a |= data;
                    self.set_flag(Self::Z_FLAG, self.a == 0);
                    self.set_flag(Self::N_FLAG, self.a & 0b1000_0000 != 0);
                    self.pc += 2;
                },
                0x16 => { // ASL zpg,X
                    let data: u8 = self.memory[b as usize + self.x as usize];
                    self.set_flag(Self::C_FLAG, data & 0b1000_0000 != 0);
                    self.memory[b as usize + self.x as usize] <<= 1;
                    let data: u8 = self.memory[b as usize + self.x as usize];
                    self.set_flag(Self::N_FLAG, data & 0b1000_0000 != 0);
                    self.set_flag(Self::Z_FLAG, data == 0);
                    self.pc += 2;
                },
                0x18 => { // CLC
                    self.set_flag(Self::C_FLAG, false);
                    self.pc += 1;
                },
                0x19 => { // ORA abs,Y
                    let mut addr: usize = (hw + self.y as u16) as usize;
                    if self.status & Self::C_FLAG != 0{
                        addr += 1;
                    }
                    self.a |= self.memory[addr];
                    self.set_flag(Self::N_FLAG, self.a & 0b1000_0000 != 0);
                    self.set_flag(Self::Z_FLAG, self.a == 0);
                    self.pc += 3;
                },
                0x1D => { // ORA abs,X
                    let mut addr: usize = (hw + self.x as u16) as usize;
                    if self.status & Self::C_FLAG != 0{
                        addr += 1;
                    }
                    self.a |= self.memory[addr];
                    self.set_flag(Self::N_FLAG, self.a & 0b1000_0000 != 0);
                    self.set_flag(Self::Z_FLAG, self.a == 0);
                    self.pc += 3;
                },
                0x1E => { // ASL abs,X
                    let mut addr: usize = (hw + self.x as u16) as usize;
                    if self.status & Self::C_FLAG != 0{
                        addr += 1;
                    }
                    let data = self.memory[addr];
                    self.set_flag(Self::C_FLAG, data & 0b1000_0000 != 0);
                    self.memory[addr] <<= 1;
                    let data = self.memory[addr];
                    self.set_flag(Self::N_FLAG, data & 0b1000_0000 != 0);
                    self.set_flag(Self::Z_FLAG, data == 0);
                    self.pc += 3;
                },
                0x20 => { // JSR abs
                    self.push_u16(self.pc + 2);
                    self.pc = hw;
                },
                0x21 => { // AND X,ind

                    unimplemented!("0x{:04} AND X,ind", self.pc);
                },
                0x24 => { // BIT zpg
                    unimplemented!("0x{:04} BIT zpg", self.pc);
                },
                0x25 => { // AND zpg
                    unimplemented!("0x{:04} AND zpg", self.pc);
                },
                0x26 => { // ROL zpg
                    unimplemented!("0x{:04} ROL zpg", self.pc);
                },
                0x28 => { // PLP impl
                    unimplemented!("0x{:04} PLP impl", self.pc);
                },
                0x29 => { // AND #
                    unimplemented!("0x{:04} AND #", self.pc);
                },
                0x2A => { // ROL A
                    unimplemented!("0x{:04} ROL A", self.pc);
                },
                0x2C => { // BIT abs
                    unimplemented!("0x{:04} BIT abs", self.pc);
                },
                0x2D => { // AND abs
                    unimplemented!("0x{:04} AND abs", self.pc);
                },
                0x2E => { // ROL abs
                    unimplemented!("0x{:04} ROL abs", self.pc);
                },
                0x58 => { // CLI
                    self.set_flag(Self::I_FLAG, false);
                    self.pc += 1;
                },
                0xA2 => { // LDX #
                    self.set_flag(Self::N_FLAG, b > 0b1000_0000);
                    self.set_flag(Self::Z_FLAG, b == 0);
                    self.x = b;
                    self.pc += 2;
                },
                0xB8 => { // CLV
                    self.set_flag(Self::V_FLAG, false);
                    self.pc += 1;
                },
                0xD8 => { // CLD
                    self.set_flag(Self::D_FLAG, false);
                    self.pc += 1;
                },
                0xE0 => { // CPX #
                    self.set_flag(Self::N_FLAG, (b & 0b1000_0000u8) != 0u8);
                    self.set_flag(Self::Z_FLAG, b == 0);
                    self.set_flag(Self::C_FLAG, self.x < b);
                    self.pc += 2;
                },
                0xEF => { // CUSTOM PRINT_REGS
                    self.print_regs();
                    self.pc += 1;
                },
                0xff => break, // CUSTOM HALT
                _ => unimplemented!("{:04x} opcode: 0x{opcode:2X}", self.pc),
            }
            assert!(old_pc != self.pc, "{opcode:02X} does not increment pc.");
        }
    }

    fn print_regs(&self) {
        println!("CPU STATE [
        a: 0x{:02X}
        x: 0x{:02X}
        y: 0x{:02X}
        pc: 0x{:04X}
        sp: 0x{:02X}
        status: 0b{:08b}\n]",
        self.a,
        self.x,
        self.y,
        self.pc,
        self.sp,
        self.status);
    }
}

fn main() {
    let mut cpu: Cpu6502 = Cpu6502::new();
    // set reset vector to 0x8000
    cpu.memory[0xfffc] = 0x00;
    cpu.memory[0xfffd] = 0x80;
    cpu.reset();
    // set instructions
    cpu.memory[0x8000] = 0xef;
    cpu.memory[0x8001] = 0xA2;
    cpu.memory[0x8002] = 0xf0;
    cpu.memory[0x8003] = 0xef;
    cpu.memory[0x8004] = 0xff;
    cpu.run();
}
