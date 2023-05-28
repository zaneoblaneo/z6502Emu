#![allow(dead_code)]
#[derive(Copy, Clone)]
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

    fn check_neg_u8(&self, num: u8) -> bool {
        num & 0b1000_0000 != 0
    }
    
    fn check_neg_u16(&self, num: u16) -> bool {
        num & 0b1000_0000_0000_0000 != 0
    }

    fn set_flag(&mut self, flag_mask: u8, d: bool){
        if d {
            self.status |= flag_mask;
        } else {
            self.status &= !flag_mask;
        }
    }

    fn get_flag(&self, flag_mask: u8) -> bool{
        self.status & flag_mask != 0
    }
    
    /// returns the flag selected by `flag_mask`
    /// to the user as either a `0` or a `1`
    fn get_flag_u8(&self, flag_mask: u8) -> u8{
        match flag_mask{
            0b1000_0000 => (self.status >> 7) & 1,
            0b0100_0000 => (self.status >> 6) & 1,
            0b0001_0000 => (self.status >> 4) & 1,
            0b0000_1000 => (self.status >> 3) & 1,
            0b0000_0100 => (self.status >> 2) & 1,
            0b0000_0010 => (self.status >> 1) & 1,
            0b0000_0001 => self.status & 1,
            _ => unreachable!(""),
        }
    }
    
    /// A helper function that returns the index into `memory` using the
    /// abs,X addressing mode
    fn get_addr_abs_x(&self) -> usize{
        let hw: usize = (self.memory[self.pc as usize + 1] as usize) << 8
            | self.memory[self.pc as usize + 2] as usize;
        hw + self.x as usize + self.get_flag_u8(Self::C_FLAG) as usize
    }
    
    /// A helper function that returns the index into `memory` using the
    /// abs,Y addressing mode
    fn get_addr_abs_y(&self) -> usize{
        let hw: usize = (self.memory[self.pc as usize + 1] as usize) << 8
            | self.memory[self.pc as usize + 2] as usize;
        hw + self.y as usize + self.get_flag_u8(Self::C_FLAG) as usize
    }
    
    /// A helper function that returns the index into `memory` using the
    /// ind addressing mode
    fn get_addr_ind(&self) -> usize{
        let hw: usize = (self.memory[self.pc as usize + 1] as usize) << 8
            | self.memory[self.pc as usize + 2] as usize;
        (self.memory[hw] as usize) << 8 | self.memory[hw + 1] as usize
    }

    /// A helper function that returns the index into `memory` using the
    /// X,ind addressing mode
    fn get_addr_x_ind(&self) -> usize{
        let b: usize = self.memory[self.pc as usize + 1] as usize;
        (self.memory[b + self.x as usize] as usize) << 8 
            | self.memory[b + self.x as usize + 1] as usize
    }
    
    /// A helper function that returns the index into `memory` using the 
    /// ind,Y addressing mode
    fn get_addr_ind_y(&self) -> usize{
        let b: usize = self.memory[self.pc as usize + 1] as usize;
        ((self.memory[b] as usize) << 8 | self.memory[b + 1] as usize) 
            + self.y as usize + self.get_flag_u8(Self::C_FLAG) as usize
    }
    
    /// A helper function that returns the index into `memory` using the
    /// rel addressing mode
    fn get_addr_rel(&self) -> usize{
        self.pc as usize + self.memory[self.pc as usize + 1] as usize
    }
    
    /// A helper function that returns the index into `memory` using the
    /// zpg,X addressing mode
    fn get_addr_zpg_x(&self) -> usize{
        let b: usize = self.memory[self.pc as usize + 1] as usize;
        b + self.x as usize
    }
    
    /// A helper function that returns the index into `memory` using the
    /// zpg,Y addressing mode
    fn get_addr_zpg_y(&self) -> usize{
        let b: usize = self.memory[self.pc as usize + 1] as usize;
        b + self.y as usize
    }

    fn push_u8(&mut self, d: u8){
        self.memory[self.sp as usize] = d;
        self.sp -= 1u8;
    }

    fn pop_u8(&mut self) -> u8{
        self.sp += 1u8;
        self.memory[self.sp as usize - 1usize]
    }

    fn push_u16(&mut self, d: u16){
        self.memory[self.sp as usize] = ((d >> 8) & 0xffu16) as u8;
        self.memory[self.sp as usize - 1usize] = d as u8;
        self.sp -= 2u8;
    }

    fn pop_u16(&mut self) -> u16{
        let out: u16 = (self.memory[self.sp as usize + 1usize] as u16)
            << 8 | (self.memory[self.sp as usize] as u16);
        self.sp += 2u8;
        out
    }
    
    /// Defined as `a` = `a` & `data`, while setting the `N_FLAG`
    /// and the `Z_FLAG`
    fn op_and(&mut self, data: u8){
        self.a &= data;
        self.set_flag(Self::N_FLAG, self.check_neg_u8(self.a));
        self.set_flag(Self::Z_FLAG, self.a == 0);
    }
    
    /// Defined as `a` = `a` | `data`, while setting the `N_FLAG`
    /// and the `Z_FLAG`
    fn op_ora(&mut self, data: u8){
        self.a |= data;
        self.set_flag(Self::N_FLAG, self.check_neg_u8(self.a));
        self.set_flag(Self::Z_FLAG, self.a == 0);
    }
    
    /// Defined as `data` = `data` << 1, while setting the
    /// `C_FLAG`, `N_FLAG`, and `Z_FLAG`
    fn op_asl(&mut self, data: u8) -> u8{
        self.set_flag(Self::C_FLAG, self.check_neg_u8(data));
        let out: u8 = data << 1;
        self.set_flag(Self::N_FLAG, self.check_neg_u8(out));
        self.set_flag(Self::Z_FLAG, out == 0);
        out
    }
    
    /// Defined as `out` = `data` << 1 | `self.check_neg_u8(data)`
    /// while setting the `C_FLAG`, `N_FLAG`, and `Z_FLAG`
    fn op_rol(&mut self, data: u8) -> u8{
        let tmp: u8 = self.get_flag_u8(Self::C_FLAG);
        self.set_flag(Self::C_FLAG, self.check_neg_u8(data));
        let out: u8 = data << 1 | tmp;
        self.set_flag(Self::N_FLAG, self.check_neg_u8(out));
        self.set_flag(Self::Z_FLAG, out == 0);
        out
    }

    /// Defined as `a` = `a` EOR `data` while setting the `N_FLAG` and `Z_FLAG`
    fn op_eor(&mut self, data: u8){
        self.a ^= data;
        self.set_flag(Self::N_FLAG, self.check_neg_u8(self.a));
        self.set_flag(Self::Z_FLAG, self.a == 0);
    }

    fn run(&mut self) {
        loop {
            let old_pc: u16 = self.pc;
            let opcode: u8  = self.memory[self.pc as usize];
            let b: u8       = self.memory[self.pc as usize + 1usize];
            let hw: u16     = ((self.memory[self.pc as usize + 2usize] as u16) 
                            << 8u16) 
                            | self.memory[self.pc as usize + 1usize] as u16;
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
                    let addr: usize = self.get_addr_x_ind();
                    self.op_ora(self.memory[addr]);
                    self.pc += 2;
                }
                0x05 => { // ORA zpg
                    self.op_ora(self.memory[b as usize]);
                    self.pc += 2;
                },
                0x06 => { // ASL zpg
                    let data = self.op_asl(self.memory[b as usize]);
                    self.memory[b as usize] = data;
                    self.pc += 2;

                },
                0x08 => { // PHP
                    self.push_u8(self.status);
                    self.pc += 1;
                }
                0x09 => { // ORA #
                    self.op_ora(b);
                    self.pc += 2;
                },
                0x0A => { // ASL A
                    let data: u8 = self.op_asl(self.a);
                    self.a = data;
                    self.pc += 1;
                },
                0x0D => { // ORA abs
                    self.op_ora(self.memory[hw as usize]);
                    self.pc += 3;
                },
                0x0E => { // ASL abs
                    let data: u8 = self.memory[hw as usize];
                    let data: u8 = self.op_asl(data);
                    self.memory[hw as usize] = data;
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
                    let data: u8 = self.memory[self.get_addr_ind_y()];
                    self.op_ora(data);
                    self.pc += 2;
                },
                0x15 => { // ORA zpg,X
                    let data: u8 = self.memory[self.get_addr_zpg_x()];
                    self.op_ora(data);
                    self.pc += 2;
                },
                0x16 => { // ASL zpg,X
                    let data: u8 = self.memory[self.get_addr_zpg_x()];
                    let data: u8 = self.op_asl(data);
                    self.memory[self.get_addr_zpg_x()] = data;
                    self.pc += 2;
                },
                0x18 => { // CLC
                    self.set_flag(Self::C_FLAG, false);
                    self.pc += 1;
                },
                0x19 => { // ORA abs,Y
                    let addr: usize = self.get_addr_abs_y();
                    self.op_ora(self.memory[addr]);
                    self.pc += 3;
                },
                0x1D => { // ORA abs,X
                    let addr: usize = self.get_addr_abs_x();
                    self.op_ora(self.memory[addr]);
                    self.pc += 3;
                },
                0x1E => { // ASL abs,X
                    let addr: usize = self.get_addr_abs_x();
                    self.memory[addr] = self.op_asl(self.memory[addr]);
                    self.pc += 3;
                },
                0x20 => { // JSR abs
                    self.push_u16(self.pc + 2);
                    self.pc = hw;
                },
                0x21 => { // AND X,ind
                    let addr: usize = self.get_addr_x_ind();
                    self.op_and(self.memory[addr]);
                    self.pc += 2;
                },
                0x24 => { // BIT zpg
                    let tmp = self.memory[b as usize];
                    self.set_flag(Self::N_FLAG, self.check_neg_u8(tmp));
                    self.set_flag(Self::V_FLAG, tmp & 0b0100_0000 != 0);
                    self.set_flag(Self::Z_FLAG, self.a & tmp == 0);
                    self.pc += 2;
                },
                0x25 => { // AND zpg
                    self.op_and(self.memory[b as usize]);
                    self.pc += 2;
                },
                0x26 => { // ROL zpg
                    self.memory[b as usize] 
                        = self.op_rol(self.memory[b as usize]);
                    self.pc += 2;
                },
                0x28 => { // PLP impl
                    self.status = self.pop_u8();
                    self.pc += 1;
                },
                0x29 => { // AND imm
                    self.op_and(b);
                    self.pc += 2;
                },
                0x2A => { // ROL A
                    self.a = self.op_rol(self.a);
                    self.pc += 1;
                },
                0x2C => { // BIT abs
                    let tmp = self.memory[hw as usize];
                    self.set_flag(Self::N_FLAG, self.check_neg_u8(tmp));
                    self.set_flag(Self::V_FLAG, tmp & 0b0100_0000 != 0);
                    self.set_flag(Self::Z_FLAG, self.a & tmp == 0);
                    self.pc += 3;
                },
                0x2D => { // AND abs
                    self.op_and(self.memory[hw as usize]);
                    self.pc += 3;
                },
                0x2E => { // ROL abs
                    self.memory[hw as usize] 
                        = self.op_rol(self.memory[hw as usize]);
                    self.pc += 3;
                },
                0x31 => { // BMI rel
                    if self.get_flag(Self::N_FLAG){
                        self.pc += b as u16;
                    } else{
                        self.pc += 2;
                    }
                },
                0x32 => { // AND ind,Y
                    let addr: usize = self.memory[b as usize] as usize 
                        + self.y as usize
                        + self.get_flag_u8(Self::C_FLAG) as usize;
                    self.op_and(self.memory[addr]);
                    self.pc += 2;
                },
                0x35 => { // AND zpg,X
                    let data: u8 = self.memory[b as usize + self.x as usize];
                    self.op_and(data);
                    self.pc += 2;
                },
                0x36 => { // ROL zpg,X
                    let addr: usize = b as usize + self.x as usize;
                    self.memory[addr] = self.op_rol(self.memory[addr]);
                    self.pc += 2;
                },
                0x38 => { // SEC impl
                    self.set_flag(Self::C_FLAG, true);
                    self.pc += 1;
                },
                0x39 => { // AND abs,Y
                    let addr: usize = hw as usize 
                        + self.y as usize
                        + self.get_flag_u8(Self::C_FLAG) as usize;
                    self.op_and(self.memory[addr]);
                    self.pc += 3;
                },
                0x3D => { // AND abs,X
                    let addr: usize = hw as usize
                        + self.x as usize
                        + self.get_flag_u8(Self::C_FLAG) as usize;
                    self.op_and(self.memory[addr]);
                    self.pc += 3;
                },
                0x3E => { // ROL abs,X
                    let addr: usize = hw as usize
                        + self.x as usize
                        + self.get_flag_u8(Self::C_FLAG) as usize;
                    self.memory[addr] = self.op_rol(self.memory[addr]);
                    self.pc += 3;
                },
                0x40 => { // RTI
                    self.status = self.pop_u8() & 0b1111_0111;
                    self.pc = self.pop_u16();
                },
                0x41 => { // EOR X,ind
                    let index: usize = b as usize + self.x as usize;
                    let addr: usize = (self.memory[index] as usize) << 8
                        | (self.memory[index + 1usize] as usize);
                    self.op_eor(self.memory[addr]);
                    self.pc += 2;
                },
                0x58 => { // CLI
                    self.set_flag(Self::I_FLAG, false);
                    self.pc += 1;
                },
                0xA2 => { // LDX #
                    self.set_flag(Self::N_FLAG, self.check_neg_u8(b));
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
                    self.set_flag(Self::N_FLAG, self.check_neg_u8(b));
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
