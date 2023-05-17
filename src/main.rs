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
    
    fn brk(&mut self) {
        unimplemented!("BRK")
    }

    fn set_n_flag(&mut self, d: bool) {
        if d {
            self.status |= 0b1000_0000;
        } else {
            self.status &= 0b0111_1111;
        }
    }

    fn set_v_flag(&mut self, d: bool) {
        if d {
            self.status |= 0b0100_0000;
        } else {
            self.status &= 0b1011_1111;
        }
    }
    
    fn set_b_flag(&mut self, d: bool) {
        if d {
            self.status |= 0b0001_0000;
        } else {
            self.status &= 0b1110_1111;
        }
    }

    fn set_d_flag(&mut self, d: bool) {
        if d {
            self.status |= 0b0000_1000;
        } else {
            self.status &= 0b1111_0111;
        }
    }

    fn set_i_flag(&mut self, d: bool) {
        if d {
            self.status |= 0b0000_0100;
        } else {
            self.status &= 0b1111_1011;
        }
    }
    
    fn set_z_flag(&mut self, d: bool) {
        if d {
            self.status |= 0b0000_0010;
        } else {
            self.status &= 0b1111_1101;
        }
    }

    fn set_c_flag(&mut self, d: bool) {
        if d {
            self.status |= 0b0000_0001;
        } else {
            self.status &= 0b1111_1110;
        }
    }
    
    fn ldx_imm(&mut self, d: u8) {
        self.set_n_flag(d > 0b1000_0000);
        self.set_z_flag(d == 0);
        self.x = d;
        self.pc += 2;
    }

    fn ora_imm(&mut self, d: u8) {
        self.set_n_flag(d > 0b1000_0000);
        self.set_z_flag(d == 0);
        self.a |= d;
    }

    fn run(&mut self) {
        loop {
            let opcode = self.memory[self.pc as usize];
            match opcode {
                0 => self.brk(),
                0x13 => unimplemented!("Invalid opcode. 0x{opcode:2X}"),
                0x18 => self.set_c_flag(false),
                0x58 => self.set_i_flag(false),
                0xA2 => self.ldx_imm(self.memory[self.pc as usize + 1usize]),
                0xB8 => self.set_v_flag(false),
                0xD8 => self.set_d_flag(false),
                0xff => break, // CUSTOM HALT
                _ => unimplemented!("0x{opcode:2X}"),
            }
        }
    }

    fn print_regs(&self) {
        println!("
        a: 0x{:02X}
        x: 0x{:02X}
        y: 0x{:02X}
        pc: 0x{:04X}
        sp: 0x{:02X}
        status: 0b{:08b}", 
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
    cpu.reset();
    cpu.memory[0] = 0xA2;
    cpu.memory[1] = 0xf0;
    cpu.memory[2] = 0xff;
    cpu.print_regs();
    cpu.run();
    cpu.print_regs();
}
