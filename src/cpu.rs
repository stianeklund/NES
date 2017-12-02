use std::fmt;
use memory::{Memory, Ram};
use rom::Cartridge;
use opcode::Opcode;

#[derive(Debug)]
pub struct StatusRegister {
    negative: u8,
    overflow: u8,
    bit5: u8,      // always 1
    brk: u8,
    decimal: u8,
    interrupt: u8,
    zero: u8,
    carry: u8,

}

impl StatusRegister {
    fn default() -> Self{
        StatusRegister {
            negative: 0,
            overflow: 0,
            bit5: 1,
            brk: 0,
            decimal: 0,
            interrupt: 0,
            zero: 0,
            carry: 0,
        }
    }
}

#[derive(Debug)]
pub struct Registers {
    pub pc: u16,
    pub sp: u8,
    pub acc: u8,
    pub x: u8,
    pub y: u8,
    pub status: u8,
}

impl Registers {
    pub fn default() -> Self {
        Registers {
            pc: 0,
            sp:0xfd,
            acc: 0,
            x: 0,
            y: 0,
            status: 0,
        }
    }
}


pub struct Cpu {
    reg: Registers,
    flags: StatusRegister,
    cycles: u8,
    opcode: u8,
}
impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}\t{}\t{}\t{}\t{}\t{},{},{},{},{},{}", "Opcode","PC","SP","A","X","Y",
                 "\tN","D","I","Z","C");
        writeln!(f, "{:04x}\t{:04x}\t{:04x}\t{:02x}\t{:04x}\t{:04x}\t{} {} {} {} {}", self.opcode, self.reg.pc, self.reg.sp, self.reg.acc,
                 self.reg.x, self.reg.y, self.flags.negative, self.flags.decimal,
                 self.flags.interrupt, self.flags.zero, self.flags.carry)
    }
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            reg: Registers {
                pc: 0,
                sp: 0xfd,
                acc: 0,
                x: 0,
                y: 0,
                status: 0,
            },
            flags: StatusRegister {
                negative: 0,
                overflow: 0,
                bit5: 0,
                brk: 0,
                decimal: 0,
                interrupt: 0,
                zero: 0,
                carry: 0,
            },
            cycles: 0,
            opcode: 0,
        }
    }
}
pub struct ExecutionContext {
    pub cpu: Cpu,
    pub cart: Cartridge,
    pub memory: Ram,
}
impl ExecutionContext {
    pub fn new() -> ExecutionContext {
        ExecutionContext {
            cpu: Cpu::new(),
            cart: Cartridge::new(),
            memory: Ram::new(),
        }
    }

    pub fn decode(&mut self) {
        // let opcode = self.read_byte(self.cpu.reg.pc + 1);
        let opcode = self.cart.prg[self.cpu.reg.pc as usize + 1];

        match opcode {
            0x00 => self.cpu.flags.brk = 1,
            0x01 => self.blp(),
            0xa1 => self.lda(),
            0xa2 => self.ldx(),
            _ => eprintln!("Not implemented"),
        }
        self.cpu.opcode = opcode;
    }
    fn lda(&mut self) {
        self.cpu.reg.acc = self.read_byte(self.cpu.reg.pc + 1);
        self.cpu.flags.zero == self.cpu.reg.acc;
    }
    fn ldx(&mut self) {
        println!("LDX ran");
        unimplemented!();
    }

    fn blp(&self) {
        unimplemented!();
    }
}
