use std::fmt;
use opcode::Instruction;
use interconnect::MemoryHandler;
use memory::{Ram, Mapper};
use rom::Cartridge;


impl MemoryHandler for ExecutionContext {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            // See https://wiki.nesdev.com/w/index.php/CPU_memory_map
            0...0x07ff => self.ram.memory[addr as usize] as u8,
            // 0...0x2007 => self.ppu[addr] // TODO
            // RAM Mirror
            0x0800...0x1fff => self.ram.memory[addr as usize & 0x07ff] as u8,
            // PRG ROM
            0x8000...0xffff => self.cart.prg[addr as usize & 0x7fff] as u8,
            _ => panic!("Unrecognized addr: {:04x}", addr)
        }

    }

    fn write(&mut self, addr: u16, byte: u8) {
        match addr {
            0...0x07ff => self.ram.memory[addr as usize] = byte,
            0x0800...0x1fff => self.ram.memory[addr as usize & 0x07ff] = byte,
            0x8000...0xffff => self.cart.prg[addr as usize & 0x7fff] = byte,
            _ => eprintln!("Unable to write to memory address"),
        };
    }
}

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
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub status: u8,
}

impl Registers {
    pub fn default() -> Self {
        Registers {
            pc: 0,
            sp:0,
            a: 0,
            x: 0,
            y: 0,
            status: 0,
        }
    }
}


pub struct Cpu {
    pub reg: Registers,
    flags: StatusRegister,
    cycles: u16,
    opcode: u8,
}

impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}\t{}\t{}\t{}\t{}\t{}{} {} {} {} {} {}",
                 "Opcode","PC","SP","A","X","Y\t", "N","D","I","Z","C","Cycles")?;
        writeln!(f, "{:04x}\t{:04x}\t{:04x}\t{:02x}\t{:04x}\t{:04x}\t{} {} {} {} {} {}",
                 self.opcode, self.reg.pc, self.reg.sp, self.reg.a, self.reg.x, self.reg.y,
                 self.flags.negative, self.flags.decimal, self.flags.interrupt, self.flags.zero, self.flags.carry, self.cycles)?;
        Ok(())
    }
}

impl Cpu {
    pub fn default() -> Cpu {
        Cpu {
            reg: Registers {
                pc: 0,
                sp: 0,
                a: 0,
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
    pub ram: Ram,
}

impl ExecutionContext {
    pub fn new() -> ExecutionContext {
        ExecutionContext {
            cpu: Cpu::default(),
            cart: Cartridge::default(),
            ram: Ram::default(),
        }
    }
    pub fn setup_pc(&mut self, addr: u16) {
        let return_addr = match addr {
            0 ... 0x07ff => {
                println!("\n Addr RAM:{:04x}", self.ram.memory[addr as usize]);
                self.ram.memory[addr as usize]
            },
            0x0800 ... 0x1fff => {
                println!("\n Addr RAM Mirror:{:04x}", self.ram.memory[addr as usize & 0x07ff]);
                self.ram.memory[addr as usize & 0x07ff]
            },
            0x8000 ... 0xffff => {
                println!("\n Indexed addr PRG:{:04x}, addr:{:04x}", self.cart.prg[addr as usize & 0x7fff], addr);
                // self.cart.prg[addr as usize & 0x7fff]
                // Read word in prg rom
                self.cart.prg[(((addr as u16) << 8) | (addr as u16)) as usize & 0x7fff]
            },
            _ => panic!("Unrecognized read address: {:04x}", addr)
        };
        self.cpu.reg.pc = return_addr as u16;
    }

    fn adv_pc(&mut self, t: u16) { self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(t); }
    fn adv_cycles(&mut self, t: u16) { self.cpu.cycles += t; }

    pub fn decode(&mut self) {
        // Instruction::decode(self.cart.prg[self.cpu.reg.pc as usize]);
        // let opcode = self.read(self.cpu.reg.pc);

        // Read opcode from program memory
        let opcode = self.cart.prg[self.cpu.reg.pc as usize];

        // println!("PRG Contents:{:?}", self.cart.prg);


        match opcode {
            0x00 => self.brk(),
            0x01 => self.bpl(),
            0x06 => self.asl(),
            0xa1 => self.lda(),
            0xa2 => self.ldax(),
            0x4e => self.lsr(),
            0x1a => self.rol(),
            0x20 => self.jsr(),
            0x5a => self.nop(),
            0x61 => self.rts(),
            0x65 => self.adc(),
            0x6c => self.jmp(),
            0x70 => self.bvs(),
            0x72 => self.nop(),
            0x73 => self.nop(),
            _ => eprintln!("Not implemented"),
        }
        self.cpu.opcode = opcode as u8;
        self.adv_pc(1);

        println!("{:?}", self.cpu);
    }
    fn adc(&mut self) {
        println!("ADC");
        self.adv_pc(1);
        let mem_value = self.ram.memory[self.cpu.reg.pc as usize];
        let result = (self.cpu.reg.a as u16).wrapping_add(mem_value as u16).wrapping_add(self.cpu.flags.carry as u16);
        // TODO Flags
        self.cpu.flags.carry = (result & 0x0100) as u8;
        self.adv_cycles(3)
    }
    // Arithmetic shift left zero page
    fn asl(&mut self) {
        println!("ASL");
        let carry = (self.cpu.reg.a & 1) != 0;
        let value = self.ram.memory[self.cpu.reg.pc as usize];
        // Check if 7th bit has been set
        let new_carry = (value & 0x80) != 0;
        // Shift left by one
        let mut result = value << 1;

        self.cpu.flags.carry = new_carry as u8;

        self.cpu.flags.negative;
        self.cpu.flags.zero;
        self.cpu.flags.carry = carry as u8;
        self.adv_pc(1);
        self.adv_cycles(6);
    }
    fn nop(&mut self) {
        self.adv_pc(1);
    }
    fn brk(&mut self) {
        println!("BRK");
        self.cpu.flags.brk = 1;
        self.adv_pc(1);
        self.adv_cycles(7);
    }
    // Branch if Overflow set
    fn bvs(&mut self) {
        println!("BVS");
        if self.cpu.flags.overflow == 1 {
            self.adv_pc(1);
            self.adv_cycles(3);
        } else {
            self.adv_cycles(2);
        }
    }
    // LDA A8
    fn lda(&mut self) {
        // let imm = self.read_byte(self.cpu.reg.pc + 1);
        let imm = self.ram[self.cpu.reg.pc + 1];
        self.cpu.reg.a = imm as u8;
        self.cpu.flags.zero == self.cpu.reg.a;
        self.adv_pc(1);
        self.adv_cycles(3)
    }
    // LDA (A8, X)
    fn ldax(&mut self) {
        println!("LDA (A8, X)");
        self.adv_cycles(1);
        self.adv_cycles(6);
    }
    // LDA (A8, Y)
    fn lday(&mut self) {
        println!("LDA (A8, Y)");
    }
    // Logical Shift Right
    fn lsr(&mut self) {
        println!("LSR");
        // Flags affected
        let carry = (self.cpu.reg.a & 1) != 0;
        self.cpu.reg.a = (self.cpu.reg.a >> 1) | ((self.cpu.flags.carry as u8) << 7);
        self.cpu.flags.negative;
        self.cpu.flags.zero;
        self.cpu.flags.carry = carry as u8;
        self.adv_pc(2);
        self.adv_cycles(6);
    }

    fn bpl(&mut self) {
        println!("BPL");
        // Cycles 3+ / 2
        self.adv_pc(2);
        self.adv_cycles(3);
    }
    fn rol(&mut self) {
        println!("ROL");
        self.adv_pc(1);
        self.adv_cycles(5);
    }
    fn rts(&mut self) {
        self.adv_cycles(6);
    }

    fn push_stack(&mut self, value: u16) {
        let sp = self.cpu.reg.sp;
        self.write_word(0x100 + (sp.wrapping_sub(1)) as u16, value);
        self.cpu.reg.sp = self.cpu.reg.sp.wrapping_sub(2);
    }
    // Jump to Subroutine
    fn jsr(&mut self) {
        // Get value at word in PC & advance pc by 2
        let addr = self.read_word(self.cpu.reg.pc);
        println!("JSR");
        self.adv_pc(2);

        // Push to stack
        let pc = self.cpu.reg.pc;
        self.push_stack(pc - 1);
        self.adv_cycles(3); // 6 if no jump?
        self.cpu.reg.pc = addr;
    }
    fn jmp(&mut self) {
        println!("JMP");
        let addr = self.cpu.reg.pc + 2;
        self.cpu.reg.pc = self.read_word(addr);
        self.adv_cycles(3);
    }
}
