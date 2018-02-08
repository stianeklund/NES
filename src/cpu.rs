use std::fmt;
use opcode::Instruction;
use interconnect::MemoryMapper;
use memory::{Ram};
use rom::Cartridge;
use ppu::Ppu;

impl MemoryMapper for ExecutionContext {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            // See https://wiki.nesdev.com/w/index.php/CPU_memory_map
            0...0x07ff => self.ram.memory[addr as usize] as u8,
            // 0...0x2007 => self.ppu[addr] // TODO
            // RAM Mirror
            0x0800...0x1fff => self.ram.memory[addr as usize & 0x07ff],
            0x2000 ... 0x3fff => self.ppu.vram[addr as usize & 0x2efff],
            0x8000...0xffff => {
                let mut mask_amount = 0;
                if self.cart.header.prg_rom_size == 1 {
                    mask_amount = 0x3fff;
                } else {
                    mask_amount = 0x7fff;
                }
                self.cart.prg[addr as usize & mask_amount]
            },
            _ => panic!("Unrecognized addr: {:04x}", addr)
        }

    }

    fn write(&mut self, addr: u16, byte: u8) {
        match addr {
            0...0x07ff => self.ram.memory[addr as usize] = byte,
            0x0800...0x1fff => self.ram.memory[addr as usize & 0x07ff] = byte,
            // $2000-2FFF is normally mapped to the 2kB NES internal VRAM,
            // providing 2 nametables with a mirroring configuration controlled by the cartridge,
            // but it can be partly or fully remapped to RAM on the cartridge,
            // allowing up to 4 simultaneous nametables.
            0x2000 ... 0x3fff => self.ppu.vram[addr as usize & 0x2efff] = byte,
            0x8000...0xffff => self.cart.prg[addr as usize & 0x3fff] = byte,
            _ => eprintln!("Unable to write to memory address"),
        };
    }
}

#[derive(Debug)]
pub struct StatusRegister {
    negative: bool,
    overflow: bool,
    bit5: bool,      // always 1
    brk: bool,
    decimal: bool,
    interrupt: bool,
    zero: bool,
    carry: bool,
}

#[derive(Debug)]
pub struct Registers {
    pub pc: u16,
    pub prev_pc: u16,
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
            prev_pc: 0,
            sp: 0,
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
                 "Opcode","PC","SP","A","X","Y\t", "N\t","D\t","I\t","Z\t","C\t","Cycles")?;
        writeln!(f, "{:04x}\t{:04x}\t{:04x}\t{:02x}\t{:02x}\t{:02x}\t{}\t{}\t{}\t{}\t{}\t{}",
                 self.opcode, self.reg.prev_pc, self.reg.sp, self.reg.a, self.reg.x, self.reg.y,
                 self.flags.negative, self.flags.decimal, self.flags.interrupt, self.flags.zero, self.flags.carry, self.cycles)?;
        Ok(())
    }
}

impl Cpu {
    pub fn default() -> Cpu {
        Cpu {
            reg: Registers {
                pc: 0,
                prev_pc: 0,
                sp: 0, // 0xfd,
                a: 0,
                x: 0,
                y: 0,
                status: 0,
            },
            flags: StatusRegister {
                negative: false,
                overflow: false,
                bit5: true,
                brk: false,
                decimal: false,
                interrupt: false,
                zero: false,
                carry: false,
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
    pub ppu: Ppu,
}
#[derive(Debug, PartialEq)]
enum AddressMode {
    Accumulator, // Not really an addressing mode but used here for convenience to access it easily
    ZeroPage,    // Zero Page addressing, $00nn
    ZeroPageX,   // $00nn + X
    ZeroPageY,   // $00nn + y
    Immediate,   // Immediate addressing; immediately following the opcode.
    Absolute,    // Absolute addressing. Fetches the next 2 mem slots & combines them into a word.
    AbsoluteX,   // X indexed. Fetches the next 2 mem slots & combines them into a word, then adds X.
    AbsoluteY,   // Y indexed. Fetches the next 2 mem slots & combines them into a word, then adds Y.
    Indirect,    // Indirect addressing; special for JMP (Not really implemented or needed?)
    IndirectX,   // X indexed addressing.
    IndirectY,   // Y indexed addressing.
}

impl ExecutionContext {
    pub fn new() -> ExecutionContext {
        ExecutionContext {
            cpu: Cpu::default(),
            cart: Cartridge::default(),
            ram: Ram::default(),
            ppu: Ppu::default(),
        }
    }
    fn adv_pc(&mut self, t: u16) { self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(t); }
    fn adv_cycles(&mut self, t: u16) { self.cpu.cycles = self.cpu.cycles.wrapping_add(t); }

    pub fn decode(&mut self) {
        let opcode = self.read(self.cpu.reg.pc);

        match opcode {
            0x00 => self.brk(),
            0x01 => self.bpl(),
            0x04 => self.rti(),
            0x06 => self.asl(),
            0x07 => self.slo(AddressMode::ZeroPage),
            0x10 => self.bpl(),
            0xa0 => self.ldy(AddressMode::Immediate),
            0xa1 => self.lda(AddressMode::IndirectX),
            0xa2 => self.lda(AddressMode::Immediate),
            0xa5 => self.lda(AddressMode::ZeroPage),
            0xad => self.lda(AddressMode::Absolute),
            0xa9 => self.lda(AddressMode::Immediate),
            0xb1 => self.lda(AddressMode::IndirectY),
            0xb5 => self.lda(AddressMode::ZeroPageX),
            0xb8 => self.clv(),
            0xb9 => self.lda(AddressMode::AbsoluteY),
            0xbd => self.lda(AddressMode::AbsoluteX),
            0x40 => self.rti(),
            0x4e => self.lsr(),
            0x48 => self.pha(),
            0x2a => self.rol(AddressMode::Accumulator),
            0x2e => self.rol(AddressMode::Absolute),
            0x20 => self.jsr(),
            0x24 => self.bit(AddressMode::ZeroPage),
            0x29 => self.and(AddressMode::Immediate),
            0x2d => self.and(AddressMode::Absolute),
            0x3e => self.rol(AddressMode::AbsoluteX),
            0x4c => self.jmp(),
            0x5a => self.nop(),
            0x60 => self.rts(),
            0x61 => self.adc(AddressMode::IndirectX),
            0x64 => self.dop(AddressMode::ZeroPage),
            0x65 => self.adc(AddressMode::ZeroPage),
            0x68 => self.pla(),
            0x6c => self.jmp(),
            0x6d => self.adc(AddressMode::Absolute),
            0x69 => self.adc(AddressMode::Immediate),
            0x6e => self.lsr(),
            0x70 => self.bvs(),
            0x72 => self.nop(),
            0x73 => self.nop(),
            0x78 => self.sei(),
            0x84 => self.sty(AddressMode::ZeroPage),
            0x85 => self.sta(AddressMode::ZeroPage),
            0x86 => self.stx(AddressMode::ZeroPage),
            0x8d => self.sta(AddressMode::Absolute),
            0x8e => self.stx(AddressMode::Absolute),
            0x91 => self.sta(AddressMode::IndirectY),
            0x9a => self.txs(),
            0x9d => self.sta(AddressMode::AbsoluteX),
            0xc3 => self.dcp(),
            0xc6 => self.dec(AddressMode::ZeroPage),
            0xc9 => self.cmp(AddressMode::Immediate),
            0xce => self.dec(AddressMode::Absolute),
            0xd0 => self.bne(),
            0xd3 => self.dcp(),
            0xd6 => self.dec(AddressMode::ZeroPageX),
            0xd8 => self.cld(),
            0xde => self.dec(AddressMode::AbsoluteX),
            0xdf => self.dcp(),
            0xe6 => self.inc(AddressMode::ZeroPage),
            0xe8 => self.inx(),
            0xec => self.cpx(),
            0xc8 => self.iny(),
            0xf0 => self.beq(),
            0xf6 => self.inc(AddressMode::ZeroPage),
            0xf7 => self.isc(AddressMode::ZeroPageX),
            0xff => self.isc(AddressMode::AbsoluteX),
            _ => eprintln!("Not implemented"),
        }
        self.cpu.opcode = opcode as u8;
        self.cpu.reg.prev_pc = self.cpu.reg.pc;

        println!("{}", Instruction::mnemonic(opcode));
        // Debug print CPU values
        println!("{:?}", self.cpu);
    }
    // TODO
    fn adc(&mut self, mode: AddressMode) {
        let mem_value = self.read(self.cpu.reg.pc);
        let result = (self.cpu.reg.a as u16).wrapping_add(mem_value as u16).wrapping_add(self.cpu.flags.carry as u16);
        self.cpu.reg.a = result as u8;

        // TODO Overflow flag?
        self.cpu.flags.carry = (self.cpu.reg.a & 0x01) != 0;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.cpu.flags.zero = (self.cpu.reg.a & 0xff) == 0;

        match mode {
            AddressMode::Accumulator => {},
            AddressMode::ZeroPage => {},
            AddressMode::ZeroPageX => {},
            AddressMode::ZeroPageY => {},
            AddressMode::Immediate => { self.adv_pc(2); self.adv_cycles(2); },
            AddressMode::Absolute => { self.adv_pc(3); self.adv_cycles(4); },
            AddressMode::AbsoluteX => {},
            AddressMode::AbsoluteY => {},
            AddressMode::Indirect => {},
            AddressMode::IndirectX => {},
            AddressMode::IndirectY => {},
        }
    }
    // Arithmetic shift left zero page
    fn asl(&mut self) {
        let carry = (self.cpu.reg.a & 1) != 0;
        let value = self.ram.memory[self.cpu.reg.pc as usize];
        // Check if 7th bit has been set
        // Shift left by one
        let mut result = value << 1;

        self.cpu.flags.carry = (value & 0x80) != 0;
        self.cpu.flags.negative;
        self.cpu.flags.zero;
        self.cpu.flags.carry = carry;
        self.adv_pc(2);
        self.adv_cycles(6);
    }
    // TODO Implement different byte lengths & cycle sizes for each addressing mode
    fn and(&mut self, mode: AddressMode) {
        let mut pc = self.cpu.reg.pc;
        let data = match mode {
            AddressMode::Immediate => { self.read(pc + 1) as u16 },
            AddressMode::ZeroPage  => { self.read(pc + 1) as u16 & 0xff }
            AddressMode::ZeroPageX => { self.read(pc + 1) as u16 & 0xff + self.cpu.reg.x as u16},
            AddressMode::Absolute  => { self.read_word(pc + 1) },
            AddressMode::AbsoluteX => { self.read_word(pc + 1) + self.cpu.reg.x as u16 },
            AddressMode::AbsoluteY => { self.read_word(pc + 1) + self.cpu.reg.y as u16 },
            AddressMode::IndirectX => { unimplemented!() },
            AddressMode::IndirectY => { unimplemented!() },
            _ => unreachable!(),
        };

        let result = self.cpu.reg.a as u16 & data;
        self.cpu.reg.a =  result as u8; // ((self.cpu.reg.a as u16) & data) as u8;

        self.adv_pc(2);
        self.adv_cycles(2);

        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.cpu.flags.zero = (self.cpu.reg.a & 0xff) == 0;
    }

    // Branch on Equal
    fn beq(&mut self) {
        if self.cpu.flags.zero {
            let offset = self.read(self.cpu.reg.pc + 1);
            println!("Offset:{:02x}", offset);
            self.cpu.reg.pc = (self.cpu.reg.pc as i16 + offset as i16) as u16;
            self.adv_cycles(1);
        } else {
            self.adv_pc(2);
        }
        self.adv_cycles(2);
    }
    // Branch on Plus (if positive)
    fn bpl(&mut self) {
        // Cycles 3+ / 2
        if !self.cpu.flags.negative {
            let offset = self.read(self.cpu.reg.pc + 1);
            self.cpu.reg.pc += offset as u16;
            self.adv_cycles(3);
        } else {
            println!("Not branching");
            self.adv_pc(2);
            self.adv_cycles(2);
        }
    }

    fn brk(&mut self) {
        self.cpu.flags.brk = true;
        self.push_word(self.cpu.reg.pc + 1);
        self.adv_pc(2);
        self.cpu.reg.pc = self.read_word(0xfffe);
        self.adv_cycles(7);
    }
    // Test Bits N Z V
    fn bit(&mut self, mode: AddressMode) {
        match mode {
            AddressMode::ZeroPage => {
                // BIT $44 HEX: $24, LEN: 2, TIME: 3
                let value = self.read(self.cpu.reg.pc + 1) & 0xff;
                let a = self.cpu.reg.a;
                self.cpu.flags.zero = (value & a) == 0;
                self.cpu.flags.negative= (value & 0x80) != 0;
                self.cpu.flags.overflow = (value & 0x40) != 0;
                self.adv_cycles(3);
                self.adv_pc(2);
            }
            _ => eprintln!("{:?} not covered", mode),
        }
    }
    fn bne(&mut self) {
        // If zero flag is 0 branch
        if !self.cpu.flags.zero {
            // Print possible branch offsets
            let offset = self.read(self.cpu.reg.pc + 1) as i16;
            self.cpu.reg.pc += offset as u16;
            // self.adv_pc(2);
            self.adv_cycles(3);
        } else {
            self.adv_cycles(2)}
            self.adv_pc(2);
    }
    // Branch on overflow set
    fn bvs(&mut self) {
        if self.cpu.flags.overflow {
            self.adv_pc(2);
            self.adv_cycles(3);
        } else {
            self.adv_cycles(2);
            self.adv_pc(1);
        }
    }
    // Clear carry
    fn clc(&mut self) {
        self.cpu.flags.carry = false;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // Clear decimal
    fn cld(&mut self) {
        self.cpu.flags.decimal = false;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // Clear Interrupt Disable
    fn cli(&mut self) {
        self.cpu.flags.interrupt = false;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // Clear overflow
    fn clv(&mut self) {
        self.cpu.flags.overflow = false;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    fn cmp(&mut self, mode: AddressMode) {
        match mode {
            AddressMode::Accumulator => {},
            AddressMode::ZeroPage => {},
            AddressMode::ZeroPageX => {},
            AddressMode::ZeroPageY => {},
            AddressMode::Immediate => {
                let value = self.read(self.cpu.reg.pc + 1);
                let result = self.cpu.reg.a - value;

                self.cpu.flags.negative = (result & 0x80) != 0;
                self.cpu.flags.zero = (result & 0xff) == 0;
                self.cpu.flags.carry = (result & 0x01) != 0;
                self.adv_cycles(2);
                self.adv_pc(2);
            },
            AddressMode::Absolute => {},
            AddressMode::AbsoluteX => {},
            AddressMode::AbsoluteY => {},
            AddressMode::Indirect => {},
            AddressMode::IndirectX => {},
            AddressMode::IndirectY => {},
        }
    }

    fn cpm(&mut self) {
        unimplemented!();
    }
    fn cpx(&mut self) {
        unimplemented!();
    }
    fn dec(&mut self, mode: AddressMode) {
        // TODO
        match mode {
            AddressMode:: Accumulator => {},
            AddressMode::ZeroPage => {},
            AddressMode::ZeroPageX => {},
            AddressMode::ZeroPageY => {},
            AddressMode::Immediate => {},
            AddressMode::Absolute => {},
            AddressMode::AbsoluteX => {},
            AddressMode::AbsoluteY => {},
            AddressMode::Indirect => {},
            AddressMode::IndirectX => {},
            AddressMode::IndirectY => {},
        }
        let mut data = self.read_word(self.cpu.reg.pc + 1);

        // Decrement memory value to be compared
        data -= 1;

        // Should decremented value be stored anywhere?
        // self.ram[data];
        self.cpu.flags.zero = (data & 0xff) == 0;
        // If bit 7 of the decremented result is set set flag
        self.cpu.flags.negative = (data & 0x80) != 0;
        self.adv_pc(3);
        self.adv_cycles(7);

    }
    // Decrement & compare
    fn dcp(&mut self) {
        println!("DCP (illegal opcode)");
        let data = self.cart.prg[self.cpu.reg.pc as usize + 2];
        println!("Data:{:04x}", data);
        self.adv_pc(2);
        self.adv_cycles(7);
    }
    // Double NOP
    fn dop(&mut self, mode: AddressMode) {
        match mode {
            AddressMode::Accumulator => {},
            AddressMode::ZeroPage => { self.adv_cycles(3); self.adv_pc(2)},
            AddressMode::ZeroPageX => {},
            AddressMode::ZeroPageY => {},
            AddressMode::Immediate => {},
            AddressMode::Absolute => {},
            AddressMode::AbsoluteX => {},
            AddressMode::AbsoluteY => {},
            AddressMode::Indirect => {},
            AddressMode::IndirectX => {},
            AddressMode::IndirectY => {},
        }

    }
    fn ldy(&mut self, mode: AddressMode) {
        // TODO Handle page boundary crossing
        // + 1 cycle if page boundary is crossed
        match mode {
            AddressMode::Absolute => {
                // LDA A16
                let data = self.read_word(self.cpu.reg.pc + 1);
                self.cpu.reg.y = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_cycles(4);
                self.adv_pc(3);
            },
            AddressMode::AbsoluteX => {
                let data = self.read_word(self.cpu.reg.pc + 1) + self.cpu.reg.x as u16;
                self.cpu.reg.y = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_cycles(6);
                self.adv_pc(2);
            },
            AddressMode::AbsoluteY => {
                let data = self.read_word(self.cpu.reg.pc + 1) + self.cpu.reg.y as u16;
                self.cpu.reg.y = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_cycles(4);
                self.adv_pc(3);
            }
            AddressMode::IndirectX => {
                unimplemented!();
                let data = self.read_word(self.cpu.reg.pc + 1);
                self.cpu.reg.y = data as u8;
                // TODO Cycles is 5 if page boundry is crossed
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_cycles(2);
                self.adv_pc(2);
            }
            AddressMode::IndirectY => {
                let data = self.read_word(self.cpu.reg.pc + 1);
                self.cpu.reg.y = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                // TODO Cycles is 5 if page boundry is crossed
                self.adv_cycles(2);
                self.adv_pc(2);
                unimplemented!();
            }
            AddressMode::Immediate => {
                // LDA #d8
                let data = self.read(self.cpu.reg.pc + 1);
                self.cpu.reg.y = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_pc(2);
                self.adv_cycles(2);
            }
            AddressMode::ZeroPage => {
                let data = self.read_word(self.cpu.reg.pc + 1) & 0xff;
                self.cpu.reg.a = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_pc(2);
                self.adv_cycles(2);
            }
            AddressMode::ZeroPageX => {
                // TODO Handle boundary crossing
                let data = (self.read_word(self.cpu.reg.pc + 1) & 0xff) + self.cpu.reg.x as u16;
                self.cpu.reg.a = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_pc(2);
                self.adv_cycles(2);
            }
            AddressMode::ZeroPageY => {
                // TODO Handle boundary crossing
                let data = (self.read_word(self.cpu.reg.pc + 1) & 0xff) + self.cpu.reg.y as u16;
                self.cpu.reg.a = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_pc(2);
                self.adv_cycles(2);
            }
            _ => eprintln!("Not included"),
        }

    }
        fn lda(&mut self, mode: AddressMode) {
        // TODO Handle page boundary crossing
        // + 1 cycle if page boundary is crossed
        match mode {
            AddressMode::Absolute => {
                // LDA A16
                let data = self.read_word(self.cpu.reg.pc + 1);
                self.cpu.reg.a = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_cycles(4);
                self.adv_pc(3);
            },
            AddressMode::AbsoluteX => {
                let data = self.read_word(self.cpu.reg.pc + 1) + self.cpu.reg.x as u16;
                self.cpu.reg.a = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                // TODO Add 1 cycle if boundary is crossed
                self.adv_cycles(4);
                self.adv_pc(3);
            },
            AddressMode::AbsoluteY => {
                let data = self.read_word(self.cpu.reg.pc + 1) + self.cpu.reg.y as u16;
                self.cpu.reg.a = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                // TODO Add 1 cycle if boundary is crossed
                self.adv_cycles(4);
                self.adv_pc(3);
            }
            AddressMode::IndirectX => {
                // TODO Is this correct?
                let data = self.read_word(self.cpu.reg.pc + 1);
                // Our target address
                let addr = self.cpu.reg.x as u16 + data;
                // Read value from our target address & assign to accumulator
                self.cpu.reg.a = self.read(addr) & 0xff;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_cycles(6);
                self.adv_pc(2);
            }
            AddressMode::IndirectY => {
                // TODO Is this correct?
                let data = self.read_word(self.cpu.reg.pc + 1);
                // Our target address
                let addr = self.cpu.reg.y as u16 + data;
                // Read value from our target address & assign to accumulator
                self.cpu.reg.a = self.read(addr) & 0xff;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                // TODO Cycles is 6 if page boundry is crossed?
                self.adv_cycles(5);
                self.adv_pc(2);
            }
            AddressMode::Immediate => {
                // LDA #d8
                let data = self.read(self.cpu.reg.pc + 1);
                self.cpu.reg.a = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_pc(2);
                self.adv_cycles(2);
            }
            AddressMode::ZeroPage => {
                let data = self.read_word(self.cpu.reg.pc + 1) & 0xff;
                self.cpu.reg.a = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_pc(2);
                self.adv_cycles(3);
            }
            AddressMode::ZeroPageX => {
                let data = (self.read_word(self.cpu.reg.pc + 1) & 0xff) + self.cpu.reg.x as u16;
                self.cpu.reg.a = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_pc(2);
                self.adv_cycles(4);
            }
            AddressMode::ZeroPageY => {
                let data = (self.read_word(self.cpu.reg.pc + 1) & 0xff) + self.cpu.reg.y as u16;
                self.cpu.reg.a = data as u8;
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_pc(2);
                self.adv_cycles(3);
            }

            _ => eprintln!("Not included"),
        }

    }
    // Logical Shift Right
    fn lsr(&mut self) {
        // Flags affected
        let carry = (self.cpu.reg.a & 1) != 0;
        self.cpu.reg.a = (self.cpu.reg.a >> 1) | ((self.cpu.flags.carry as u8) << 7);
        self.cpu.flags.negative;
        self.cpu.flags.zero;
        self.cpu.flags.carry = carry;
        self.adv_pc(3);
        self.adv_cycles(6);
    }
    fn nop(&mut self) {
        self.adv_pc(1);
        self.adv_cycles(2);
    }

    // Rotate one bit right memory or accumulator
    fn rol(&mut self, mode: AddressMode) {

        let mut pc = self.cpu.reg.pc;

        let mut src: u16 = match mode {
            AddressMode::Accumulator => {
                self.adv_cycles(2);
                self.adv_pc(1);
                self.cpu.reg.a as u16
            }
            AddressMode::ZeroPage => {
                self.adv_cycles(5);
                self.adv_pc(2);
                self.read_word(pc) & 0xff as u16
            }
            AddressMode::ZeroPageX => {
                self.adv_cycles(6);
                self.adv_pc(2);
                self.read_word(pc) & 0xff + self.cpu.reg.x as u16
            }
            AddressMode::Absolute => {
                self.adv_cycles(6);
                self.adv_pc(3);
                self.read_word(pc) as u16
            }
            AddressMode::AbsoluteX => {
                self.adv_cycles(7);
                self.adv_pc(3);
                self.read_word(pc) + self.cpu.reg.x as u16
            }
            _ => { unreachable!("Address mode {:?} should not be called on ROL", mode) }
        };

        // Original bit is shifted into carry slot & carry is shifted into bit 7.
        let result = src << 1 & 0xfe;

        // The program counter is already modified by this point by the above `match`.
        let addr = self.cpu.reg.pc;

        // Write result to memory address or accumulator
        println!("Writing :{:04x} to address:{:04x}", result, addr);
        if mode != AddressMode::Accumulator {
            self.write(addr, result as u8);
        } else {
            self.cpu.reg.a = result as u8;
        }

        // Set flag values
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.carry = (result & 0x01) != 0;

        // self.cpu.flags.interrupt = result & 0x04;
        // self.cpu.flags.decimal = result & 0x08;
        // self.cpu.flags.overflow = result & 0x40;
    }
    fn ror(&mut self, mode: AddressMode) {
        unimplemented!();
        match mode {
            AddressMode::Accumulator => {},
            AddressMode::ZeroPage => {},
            AddressMode::ZeroPageX => {},
            AddressMode::ZeroPageY => {},
            AddressMode::Immediate => {},
            AddressMode::Absolute => {},
            AddressMode::AbsoluteX => {},
            AddressMode::AbsoluteY => {},
            AddressMode::Indirect => {},
            AddressMode::IndirectX => {},
            AddressMode::IndirectY => {},
        }

    }
    fn rts(&mut self) {
        let low = self.read(self.cpu.reg.sp as u16);
        let high = self.read(self.cpu.reg.sp.wrapping_add(1) as u16);
        let mut ret: u16 = (high as u16) << 8 | (low as u16);
        // Set program counter for debug output
        self.cpu.reg.prev_pc = self.cpu.reg.pc;

        println!("Returning to ${:04X}", ret);
        self.cpu.reg.pc = ret as u16;

        self.cpu.reg.sp = self.cpu.reg.sp.wrapping_add(2);
        self.adv_pc(1);
        self.adv_cycles(6);
    }
    fn rti(&mut self) {
        // Return from interrupt
        // TODO store flags & pop pc
        self.adv_pc(1);
        self.adv_cycles(6);
    }
    // Shift left one bit in memory, then OR accumulator with memory.
    // Part of undocumented opcodes
    fn slo(&mut self, mode: AddressMode) {
        unimplemented!()
    }
    fn sta(&mut self, mode: AddressMode) {
        // Ex: Absolute: STA $4400 Hex: $8D Len: 3 Cycles:4
        match mode {
            AddressMode::Absolute => {
                let addr = self.read_word(self.cpu.reg.pc + 1);
                // Write value of accumulator to memory address
                self.write(addr, self.cpu.reg.a);
                self.adv_cycles(4);
                self.adv_pc(3);
            },
            AddressMode::AbsoluteX => {
                let addr = self.read_word(self.cpu.reg.pc + 1) + self.cpu.reg.x as u16;
                self.write(addr, self.cpu.reg.a);
                self.adv_cycles(4);
                self.adv_pc(3);
            },
            AddressMode::IndirectY => {
                let value = self.cpu.reg.pc + 1;
                let addr = self.read_word(value) + self.cpu.reg.y as u16;
                self.write(addr, self.cpu.reg.a);
                self.adv_cycles(4);
                self.adv_pc(2);
            },
            AddressMode::ZeroPage=> {
                // Mask the upper two bytes
                let addr = self.read_word(self.cpu.reg.pc + 1) & 0xff;
                // Write value of accumulator to memory address
                self.write(addr, self.cpu.reg.a);
                self.adv_cycles(3);
                self.adv_pc(2);
            }
            _ => eprintln!("{:?} not covered", mode),
        }
    }
    fn sty(&mut self, mode: AddressMode) {
        // Ex: Absolute: STA $4400 Hex: $8D Len: 3 Cycles:4
        match mode {

            AddressMode::Absolute => {
                let addr = self.read_word(self.cpu.reg.pc + 1);
                let y = self.cpu.reg.y;
                self.write(addr, y);
                self.adv_pc(2);
                self.adv_cycles(3);
            },
            AddressMode::ZeroPage=> {
                let addr = self.read_word(self.cpu.reg.pc + 1);
                let y = self.cpu.reg.y;
                // Write value of accumulator to memory address
                self.write(addr, y);
                self.adv_pc(2);
                self.adv_cycles(3);
            }
            _ => eprintln!("{:?} not covered", mode),
        }
    }
     fn stx(&mut self, mode: AddressMode) {
        // Ex: Absolute: STA $4400 Hex: $8D Len: 3 Cycles:4
        match mode {
            AddressMode::Absolute => {
                let addr = self.read_word(self.cpu.reg.pc + 1);
                // Write value of accumulator to memory address
                self.write(addr, self.cpu.reg.x);
                self.adv_cycles(4);
                self.adv_pc(3);
            }
            AddressMode::ZeroPage => {
                let addr = self.read_word(self.cpu.reg.pc + 1) & 0xff;
                // Write value of accumulator to memory address
                self.write(addr, self.cpu.reg.x);
                self.adv_cycles(3);
                self.adv_pc(2);
            }
            _ => eprintln!("{:?} not covered", mode),
        }
    }

    // Transfer X to Stack Pointer
    fn txs(&mut self) {
        // TODO TXS is like POP?
        self.push_byte(self.cpu.reg.x);
        self.adv_pc(1);
        self.adv_cycles(2);
    }

    fn push_word(&mut self, value: u16) {
        let sp = self.cpu.reg.sp;
        self.write_word(0x100 + (sp.wrapping_sub(1)) as u16, value);
        self.cpu.reg.sp = self.cpu.reg.sp.wrapping_sub(2);
    }
    // Push register
    fn push_byte(&mut self, byte: u8) {
        let sp = self.cpu.reg.sp;
        self.write(0x100 + sp as u16, byte);
        self.cpu.reg.sp = self.cpu.reg.sp.wrapping_sub(1);
    }
    fn pull_byte(&mut self, byte: u8) {
        let sp = self.cpu.reg.sp;
        self.write(0x100 + sp as u16, byte);
        self.cpu.reg.sp = self.cpu.reg.sp.wrapping_add(1);
    }
    // Push accumulator
    fn pha(&mut self) {
        self.push_byte(self.cpu.reg.a);
        self.adv_pc(1);
        self.adv_cycles(3);
    }

    // PuL1 (POP) Accumulator
    fn pla(&mut self) {
        // Pulls an 8-bit value from the stack and into the accumulator.
        // Flags affected zero & negative
        //  self.cpu.reg.sp + 1;
        self.pull_byte(self.cpu.reg.sp);
        let value = self.cpu.reg.sp;
        self.cpu.flags.zero = (value & 0xff) == 0;
        self.cpu.flags.negative = (value & 0x80) != 0;

        self.cpu.reg.a = value;




        self.adv_pc(1);
        self.adv_cycles(4);
    }
    // Increment Memory
    fn inc(&mut self, mode: AddressMode) {
        match mode {
            AddressMode::ZeroPage => {
                // Opcode e6
                // Mask the upper two bytes

                let value = self.read(self.cpu.reg.pc.wrapping_add(1)) as u16 & 0xff;
                // let mut value = self.read(self.cpu.reg.pc + 1) as u16 & 0xff;
                // let value = self.cpu.reg.pc.wrapping_add(1);
                let addr = self.read_word(value) as u8;
                self.cpu.flags.zero = (value & 0xff) == 0;
                self.cpu.flags.negative = (value & 0x80) != 0;
                self.write(value, addr);
                self.adv_cycles(5);
                self.adv_pc(2);
            }
            AddressMode::ZeroPageX => {},
            AddressMode::ZeroPageY => {},
            AddressMode::Immediate => {},
            AddressMode::Absolute => {},
            AddressMode::AbsoluteX => {},
            AddressMode::AbsoluteY => {},
            AddressMode::Indirect => {},
            AddressMode::IndirectX => {},
            AddressMode::IndirectY => {},
            _ => eprintln!("{:?} not covered", mode)
        }

    }
    // Increment X (implied mode)
    fn inx(&mut self) {
        self.cpu.reg.x = self.cpu.reg.x.wrapping_add(1);
        self.adv_cycles(2);
        self.cpu.flags.zero = (self.cpu.reg.x & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.x & 0x80) != 0;
        self.adv_pc(1);

    }
    fn iny(&mut self) {
        self.cpu.reg.y = self.cpu.reg.y.wrapping_add(1);
        self.adv_cycles(2);
        self.cpu.flags.zero = (self.cpu.reg.y & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.y & 0x80) != 0;
        self.adv_pc(1);

    }
    // Jump to Subroutine
    fn jsr(&mut self) {
        let addr = self.read_word(self.cpu.reg.pc + 1);
        self.adv_pc(3);

        // Push to stack
        let pc = self.cpu.reg.pc;
        self.cpu.reg.prev_pc = self.cpu.reg.pc;
        self.push_word(pc - 1);
        self.adv_cycles(3); // 6 if no jump?
        self.cpu.reg.pc = addr;
    }
    fn jmp(&mut self) {
        // let pc = self.cpu.reg.pc + 1;
        // let addr = self.read_word(pc);
        self.cpu.reg.prev_pc = self.cpu.reg.pc;
        println!("JMP Addr:{:04x}, next addr:{:04x}", self.read_word(self.cpu.reg.pc), self.read_word(self.cpu.reg.pc + 1));
        self.cpu.reg.pc = self.read_word(self.cpu.reg.pc + 1);
        self.adv_cycles(3);
    }
    fn sei(&mut self) {
        self.cpu.flags.interrupt = true;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // ISC (Increase memory by one)
    fn isc(&mut self, mode: AddressMode) {
        match mode {

            AddressMode::AbsoluteX => {
                let value = self.cpu.reg.pc.wrapping_add(1);
                let addr = self.read_word(value).wrapping_add(self.cpu.reg.x as u16);
                println!("Value:{:04x}, Word:{:04x}", value, addr);
                let result = (self.cpu.reg.a as u16).wrapping_sub(addr as u16).wrapping_sub(self.cpu.flags.carry as u16);
                self.cpu.flags.zero = (result & 0xff) == 0;
                self.cpu.flags.negative = (result & 0x80) != 0;
                // self.cpu.reg.a -= addr;
                self.cpu.reg.a = result as u8;

                self.adv_pc(3);
                self.adv_cycles(7);
            }
            AddressMode::ZeroPageX => {
                let value = self.cpu.reg.pc.wrapping_add(1);
                let addr = self.read_word(value) & 0xff + self.cpu.reg.x as u16;
                println!("Value:{:04x}, Word:{:04x}", value, addr);
                let result = (self.cpu.reg.a as u16).wrapping_sub(addr as u16).wrapping_sub(self.cpu.flags.carry as u16);
                self.cpu.flags.zero = (result & 0xff) == 0;
                self.cpu.flags.negative = (result & 0x80) != 0;
                // self.cpu.reg.a -= addr;
                self.cpu.reg.a = result as u8;

                self.adv_pc(2);
                self.adv_cycles(6);
            }
            _ => eprintln!("Unknown address mode"),
        }
    }
}
