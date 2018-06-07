use std::fmt;
use opcode::Instruction;
use interconnect::MemoryMapper;
use memory::{Ram};
use rom::Cartridge;
use ppu::Ppu;
use apu::Apu;

impl MemoryMapper for ExecutionContext {
    fn read(&self, addr: u16) -> u8 {

        match addr {
            // See https://wiki.nesdev.com/w/index.php/CPU_memory_map
            0...0x07ff => self.ram.memory[addr as usize] as u8,
            0x2000 ... 0x3fff => self.ppu.read(addr),
            0x4000 ... 0x4017 => self.apu.read(addr),
            // $6000-$7FFF = Battery Backed Save or Work RAM
            0x6000 ... 0x7fff => self.ram.sram[addr as usize] as u8,
            0x8000...0xffff => {
                let mut mask_amount = 0;
                if self.cart.header.prg_rom_size == 1 {
                    mask_amount = 0x3fff;
                } else {
                    mask_amount = 0x7fff;
                }
                self.cart.prg[addr as usize & mask_amount]
            },
            _ => unimplemented!("Reads to {:04x} is not implemented", addr),
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

            0x2000 ... 0x3fff => self.ppu.write(addr,byte),
            0x4000 ... 0x4017 => self.apu.write(addr, byte),
            0x6000 ... 0x7fff => {
                // Many CPU tests just store ASCII characters in SRAM
                // Output as characters when writing to SRAM
                self.ram.sram[addr as usize] = byte;
                // Print contents of work ram
                let output = self.ram.sram[addr as usize];
                println!("{}", output as char);
            },
            0x8000...0xffff => self.cart.prg[addr as usize & 0x3fff] = byte,
            _ => eprintln!("Trying to write to memory address {:04x}", addr),
        };
        println!("Writing {:04x} to ${:04x}", byte, addr);
    }
}

#[derive(Debug)]
pub struct StatusRegister {
    negative: bool,
    overflow: bool,
    reserved: bool,
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
                sp: 0,
                a: 0,
                x: 0,
                y: 0,
            },
            flags: StatusRegister {
                negative: false,
                overflow: false,
                reserved: true,
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
    pub apu: Apu,
}
#[derive(Debug, PartialEq)]
enum AddressMode {
    ZeroPage,    // Zero Page addressing, $00nn
    ZeroPageX,   // $00nn + X
    ZeroPageY,   // $00nn + y
    Immediate,   // Immediate addressing; immediately following the opcode.
    Absolute,    // Absolute addressing. Fetches the next 2 mem slots & combines them into a word.
    AbsoluteX,   // X indexed. Fetches the next 2 mem slots & combines them into a word, then adds X.
    AbsoluteY,   // Y indexed. Fetches the next 2 mem slots & combines them into a word, then adds Y.
    Indirect,    // Indirect addressing; special for JMP (Not really implemented or needed?)
    IndirectX,   // Indexed Indirect (only used on X reg). See: http://www.emulator101.com/6502-addressing-modes.html
    IndirectY,   // Indirect Indexed (only used on Y reg) see above URL.
}

impl ExecutionContext {
    pub fn new() -> ExecutionContext {
        ExecutionContext {
            cpu: Cpu::default(),
            cart: Cartridge::default(),
            ram: Ram::default(),
            ppu: Ppu::default(),
            apu: Apu::default(),
        }
    }
    fn adv_pc(&mut self, amount: u16) { self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(amount); }
    fn adv_cycles(&mut self, cycles: u16) { self.cpu.cycles = self.cpu.cycles.wrapping_add(cycles); }

    pub fn decode(&mut self) {
        let opcode = self.read(self.cpu.reg.pc);
        println!("{}", Instruction::mnemonic(opcode));
        // Debug print CPU values
        println!("{:?}", self.cpu);
        match opcode {
            0x00 => self.brk(),
            0x01 => self.bpl(),
            0x03 => self.slo(AddressMode::IndirectX),
            0x04 => self.rti(),
            0x05 => self.ora(AddressMode::ZeroPage),
            0x06 => self.asl(AddressMode::ZeroPage),
            0x07 => self.slo(AddressMode::ZeroPage),
            0x08 => self.php(),
            0x09 => self.ora(AddressMode::Immediate),
            0x10 => self.bpl(),
            0x13 => self.slo(AddressMode::IndirectY),
            0x16 => self.asl(AddressMode::ZeroPageX),
            0x17 => self.slo(AddressMode::ZeroPage),
            0x18 => self.clc(),
            0x1b => self.slo(AddressMode::AbsoluteY),
            0x1f => self.slo(AddressMode::AbsoluteX),
            0x0c => self.nop(),
            0x0e => self.asl(AddressMode::Absolute),
            0x0a => self.asla(),
            0xa0 => self.ldy(AddressMode::Immediate),
            0xa1 => self.lda(AddressMode::IndirectX),
            0xa2 => self.ldx(AddressMode::Immediate),
            0xa4 => self.ldy(AddressMode::ZeroPage),
            0xa5 => self.lda(AddressMode::ZeroPage),
            0xa6 => self.ldx(AddressMode::ZeroPage),
            0xa8 => self.tay(),
            0xa9 => self.lda(AddressMode::Immediate),
            0xaa => self.tax(),
            0xac => self.ldy(AddressMode::Absolute),
            0xad => self.lda(AddressMode::Absolute),
            0xae => self.ldx(AddressMode::Absolute),
            0xaf => self.lax(AddressMode::Absolute),
            0xb0 => self.bcs(),
            0xb1 => self.lda(AddressMode::IndirectY),
            0xb5 => self.lda(AddressMode::ZeroPageX),
            0xb6 => self.ldx(AddressMode::ZeroPageY),
            0xb8 => self.clv(),
            0xb9 => self.lda(AddressMode::AbsoluteY),
            0xba => self.tsx(),
            0xbc => self.ldy(AddressMode::AbsoluteX),
            0xbd => self.lda(AddressMode::AbsoluteX),
            0xbe => self.ldx(AddressMode::AbsoluteY),
            0xc0 => self.cpy(AddressMode::Immediate),
            0x40 => self.rti(),
            0x41 => self.eor(AddressMode::IndirectX),
            0x4a => self.lsra(),
            0x4e => self.lsr(AddressMode::Absolute),
            0x4d => self.eor(AddressMode::Absolute),
            0x45 => self.eor(AddressMode::ZeroPage),
            0x46 => self.lsr(AddressMode::ZeroPage),
            0x48 => self.pha(),
            0x49 => self.eor(AddressMode::Immediate),
            0x28 => self.plp(),
            0x2a => self.rola(),
            0x2c => self.bit(AddressMode::ZeroPage),
            0x2e => self.rol(AddressMode::Absolute),
            0x20 => self.jsr(),
            0x21 => self.and(AddressMode::IndirectX),
            0x24 => self.bit(AddressMode::ZeroPage),
            0x29 => self.and(AddressMode::Immediate),
            0x2d => self.and(AddressMode::Absolute),
            0x30 => self.bmi(),
            0x35 => self.and(AddressMode::ZeroPageX),
            0x36 => self.rol(AddressMode::ZeroPageX),
            0x38 => self.sec(),
            0x3e => self.rol(AddressMode::AbsoluteX),
            0x4c => self.jmp(AddressMode::Absolute),
            0x50 => self.bvs(),
            0x51 => self.eor(AddressMode::IndirectY),
            0x54 => self.ign(),
            0x55 => self.eor(AddressMode::ZeroPageX),
            0x5a => self.nop(),
            0x59 => self.eor(AddressMode::AbsoluteY),
            0x5d => self.eor(AddressMode::AbsoluteX),
            0x60 => self.rts(),
            0x61 => self.adc(AddressMode::IndirectX),
            0x64 => self.dop(AddressMode::ZeroPage),
            0x65 => self.adc(AddressMode::ZeroPage),
            0x66 => self.ror(AddressMode::ZeroPage),
            0x68 => self.pla(),
            0x6a => self.rora(),
            0x6c => self.jmp(AddressMode::Indirect),
            0x6d => self.adc(AddressMode::Absolute),
            0x69 => self.adc(AddressMode::Immediate),
            0x6e => self.ror(AddressMode::Absolute),
            0x70 => self.bvs(),
            0x72 => self.nop(),
            0x73 => self.nop(),
            0x78 => self.sei(),
            0x79 => self.adc(AddressMode::IndirectY),
            0x7e => self.ror(AddressMode::AbsoluteX),
            0x84 => self.sty(AddressMode::ZeroPage),
            0x85 => self.sta(AddressMode::ZeroPage),
            0x86 => self.stx(AddressMode::ZeroPage),
            0x88 => self.dey(),
            0x8c => self.sty(AddressMode::Absolute),
            0x8d => self.sta(AddressMode::Absolute),
            0x8e => self.stx(AddressMode::Absolute),
            0x90 => self.bcc(),
            0x91 => self.sta(AddressMode::IndirectY),
            0x95 => self.sta(AddressMode::ZeroPageX),
            0x96 => self.stx(AddressMode::ZeroPageY),
            0x98 => self.tya(),
            0x9a => self.txs(),
            0x9d => self.sta(AddressMode::AbsoluteX),
            0xc3 => self.dcp(),
            0xc4 => self.cpy(AddressMode::ZeroPage),
            0xc6 => self.dec(AddressMode::ZeroPage),
            0xc9 => self.cmp(AddressMode::Immediate),
            0xca => self.dex(),
            0xce => self.dec(AddressMode::Absolute),
            0xcc => self.cpy(AddressMode::Absolute),
            0xd0 => self.bne(),
            0xd2 => self.hlt(),
            0xd3 => self.dcp(),
            0xd6 => self.dec(AddressMode::ZeroPageX),
            0xd8 => self.cld(),
            0xd9 => self.cmp(AddressMode::AbsoluteY),
            0xde => self.dec(AddressMode::AbsoluteX),
            0xdf => self.dcp(),
            0xe1 => self.sbc(AddressMode::IndirectX),
            0xe5 => self.sbc(AddressMode::ZeroPage),
            0xe6 => self.inc(AddressMode::ZeroPage),
            0xe8 => self.inx(),
            0xe9 => self.sbc(AddressMode::Immediate),
            0xea => self.nop(),
            0xec => self.cpx(AddressMode::Absolute),
            0xed => self.sbc(AddressMode::Absolute),
            0xc8 => self.iny(),
            0xf0 => self.beq(),
            0xf1 => self.sbc(AddressMode::IndirectY),
            0xf5 => self.sbc(AddressMode::ZeroPageX),
            0xf6 => self.inc(AddressMode::ZeroPage),
            0xf7 => self.isc(AddressMode::ZeroPageX),
            0xf8 => self.sed(),
            0xfe => self.inc(AddressMode::AbsoluteX),
            0xfd => self.sbc(AddressMode::AbsoluteX),
            0xff => self.isc(AddressMode::AbsoluteX),
            0x1e => self.asl(AddressMode::AbsoluteX),
            _ => unimplemented!("Unknown opcode:{:04x}", opcode),
        }
        self.cpu.opcode = opcode as u8;
        self.cpu.reg.prev_pc = self.cpu.reg.pc;
    }

    fn adc(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;
        let value: u16 = match mode {
            AddressMode::ZeroPage => {
                self.adv_pc(2);
                self.adv_cycles(3);
                self.read_word(pc + 1) & 0xff
            }
            AddressMode::ZeroPageX => {
                self.adv_pc(2);
                self.adv_cycles(4);
                self.read(pc + 2) as u16 & 0xff + self.cpu.reg.x as u16
            }
            AddressMode::Immediate => {
                self.adv_pc(2);
                self.adv_cycles(2);
                self.read(pc) as u16
            }
            AddressMode::Absolute => {
                self.adv_pc(3);
                self.adv_cycles(4);
                self.read_word(pc + 1) as u16
            }
            AddressMode::AbsoluteX => {
                self.adv_pc(3);
                self.adv_cycles(4);
                let data = self.read_word(pc + 1) + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                data
            }
            AddressMode::AbsoluteY => {
                // TODO Check if this is a valid addressing mode
                self.adv_pc(3);
                let data = self.read_word(pc + 1) + self.cpu.reg.y as u16;
                if (data - self.cpu.reg.y as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                self.adv_cycles(4);
                data
            }
            AddressMode::IndirectX => {
                self.adv_pc(2);
                self.adv_cycles(6);
                self.read(pc) as u16
            }
            AddressMode::IndirectY => {
                self.adv_cycles(4);
                self.adv_pc(3);
                let data = self.read(pc + 1) + self.cpu.reg.y & 0xff;
                let addr = (self.read(data as u16) as u16 | self.read(data as u16 + 1) as u16) << 8;
                let value = self.read(addr as u16);
                if (data as u16 - self.cpu.reg.y as u16) & 0xff00 != data as u16 & 0xff00 { self.adv_cycles(1); }
                value as u16
            }
            _ => unimplemented!("ADC address mode not supported {:?}", mode),
        };

        let a = self.cpu.reg.a as u16;
        let (result, overflow) = a.overflowing_add(value.wrapping_add(self.cpu.flags.carry as u16));
        self.cpu.reg.a = result as u8;

        self.cpu.flags.carry = (self.cpu.reg.a & 0x01) != 0;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.cpu.flags.zero = (self.cpu.reg.a & 0xff) == 0;
        // Set to 1 if last ADC resulted in a signed overflow
        self.cpu.flags.overflow = overflow;
    }
    // ASL (Accumulator) helper function for ASL Accumulator
    fn asla(&mut self) {
        let data = self.cpu.reg.a << 1;
        self.cpu.flags.carry = data >> 1 & 0xfe != 0;
        self.cpu.flags.negative;
        self.cpu.reg.a = data as u8;
        self.adv_pc(1);
        self.adv_cycles(2);
    }

    // Arithmetic shift left
    fn asl(&mut self, mode: AddressMode) {
        // ASL shifts all bits left one position. 0 is shifted into bit 0
        // and the original bit 7 is shifted into the carry slot
        // Affected flags: S Z C

        let pc = self.cpu.reg.pc;
        let addr: u16 = match mode {
            AddressMode::ZeroPage => {
                self.adv_pc(2);
                self.adv_cycles(5);
                self.read_word(pc + 1) & 0xff
            },
            AddressMode::ZeroPageX => {
                self.adv_pc(2);
                self.adv_cycles(6);
                self.read(pc + 2) as u16 & 0xff + self.cpu.reg.x as u16
            },
            AddressMode::Absolute => {
                self.adv_pc(3);
                self.adv_cycles(6);
                self.read_word(pc + 1) as u16
            },
            AddressMode::AbsoluteX => {
                self.adv_pc(3);
                self.adv_cycles(7);
                let data = self.read_word(pc + 1) as u16 + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                data
            },
            _ => unimplemented!()
        };

        let value: u8 = self.read(addr as u16);
        self.write(addr as u16, value);
        // Shift data left by one and write it back to memory
        self.write(addr as u16, value << 1);
        self.cpu.flags.carry = value >> 1 & 0xfe != 0;
        self.cpu.flags.negative = (value & 0x80) != 0;
        self.cpu.flags.zero = (value & 0xff) == 0;
    }

    fn and(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;
        let data: u16 = match mode {
            AddressMode::Immediate => {
                self.adv_pc(2);
                self.adv_cycles(2);
                self.read(pc + 1) as u16
            },
            AddressMode::ZeroPage => {
                self.adv_pc(2);
                self.adv_cycles(3);
                self.read(pc + 1) as u16 & 0xff
            }
            AddressMode::ZeroPageX => {
                self.adv_cycles(4);
                self.adv_pc(2);
                self.read(pc + 2) as u16 & 0xff + self.cpu.reg.x as u16
            },
            AddressMode::Absolute => {
                self.adv_pc(3);
                self.adv_cycles(4);
                self.read_word(pc + 1)
            },
            AddressMode::AbsoluteX => {
                self.adv_pc(3);
                self.adv_cycles(4);
                let data = self.read_word(pc + 1) + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                data
            },
            AddressMode::AbsoluteY => {
                self.adv_pc(3);
                self.adv_cycles(4);
                let data = self.read_word(pc + 1) + self.cpu.reg.y as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                data
            },
            AddressMode::IndirectX => {
                self.adv_cycles(6);
                self.adv_pc(2);
                let data = self.read(pc + 1) + self.cpu.reg.x & 0xff;
                let addr = (self.read(data as u16) as u16 | self.read(data as u16 + 1) as u16) << 8;
                let value = self.read(addr as u16);
                value as u16
            }
            AddressMode::IndirectY => {
                self.adv_cycles(5);
                self.adv_pc(2);
                let data = self.read(pc + 1) + self.cpu.reg.y & 0xff;
                let addr = (self.read(data as u16) as u16 | self.read(data as u16 + 1) as u16) << 8;
                let value = self.read(addr as u16);
                if (data as u16 - self.cpu.reg.y as u16) & 0xff00 != data as u16 & 0xff00 { self.adv_cycles(1); }
                value as u16
            },
            _ => unimplemented!("Illegal {:?} for DEC instruction", mode)
        };

        let result = self.cpu.reg.a as u16 & data;
        self.cpu.reg.a = result as u8;
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
    }
    // Branch if Carry Set
    fn bcs(&mut self) {
        if self.cpu.flags.carry {
            let offset = self.read(self.cpu.reg.pc + 1) as i8 as u16;
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
            self.adv_cycles(1);
        } else {
            self.adv_pc(2);
        }
        self.adv_cycles(2);
    }
    fn bcc(&mut self) {
        if !self.cpu.flags.carry {
            let offset = self.read(self.cpu.reg.pc + 1) as i8 as u16;
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
            self.adv_cycles(1);
        } else {
            self.adv_pc(2);
        }
        self.adv_cycles(2);
    }
    // Branch on Equal
    fn beq(&mut self) {
        if self.cpu.flags.zero {
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            let offset = self.read(self.cpu.reg.pc + 1) as i8 as u16;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
            self.adv_cycles(1);
        } else {
            self.adv_pc(2);
        }
        self.adv_cycles(2);
    }
    // Branch if Minus
    fn bmi(&mut self) {
        // TODO Handle branch cycling
        // 2 cycles (+ 1 if branch succeeds, +2 if to a new page)
        // This is for all branch instructions
        if !self.cpu.flags.negative {
            let offset = self.read(self.cpu.reg.pc + 1) as i8 as u16;
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
            self.adv_cycles(1);
        } else {
            self.adv_pc(2);
        }
        self.adv_cycles(2);
    }
    // Branch on Plus (if positive)
    fn bpl(&mut self) {
        // Cycles 3+ / 2
        // TODO with test rom we end up at STA but should be at next instruction STX
        if !self.cpu.flags.negative {
            let offset = self.read(self.cpu.reg.pc + 1) as i8 as u16;
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
            self.adv_cycles(3);
        } else {
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
            AddressMode::Absolute => {
                // BIT $44 HEX: $24, LEN: 2, TIME: 3
                let value = self.read_word(self.cpu.reg.pc + 1);
                let a = self.cpu.reg.a as u16;
                self.cpu.flags.zero = (value & a) == 0;
                self.cpu.flags.negative = (value & 0x80) != 0;
                self.cpu.flags.overflow = (value & 0x40) != 0;
                self.adv_cycles(4);
                self.adv_pc(3);
            }
            AddressMode::ZeroPage => {
                // BIT $44 HEX: $24, LEN: 2, TIME: 3
                let value = self.read(self.cpu.reg.pc + 1) & 0xff;
                let a = self.cpu.reg.a;
                self.cpu.flags.zero = (value & a) == 0;
                self.cpu.flags.negative = (value & 0x80) != 0;
                self.cpu.flags.overflow = (value & 0x40) != 0;
                self.adv_cycles(3);
                self.adv_pc(2);
            }
            _ => unimplemented!("Mode not supported {:?}", mode)
        }
    }
    fn bne(&mut self) {
        // If zero flag is 0 branch
        if !self.cpu.flags.zero {
            let offset = self.read(self.cpu.reg.pc + 1) as i8 as u16;
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
            self.adv_cycles(3);
        } else {
            self.adv_cycles(2)
        }
        self.adv_pc(2);
    }
    // Branch on overflow set
    fn bvs(&mut self) {
        if self.cpu.flags.overflow {
            let offset = self.read(self.cpu.reg.pc + 1) as i8 as u16;
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
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
    // Compare with accumulator
    fn cmp(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;
        let a = self.cpu.reg.a as u16;

        let result: u16 = match mode {
            AddressMode::ZeroPage => {
                self.adv_pc(2);
                self.adv_cycles(2);
                a - self.read_word(pc + 1) & 0xff
            },
            AddressMode::ZeroPageX => {
                self.adv_pc(2);
                self.adv_cycles(4);
                a - self.read_word(pc + 1) & 0xff + self.cpu.reg.x as u16
            },
            AddressMode::Immediate => {
                self.adv_pc(2);
                self.adv_cycles(2);
                a - self.read(pc + 1) as u16
            },
            AddressMode::Absolute => {
                self.adv_pc(3);
                self.adv_cycles(4);
                a - self.read_word(pc + 1)
            },
            AddressMode::AbsoluteX => {
                self.adv_pc(3);
                self.adv_cycles(4);
                let value = self.read(pc + 1).wrapping_add(self.cpu.reg.x) as u16;
                if (value - self.cpu.reg.x as u16) & 0xff00 != value & 0xff00 { self.adv_cycles(1); }
                a - value as u16
            },
            AddressMode::AbsoluteY => {
                self.adv_cycles(4);
                self.adv_pc(3);
                let value = self.read(pc + 1).wrapping_add(self.cpu.reg.y) as u16;
                if (value - self.cpu.reg.y as u16) & 0xff00 != value & 0xff00 { self.adv_cycles(1); }
                a - value as u16
            },
            AddressMode::IndirectX => {
                self.adv_cycles(6);
                self.adv_pc(2);
                let data = self.read(pc + 2) + self.cpu.reg.x & 0xff;
                let addr = (self.read(data as u16) as u16 | self.read(data as u16 + 1) as u16) << 8;
                let value = self.read(addr as u16) & 0xff;
                a - value as u16
            },
            // TODO Better naming (this is confusing...)
            AddressMode::IndirectY => {
                self.adv_cycles(5);
                self.adv_pc(2);
                let data = self.read(pc + 2) + self.cpu.reg.y & 0xff;
                let addr = (self.read(data as u16) as u16 | self.read(data as u16 + 1) as u16) << 8;
                if (data as u16 - self.cpu.reg.y as u16) & 0xff00 != data as u16 & 0xff00 { self.adv_cycles(1); }
                let value = self.read(addr as u16) & 0xff;
                a - value as u16
            }
            _ => unimplemented!()
        };
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.carry = (result & 0x01) != 0;
    }
    fn cpx(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;
        let x = self.cpu.reg.x as u16;
        let result: u16 = match mode {
            AddressMode::ZeroPage => {
                self.adv_pc(2);
                self.adv_cycles(2);
                x - self.read_word(pc + 1) & 0xff
            },
            AddressMode::Immediate => {
                self.adv_pc(2);
                self.adv_cycles(3);
                x - self.read(pc + 1) as u16
            },
            AddressMode::Absolute => {
                self.adv_pc(3);
                self.adv_cycles(4);
                x - self.read_word(pc + 1)
            }
            _ => unimplemented!("CPX with address mode {:?} is not supported", mode)
        };
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.carry = (result & 0x01) != 0;
    }
    fn cpy(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;
        let y = self.cpu.reg.y as u16;
        let result: u16 = match mode {
            AddressMode::ZeroPage => {
                self.adv_pc(2);
                self.adv_cycles(2);
                y - self.read_word(pc + 1) & 0xff
            },
            AddressMode::Immediate => {
                self.adv_pc(2);
                self.adv_cycles(3);
                let value = self.read(pc + 1);
                y - value as u16
            },
            AddressMode::Absolute => {
                self.adv_pc(3);
                self.adv_cycles(4);
                y - self.read_word(pc + 1)
            }
            _ => unimplemented!("CPY with address mode {:?} is not supported", mode)
        };
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.carry = (result & 0x01) != 0;
    }

    fn dec(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;

        let result: u16 = match mode {
            AddressMode::ZeroPage => {
                self.adv_pc(2);
                self.adv_cycles(5);
                let data = self.read_word(pc + 1) & 0xff;
                data.wrapping_sub(1)
            },
            AddressMode::ZeroPageX => {
                self.adv_pc(2);
                self.adv_cycles(6);
                let data = self.read_word(pc + 1) & 0xff + self.cpu.reg.x as u16;
                data.wrapping_sub(1)
            },
            AddressMode::Absolute => {
                self.adv_cycles(6);
                self.adv_pc(3);
                let data = self.read_word(pc);
                data.wrapping_sub(1)
            },
            AddressMode::AbsoluteX => {
                self.adv_cycles(3);
                self.adv_pc(7);
                let data = self.read_word(self.cpu.reg.pc + 1) + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                data.wrapping_sub(1)
            },
            _ => unimplemented!("DEC {:?} is invalid", mode)
        };
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
    }
    // Decrement Y register
    fn dey(&mut self) {
        self.cpu.reg.y = self.cpu.reg.y.wrapping_sub(1);
        self.cpu.flags.negative = (self.cpu.reg.y & 0x80) != 0;
        self.cpu.flags.zero = self.cpu.reg.y & 0xff == 0;
        self.adv_cycles(2);
        self.adv_pc(1);
    }
    // Decrement X register
    fn dex(&mut self) {
        self.cpu.reg.x = self.cpu.reg.x.wrapping_sub(1);
        self.cpu.flags.negative = (self.cpu.reg.x & 0x80) != 0;
        self.cpu.flags.zero = self.cpu.reg.x & 0xff == 0;
        self.adv_cycles(2);
        self.adv_pc(1);
    }
    // Decrement & compare
    fn dcp(&mut self) {
        println!("DCP (illegal opcode)");
        let data = self.read(self.cpu.reg.pc + 2);
        self.adv_pc(2);
        self.adv_cycles(7);
        unimplemented!();
    }
    // Double NOP
    fn dop(&mut self, mode: AddressMode) {
        match mode {
            AddressMode::ZeroPage => {
                self.adv_cycles(3);
                self.adv_pc(2)
            },
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
    // Exclusive OR (XOR)
    fn eor(&mut self, mode: AddressMode) {
        // Exclusive OR is performed on the accumulator's contents with the contents of a byte
        // of memory
        let pc = self.cpu.reg.pc;
        let value = match mode {
            AddressMode::ZeroPage => {
                self.adv_cycles(3);
                self.adv_pc(2);
                let data = self.read(pc + 1) & 0xff;
                data as u16
            },
            AddressMode::ZeroPageX => {
                self.adv_pc(2);
                self.adv_cycles(4);
                let data = self.read(pc + 1) & 0xff + self.cpu.reg.x;
                data as u16
            },
            AddressMode::Immediate => {
                self.adv_cycles(2);
                self.adv_pc(2);
                let data = self.read(pc + 1) as u16;
                data as u16
            },
            AddressMode::Absolute => {
                self.adv_cycles(4);
                self.adv_pc(2);
                self.read_word(pc + 1)
            },
            AddressMode::AbsoluteX => {
                self.adv_cycles(4);
                self.adv_pc(3);
                let data = self.read_word(pc + 1) + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                data
            }
            AddressMode::AbsoluteY => {
                self.adv_cycles(4);
                self.adv_pc(3);
                let data = self.read_word(pc + 1) + self.cpu.reg.y as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                data
            }
            AddressMode::IndirectX => {
                self.adv_cycles(6);
                self.adv_pc(2);
                let data = (self.read(pc + 2) as u16) + (self.cpu.reg.x & 0xff) as u16;
                let addr = (self.read(data) as u16 | self.read(data + 1) as u16) << 8;
                let value = self.read(addr as u16) & 0xff;
                value as u16
            }
            AddressMode::IndirectY => {
                self.adv_cycles(5);
                self.adv_pc(2);
                let data = (self.read(pc + 2) as u16) + (self.cpu.reg.y & 0xff) as u16;
                let addr = (self.read(data) as u16 | self.read(data + 1) as u16) << 8;
                let value = self.read(addr as u16) & 0xff;
                if (data - self.cpu.reg.y as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                value as u16
            }
            _ => unimplemented!("{:?} not supported", mode)
        };
        self.cpu.reg.a = (value as u8 ^ self.cpu.reg.a) as u8;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.cpu.flags.zero = (self.cpu.reg.a & 0xff) == 0;
    }
    fn hlt(&self) {
        // Halt CPU
        eprintln!("HLT! Opcode:{:04x}", self.cpu.opcode);
        ::std::process::exit(0);
    }
    fn lax(&mut self, mode: AddressMode) {
        // Load both the accumulator and the X register with contents of a memory location
        // Part of the undocumented 6502 opcodes
        // (Sub-instructions: LDA, LDX)
        let pc = self.cpu.reg.pc;
        let result = match mode {
            AddressMode::ZeroPage => {
                self.adv_pc(2);
                self.adv_cycles(3);
                let data = self.read_word(pc + 1) & 0xff;
                data as u16
            },
            AddressMode::ZeroPageX => {
                self.adv_pc(2);
                self.adv_cycles(4);
                let data = self.read_word(pc + 1) & 0xff + self.cpu.reg.x as u16;
                data as u16
            },
            AddressMode::Immediate => {
                self.adv_pc(2);
                self.adv_cycles(2);
                let data = self.read(pc + 1);
                data as u16
            },
            AddressMode::Absolute => {
                self.adv_pc(3);
                self.adv_cycles(4);
                self.read_word(pc + 1)
            },
            AddressMode::AbsoluteX => {
                self.adv_pc(3);
                self.adv_cycles(4);
                let data = self.read_word(pc + 1) + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                data
            },
            AddressMode::AbsoluteY => {
                self.adv_pc(3);
                self.adv_cycles(4);
                let data = self.read_word(pc + 1) + self.cpu.reg.y as u16;
                if (data - self.cpu.reg.y as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                data
            },
            AddressMode::IndirectX => {
                let data = (self.read(pc + 2) as u16) + (self.cpu.reg.x & 0xff) as u16;
                let addr = (self.read(data) as u16 | self.read(data + 1) as u16) << 8;
                let value = self.read(addr as u16) & 0xff;
                self.adv_cycles(6);
                self.adv_pc(2);
                value as u16
            },
            AddressMode::IndirectY => {
                let data = (self.read(pc + 2) as u16) + (self.cpu.reg.y & 0xff) as u16;
                let addr = (self.read(data) as u16 | self.read(data + 1) as u16) << 8;
                let value = self.read(addr as u16) & 0xff;
                if (data - self.cpu.reg.y as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                self.adv_cycles(6);
                self.adv_pc(2);
                value as u16
            }
            _ => unimplemented!("{:?} not supported", mode)
        };
        self.cpu.reg.a = result as u8;
        self.cpu.reg.x = result as u8;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.negative = (result & 0x80) != 0;
    }
    fn ldy(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;
        let result: u16 = match mode {
            AddressMode::Absolute => {
                self.adv_pc(3);
                self.adv_cycles(4);
                let data = self.read_word(pc + 1);
                self.cpu.reg.y = data as u8;
                data
            },
            AddressMode::AbsoluteX => {
                let data = self.read_word(pc + 1) + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                self.cpu.reg.y = data as u8;
                self.adv_pc(3);
                self.adv_cycles(4);
                data
            },
            AddressMode::Immediate => {
                let data = self.read(pc + 1);
                self.cpu.reg.y = data as u8;
                self.adv_pc(2);
                self.adv_cycles(2);
                data as u16
            }
            AddressMode::ZeroPage => {
                let data = self.read_word(pc + 1) & 0xff;
                self.cpu.reg.y = data as u8;
                self.adv_pc(2);
                self.adv_cycles(3);
                data
            }
            AddressMode::ZeroPageX => {
                let data = (self.read_word(pc + 1) & 0xff) + self.cpu.reg.x as u16;
                self.cpu.reg.y = data as u8;
                self.adv_pc(2);
                self.adv_cycles(4);
                data
            }
            _ => unimplemented!("LDY {:?} is illegal", mode)
        };
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.negative = (result & 0x80) != 0;
    }
    fn ldx(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;
        let result: u16 = match mode {
            AddressMode::Absolute => {
                self.adv_pc(3);
                self.adv_cycles(4);
                let data = self.read_word(pc + 1);
                self.cpu.reg.x = data as u8;
                data
            },
            AddressMode::AbsoluteX => {
                let data = self.read_word(pc + 1) + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                self.cpu.reg.x = data as u8;
                self.adv_pc(3);
                self.adv_cycles(4);
                data
            },
            AddressMode::Immediate => {
                let data = self.read(pc + 1);
                self.cpu.reg.x = data as u8;
                self.adv_pc(2);
                self.adv_cycles(2);
                data as u16
            }
            AddressMode::ZeroPage => {
                let data = self.read_word(pc + 1) & 0xff;
                self.cpu.reg.x = data as u8;
                self.adv_pc(2);
                self.adv_cycles(3);
                data
            }
            AddressMode::ZeroPageX => {
                let data = (self.read_word(pc + 1) & 0xff) + self.cpu.reg.x as u16;
                self.cpu.reg.x = data as u8;
                self.adv_pc(2);
                self.adv_cycles(4);
                data
            }
            _ => unimplemented!("LDX {:?} is illegal", mode)
        };
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.negative = (result & 0x80) != 0;
    }
    fn lda(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;
        let result: u16 = match mode {
            AddressMode::Absolute => {
                // LDA A16
                let data = self.read_word(pc + 1);
                self.cpu.reg.a = data as u8;
                self.adv_cycles(4);
                self.adv_pc(3);
                data
            },
            AddressMode::AbsoluteX => {
                let data = self.read_word(pc + 1) + self.cpu.reg.x as u16;


                // Check if the difference between read value - reg & mask off high 8 bits.
                // If not equal increment the cycle counter, as we've crossed the boundary
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                self.cpu.reg.a = data as u8;
                self.adv_cycles(4);
                self.adv_pc(3);
                data
            },
            AddressMode::AbsoluteY => {
                let data = self.read_word(pc + 1) + self.cpu.reg.y as u16;

                if (data - self.cpu.reg.y as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                self.cpu.reg.a = data as u8;
                self.adv_cycles(4);
                self.adv_pc(3);
                data
            }
            AddressMode::IndirectX => {
                let data = (self.read(pc + 1) as u16) + (self.cpu.reg.x & 0xff) as u16;
                let addr = (self.read(data) as u16 | self.read(data + 1) as u16) << 8;
                let value = self.read(addr as u16) & 0xff;
                self.adv_cycles(6);
                self.adv_pc(2);
                value as u16
            }
            AddressMode::IndirectY => {
                // Indirect is 3 bytes
                let data = (self.read(pc + 1) as u16) + (self.cpu.reg.y & 0xff) as u16;
                let addr = (self.read(data) as u16 | self.read(data + 1) as u16) << 8;
                if (data - self.cpu.reg.y as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                let value = self.read(addr as u16) & 0xff;
                self.adv_cycles(6);
                self.adv_pc(2);
                value as u16
            }
            AddressMode::Immediate => {
                self.adv_pc(2);
                self.adv_cycles(2);
                let data = self.read(pc + 1);
                data as u16
            }
            AddressMode::ZeroPage => {
                let data = self.read_word(pc + 1) & 0xff;
                self.adv_pc(2);
                self.adv_cycles(3);
                data
            }
            AddressMode::ZeroPageX => {
                let data = (self.read_word(pc + 1) & 0xff) + self.cpu.reg.x as u16;
                self.cpu.reg.a = data as u8;
                self.adv_pc(2);
                self.adv_cycles(4);
                data
            }
            AddressMode::ZeroPageY => {
                let data = (self.read_word(pc + 1) & 0xff) + self.cpu.reg.y as u16;
                self.cpu.reg.a = data as u8;
                self.adv_pc(2);
                self.adv_cycles(3);
                data
            }
            _ => unimplemented!("{:?} is not used for LDA", mode)
        };
        self.cpu.reg.a = result as u8;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.negative = (result & 0x80) != 0;
    }
    fn lsr(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;
        let addr = match mode {
            AddressMode::ZeroPage => {
                let data = self.read(pc + 1) & 0xff;
                self.adv_cycles(5);
                self.adv_pc(2);
                data as u16
            }
            AddressMode::ZeroPageX => {
                let data = self.read(pc + 1) & 0xff + self.cpu.reg.x;
                self.adv_cycles(6);
                self.adv_pc(2);
                data as u16
            }
            AddressMode::Absolute => {
                let data = self.read_word(pc + 1);
                self.adv_cycles(6);
                self.adv_pc(3);
                data
            }
            AddressMode::AbsoluteX => {
                let data = self.read_word(pc + 1) + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                self.adv_cycles(6);
                self.adv_pc(3);
                data
            }
            _ => unimplemented!("{:?} not implemented", mode)
        };
        let value: u8 = self.read(addr as u16);
        // self.write(addr as u16, value);
        let result = (value >> 1) | ((self.cpu.flags.carry as u8) << 7);
        self.write(addr as u16, result);
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.carry = value & 1 != 0;
    }

    // Logical Shift Right (Accumulator)
    fn lsra(&mut self) {
        // Flags affected
        let carry = (self.cpu.reg.a & 1) != 0;
        self.cpu.reg.a = (self.cpu.reg.a >> 1) | ((self.cpu.flags.carry as u8) << 7);
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.cpu.flags.zero = (self.cpu.reg.a & 0xff) == 0;
        self.cpu.flags.carry = carry;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    fn nop(&mut self) {
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    fn ora(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;
        let value = match mode {
            AddressMode::ZeroPage => {
                self.adv_cycles(3);
                self.adv_pc(2);
                let result = (self.read(pc + 1) & 0xff) as u16;
                result as u16
            },
            AddressMode::ZeroPageX => {
                self.adv_pc(2);
                self.adv_cycles(3);
                let result = (self.read(pc + 1) & 0xff + self.cpu.reg.x) as u16;
                result as u16
            },
            AddressMode::Immediate => {
                self.adv_cycles(2);
                self.adv_pc(2);
                let result = self.read(pc + 1) as u16;
                result as u16
            },
            AddressMode::Absolute => {
                self.adv_cycles(4);
                self.adv_pc(3);
                self.read_word(pc + 1)
            },
            AddressMode::AbsoluteX => {
                self.adv_cycles(4);
                self.adv_pc(3);
                let data = self.read_word(pc) + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                data
            }
            AddressMode::AbsoluteY => {
                self.adv_cycles(4);
                self.adv_pc(3);
                let data = self.read_word(pc) + self.cpu.reg.y as u16;
                if (data - self.cpu.reg.y as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                data
            }
            AddressMode::IndirectX => {
                let value = self.cpu.reg.pc + 1;
                let result = self.read(value) << 1 + self.cpu.reg.x;
                self.adv_pc(2);
                self.adv_cycles(8);
                result as u16
            }
            AddressMode::IndirectY => {
                let data = self.cpu.reg.pc + 1;
                let result = self.read(data) << 1 + self.cpu.reg.y;
                if (data - self.cpu.reg.y as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                self.adv_pc(2);
                self.adv_cycles(8);
                result as u16
            }
            _ => unimplemented!("{:?} not supported", mode)
        };
        self.cpu.reg.a = (value as u8 | self.cpu.reg.a) as u8;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.cpu.flags.zero = (self.cpu.reg.a & 0xff) == 0;
    }

    // ROL (accumulator)
    fn rola(&mut self) {
        let result = self.cpu.reg.a << 1 & 0xfe;
        self.cpu.reg.a = result as u8;
        // Set flag values
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.carry = (result & 0x01) != 0;
        self.adv_cycles(2);
        self.adv_pc(1);
    }
    // Rotate one bit right memory or accumulator
    fn rol(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;

        let src: u16 = match mode {
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
                let data = self.read_word(pc) + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                data
            }
            _ => { unreachable!("Address mode {:?} should not be called on ROL", mode) }
        };

        // Original bit is shifted into carry slot & carry is shifted into bit 7.
        let result = src << 1 & 0xfe;
        // The program counter is already modified by this point by the above `match`.
        let addr = self.cpu.reg.pc;
        self.write(addr, result as u8);

        // Set flag values
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.carry = (result & 0x01) != 0;

        // self.cpu.flags.interrupt = result & 0x04;
        // self.cpu.flags.decimal = result & 0x08;
        // self.cpu.flags.overflow = result & 0x40;
    }
    fn rora(&mut self) {
        let result = self.cpu.reg.a >> 1 & 0xfe;
        self.cpu.reg.a = result as u8;
        // Set flag values
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.carry = (result & 0x01) != 0;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    fn ror(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;

        let src: u16 = match mode {
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
                let data = self.read_word(pc) + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                data
            }
            _ => { unreachable!("Address mode {:?} should not be called on ROR", mode) }
        };

        // TODO check if result is correct
        // Assuming the slot is the same but the bit shift direction changes due to it being a
        // Rotate Right instruction
        // Original bit is shifted into carry slot & carry is shifted into bit 7.
        let result = src >> 1 & 0xfe;
        // The program counter is already modified by this point by the above `match`.
        let addr = self.cpu.reg.pc;
        // Write result to memory address
        self.write(addr, result as u8);

        // Set flag values
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.carry = (result & 0x01) != 0;

        // self.cpu.flags.interrupt = result & 0x04;
        // self.cpu.flags.decimal = result & 0x08;
        // self.cpu.flags.overflow = result & 0x40;
    }
    fn rts(&mut self) {
        self.pop_byte();
        let value = self.cpu.reg.sp as u16;

        // Set program counter for debug output
        self.cpu.reg.prev_pc = self.cpu.reg.pc;
        self.cpu.reg.pc = ((value << 8) - 1) as u16;
        // self.cpu.reg.sp = sp.wrapping_add(2) as u8;
        self.adv_pc(1);
        self.adv_cycles(6);
    }
    // Return from interrupt
    fn rti(&mut self) {
        // Pull processor flags from stack
        self.pop_byte();
        let sp = self.cpu.reg.sp;

        // TODO Check bitmask
        self.cpu.flags.negative = sp & 0x80 != 0;
        self.cpu.flags.zero = sp & 0x40 != 0;
        self.cpu.flags.carry = sp & 0x10 != 0;
        self.cpu.flags.interrupt = sp & 0x04 != 0;
        self.cpu.flags.decimal = sp & 0x01 != 0;

        self.adv_pc(1);
        self.adv_cycles(6);
    }
    fn sed(&mut self) {
        self.cpu.flags.decimal = true;
        self.adv_pc(1);
        self.adv_cycles(2);
    }

    fn sbc(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;
        let value = match mode {
            AddressMode::ZeroPage => {
                self.adv_cycles(3);
                self.adv_pc(2);
                let data = self.read(pc + 1) & 0xff;
                data as u16
            },
            AddressMode::ZeroPageX => {
                self.adv_cycles(3);
                self.adv_pc(2);
                let data = self.read(pc + 1) & 0xff + self.cpu.reg.x;
                data as u16
            },
            AddressMode::Immediate => {
                self.adv_pc(2);
                self.adv_cycles(2);
                let data = self.read(pc + 1);
                data as u16
            },
            AddressMode::Absolute => {
                self.adv_pc(3);
                self.adv_cycles(4);
                self.read_word(pc + 1)
            },
            AddressMode::AbsoluteX => {
                let data = self.read_word(pc + 1) + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.x as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                self.adv_cycles(4);
                self.adv_pc(3);
                data
            },
            AddressMode::AbsoluteY => {
                let data = self.read_word(pc + 1) + self.cpu.reg.x as u16;
                if (data - self.cpu.reg.y as u16) & 0xff00 != data & 0xff00 { self.adv_cycles(1); }
                self.adv_cycles(4);
                self.adv_pc(3);
                data
            },
            AddressMode::IndirectX => {
                let value = pc + 1;
                let data = self.read(value) << 1 + self.cpu.reg.x;
                self.adv_pc(2);
                self.adv_cycles(6);
                data as u16
            }
            AddressMode::IndirectY => {
                let addr = pc + 1;
                let data = self.read(addr) << 1 + self.cpu.reg.y;
                if (data as u16 - self.cpu.reg.y as u16) & 0xff00 != data as u16 & 0xff00 { self.adv_cycles(1); }
                self.adv_pc(2);
                self.adv_cycles(6);
                data as u16
            }
            _ => unimplemented!()
        };
        // XOR memory value with 255 to set if result is 0 to 255, or clear if less than 0.
        let a = self.cpu.reg.a as u16;
        let (result, overflow) = a.overflowing_add(value ^ 0xffu16.wrapping_add(self.cpu.flags.carry as u16));
        self.cpu.flags.overflow = overflow;

        self.cpu.reg.a = result as u8;

        self.cpu.flags.carry = (self.cpu.reg.a & 0x01) != 0;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.cpu.flags.zero = (self.cpu.reg.a & 0xff) == 0;
    }
    // SLO Accumulator
    fn sloa(&mut self) {
        let value = self.read(self.cpu.reg.pc + 1) << 1;
        self.cpu.reg.a |= value;
        self.cpu.flags.carry = value & 0x80 != 0;
        self.cpu.flags.zero = value & 0xff == 0;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // Shift left one bit in memory, then OR the result with the accumulator
    // Part of undocumented opcodes
    fn slo(&mut self, mode: AddressMode) {
        match mode {
            AddressMode::ZeroPage => {
                let value = self.read_word(self.cpu.reg.pc + 1) << 1 & 0xff;
                self.cpu.reg.a |= value as u8;
                self.cpu.flags.carry = value & 0x80 != 0;
                self.cpu.flags.zero = value & 0xff == 0;
                self.adv_pc(2);
                self.adv_cycles(5);
            },
            AddressMode::ZeroPageX => {
                let value = self.read_word(self.cpu.reg.pc + 1) << 1 & 0xff + self.cpu.reg.x as u16;
                self.cpu.reg.a |= value as u8;
                self.cpu.flags.carry = value & 0x80 != 0;
                self.cpu.flags.zero = value & 0xff == 0;
                self.adv_pc(2);
                self.adv_cycles(6);
            },
            AddressMode::ZeroPageY => {
                let value = self.read_word(self.cpu.reg.pc + 1) << 1 & 0xff + self.cpu.reg.y as u16;
                self.cpu.reg.a |= value as u8;
                self.cpu.flags.carry = value & 0x80 != 0;
                self.cpu.flags.zero = value & 0xff == 0;
                self.adv_pc(2);
                self.adv_cycles(6);
            },
            AddressMode::Absolute => {
                let value = self.read_word(self.cpu.reg.pc + 1) << 1 & 0xff;
                self.cpu.reg.a |= value as u8;
                self.cpu.flags.carry = value & 0x80 != 0;
                self.cpu.flags.zero = value & 0xff == 0;
                self.adv_pc(3);
                self.adv_cycles(6);
            },
            AddressMode::AbsoluteX => {
                let value = self.read_word(self.cpu.reg.pc + 1) << 1 + self.cpu.reg.x as u16;
                if (value - self.cpu.reg.y as u16) & 0xff00 != value & 0xff00 { self.adv_cycles(1); }
                self.cpu.reg.a |= value as u8;
                self.cpu.flags.carry = value & 0x80 != 0;
                self.cpu.flags.zero = value & 0xff == 0;
                self.adv_pc(3);
                self.adv_cycles(6);
            },
            AddressMode::AbsoluteY => {
                let value = self.read_word(self.cpu.reg.pc + 1) << 1 + self.cpu.reg.y as u16;
                if (value - self.cpu.reg.y as u16) & 0xff00 != value & 0xff00 { self.adv_cycles(1); }
                self.cpu.reg.a |= value as u8;
                self.cpu.flags.carry = value & 0x80 != 0;
                self.cpu.flags.zero = value & 0xff == 0;
                self.adv_pc(3);
                self.adv_cycles(6);
            },
            AddressMode::IndirectX => {
                let value = self.cpu.reg.pc + 1;
                let result: u16 = (self.read(value) as u16) << 1 + self.cpu.reg.x as u16;
                self.cpu.flags.carry = value & 0x80 != 0;
                self.cpu.flags.zero = value & 0xff == 0;
                self.adv_pc(2);
                self.adv_cycles(8);
            },
            AddressMode::IndirectY => {
                let value = self.cpu.reg.pc + 1;
                let result = (self.read(value) as u16) << 1 + self.cpu.reg.y as u16;
                if (value - self.cpu.reg.y as u16) & 0xff00 != value & 0xff00 { self.adv_cycles(1); }
                self.cpu.reg.a |= result as u8;
                self.cpu.flags.carry = value & 0x80 != 0;
                self.cpu.flags.zero = value & 0xff == 0;
                self.adv_pc(2);
                self.adv_cycles(8);
            }
            _ => unimplemented!()
        };
    }
    // TODO Fix indirect write?
    fn sta(&mut self, mode: AddressMode) {
        // Ex: Absolute: STA $4400 Hex: $8D Len: 3 Cycles:4
        let pc = self.cpu.reg.pc;
        let a = self.cpu.reg.a;

        match mode {
            AddressMode::Absolute => {
               let addr = self.read_word(pc + 1);
                // Write value of accumulator to memory address
                self.write(addr, a);
                self.adv_cycles(4);
                self.adv_pc(3);
            },
            AddressMode::AbsoluteX => {
                let addr = self.read_word(pc + 1) + self.cpu.reg.x as u16;
                if (addr - self.cpu.reg.x as u16) & 0xff00 != addr & 0xff00 { self.adv_cycles(1); }
                self.write(addr, a);
                self.adv_cycles(5);
                self.adv_pc(3);
            },
            AddressMode::IndirectX => {
                let addr = self.read_word(pc + 1) + self.cpu.reg.x as u16;
                self.write(addr, a);
                self.adv_cycles(6);
                self.adv_pc(2);
            },
            AddressMode::IndirectY => {
                let addr = self.read_word(pc + 1) + self.cpu.reg.y as u16;
                if (addr - self.cpu.reg.y as u16) & 0xff00 != addr & 0xff00 { self.adv_cycles(1); }
                self.write(addr, a);
                self.adv_cycles(6);
                self.adv_pc(2);
            },
            AddressMode::ZeroPage => {
                let addr = self.read_word(pc + 1) & 0xff;
                // Write value of accumulator to memory address
                self.write(addr, self.cpu.reg.a);
                self.adv_cycles(3);
                self.adv_pc(2);
            }
            AddressMode::ZeroPageX => {
                let addr = self.read_word(pc + 1) & 0xff + self.cpu.reg.x as u16;
                // Write value of accumulator to memory address
                self.write(addr, self.cpu.reg.a);
                self.adv_cycles(4);
                self.adv_pc(2);
            }
            _ => eprintln!("{:?} not covered", mode),
        }
    }
    fn sty(&mut self, mode: AddressMode) {
        // Ex: Absolute: STA $4400 Hex: $8D Len: 3 Cycles:4
        let pc = self.cpu.reg.pc;
        let y = self.cpu.reg.y;
        match mode {
            AddressMode::Absolute => {
                let addr = self.read_word(pc + 1);
                self.write(addr, y);
                self.adv_pc(3);
                self.adv_cycles(3);
            },
            AddressMode::ZeroPage => {
                let addr = self.read_word(pc + 1) & 0xff;
                self.write(addr, y);
                self.adv_pc(2);
                self.adv_cycles(3);
            }
            _ => eprintln!("{:?} not covered", mode),
        }
    }
    fn stx(&mut self, mode: AddressMode) {
        // Ex: Absolute: STA $4400 Hex: $8D Len: 3 Cycles:4
        let pc = self.cpu.reg.pc;
        let x = self.cpu.reg.x;
        match mode {
            AddressMode::Absolute => {
                let addr = self.read_word(pc + 1);
                // Write value of accumulator to memory address
                self.write(addr, x);
                self.adv_cycles(4);
                self.adv_pc(3);
            }
            AddressMode::ZeroPage => {
                let addr = self.read_word(pc + 1) & 0xff;
                // Write value of accumulator to memory address
                self.write(addr, x);
                self.adv_cycles(3);
                self.adv_pc(2);
            }
            AddressMode::ZeroPageX => {
                let data = self.read_word(pc + 1) & 0xff + self.cpu.reg.x as u16;
                self.write(data, x);
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_cycles(2);
                self.adv_pc(2);
            }
            AddressMode::ZeroPageY => {
                let data = self.read_word(pc + 1) & 0xff + self.cpu.reg.y as u16;
                self.write(data, self.cpu.reg.y);
                self.cpu.flags.zero = (data & 0xff) == 0;
                self.cpu.flags.negative = (data & 0x80) != 0;
                self.adv_cycles(2);
                self.adv_pc(2);
            }
            _ => eprintln!("{:?} not covered", mode),
        }
    }
    // Transfer Accumulator to X
    fn tax(&mut self) {
        self.cpu.reg.x = self.cpu.reg.a;
        self.cpu.flags.zero = (self.cpu.reg.x & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.x & 0x80) != 0;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // Transfer Y to Accumulator
    fn tay(&mut self) {
        self.cpu.reg.y = self.cpu.reg.a;
        self.cpu.flags.zero = (self.cpu.reg.y & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.y & 0x80) != 0;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // Transfer Y to Accumulator
    fn tya(&mut self) {
        self.cpu.reg.a = self.cpu.reg.y;
        self.cpu.flags.zero = (self.cpu.reg.a & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // Transfer X to Accumulator
    fn txa(&mut self) {
        self.cpu.reg.a = self.cpu.reg.x;
        self.cpu.flags.zero = (self.cpu.reg.a & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // Transfer X to Stack Pointer
    fn txs(&mut self) {
        let src = self.cpu.reg.x;
        self.cpu.flags.negative = (src & 0x80) != 0;
        self.cpu.flags.zero = (src & 0xff) != 0;
        self.cpu.reg.sp = self.cpu.reg.x;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // Transfer Stack Pointer to X
    fn tsx(&mut self) {
        let src = self.cpu.reg.sp;
        self.cpu.flags.negative = (src & 0x80) != 0;
        self.cpu.flags.zero = (src & 0xff) != 0;
        self.cpu.reg.x = self.cpu.reg.sp;
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
        self.cpu.reg.sp = self.cpu.reg.sp.wrapping_sub(1);
        self.write(0x100 | sp as u16, byte);
    }
    // Pull
    fn pop_byte(&mut self) -> u8 {
        let sp = self.cpu.reg.sp;
        self.cpu.reg.sp = sp.wrapping_add(1);
        self.read(0x100 | sp as u16)
    }
    // Push accumulator
    fn pha(&mut self) {
        self.push_byte(self.cpu.reg.a);
        self.adv_pc(1);
        self.adv_cycles(3);
    }
    fn get_status_flags(&mut self) -> u8 {
        let ps = if self.cpu.flags.negative { 0x80 } else { 0x0 } |
            if self.cpu.flags.overflow { 0x40 } else { 0x0 } |
            if self.cpu.flags.reserved { 0x20 } else { 0x0 } |
            if self.cpu.flags.brk { 0x10 } else { 0x0 } |
            if self.cpu.flags.decimal { 0x08 } else { 0x0 } |
            if self.cpu.flags.interrupt { 0x04 } else { 0x0 } |
            if self.cpu.flags.zero { 0x02 } else { 0x0 } |
            if self.cpu.flags.carry { 0x01 } else { 0x0 };
        ps
    }
    fn set_status_flags(&mut self, value: u8) {
        self.cpu.flags.negative = value & 0x80 == 0x80;
        self.cpu.flags.overflow = value & 0x40 == 0x40;
        self.cpu.flags.reserved = value & 0x20 == 0x20;
        self.cpu.flags.brk = value & 0x10 == 0x10;
        self.cpu.flags.decimal = value & 0x08 == 0x08;
        self.cpu.flags.interrupt = value & 0x04 == 0x04;
        self.cpu.flags.zero = value & 0x02 == 0x02;
        self.cpu.flags.carry = value & 0x01 == 0x01;
    }
    // Push Processor Status
    fn php(&mut self) {
        // Pushes a copy of the status flags to the stack
        let ps = self.get_status_flags();
        self.push_byte(ps);
        self.adv_pc(1);
        self.adv_cycles(3);
    }

    // PuL1 (POP) Accumulator
    fn pla(&mut self) {
        // Pulls an 8-bit value from the stack and into the accumulator.
        // Flags affected zero & negative
        let value = self.pop_byte();
        self.cpu.flags.zero = (value & 0xff) == 0;
        self.cpu.flags.negative = (value & 0x80) != 0;
        self.cpu.reg.a = value;
        self.adv_pc(1);
        self.adv_cycles(4);
    }
    fn plp(&mut self) {
        let status = self.pop_byte();
        self.set_status_flags(status);
        self.adv_pc(1);
        self.adv_cycles(4);
    }
    // Increment Memory
    fn inc(&mut self, mode: AddressMode) {
        let pc = self.cpu.reg.pc;

        let result = match mode {
            AddressMode::ZeroPage => {
                let value = self.read(pc + 1) as u16 & 0xff;
                let addr = self.read_word(value) as u8;
                self.write(value, addr);
                self.adv_cycles(5);
                self.adv_pc(2);
                value as u16
            },
            AddressMode::ZeroPageX => {
                let value = self.read(pc + 1) as u16 & 0xff + self.cpu.reg.x as u16;
                let addr = self.read_word(value) as u8;
                self.write(value, addr);
                self.adv_cycles(5);
                self.adv_pc(2);
                value as u16
            },
            AddressMode::Absolute => {
                let value = self.read_word(pc + 1);
                let addr = self.read_word(value) as u8;
                self.write(value, addr);
                self.adv_pc(3);
                self.adv_cycles(6);
                value as u16
            },
            AddressMode::AbsoluteX => {
                let value = self.read_word(pc + 1) + self.cpu.reg.x as u16;
                if (value - self.cpu.reg.x as u16) & 0xff00 != value & 0xff00 { self.adv_cycles(1); }
                let addr = self.read_word(value) as u8;
                self.write(value, addr);
                self.adv_pc(3);
                self.adv_cycles(7);
                value as u16
            }
            _ => unimplemented!("{:?} not supported", mode)
        };
        self.cpu.flags.carry = result & 0x80 != 0;
        self.cpu.flags.zero = result & 0xff == 0;
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
    fn ign(&mut self) {
        // IGN d,X ($14 dd, $34 dd, $54 dd, $74 dd, $D4 dd, $F4 dd; 4 cycles)
        // Reads from memory at the specified address and ignores the value. Affects no register nor flags.
        // The absolute version can be used to increment PPUADDR or reset the PPUSTATUS latch as an alternative to BIT.
        // The zero page version has no side effects.
        // IGN d,X reads from both d and (d+X)&255. IGN a,X additionally reads from a+X-256 it crosses a page boundary (i.e. if ((a & 255) + X) > 255)
        // Sometimes called TOP (triple-byte no-op), SKW (skip word), DOP (double-byte no-op), or SKB (skip byte).
        let addr = self.cpu.reg.pc + 1;
        self.read(addr);
        self.adv_pc(1);
        self.adv_cycles(2);
    }

    // Jump to Subroutine
    fn jsr(&mut self) {
        self.cpu.reg.prev_pc = self.cpu.reg.pc;
        let addr = self.read_word(self.cpu.reg.pc + 1);
        self.adv_pc(3);

        // Push to stack
        let pc = self.cpu.reg.pc;
        self.cpu.reg.prev_pc = self.cpu.reg.pc;
        self.push_word(pc - 1);
        self.adv_cycles(3); // 6 if no jump?
        self.cpu.reg.pc = addr;
    }
    fn jmp(&mut self, mode: AddressMode) {
        match mode {
            AddressMode::Indirect => self.adv_cycles(5),
            AddressMode::Absolute => self.adv_cycles(3),
            _ => unimplemented!()
        }
        self.cpu.reg.prev_pc = self.cpu.reg.pc;
        self.cpu.reg.pc = self.read_word(self.cpu.reg.pc + 1);
    }
    fn sec(&mut self) {
        self.cpu.flags.carry = true;
        self.adv_cycles(2);
        self.adv_pc(1);
    }
    fn sei(&mut self) {
        self.cpu.flags.interrupt = true;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // ISC (Increase memory by one)
    fn isc(&mut self, mode: AddressMode) {
        let result = match mode {
            AddressMode::AbsoluteX => {
                self.adv_pc(3);
                self.adv_cycles(7);

                let value = self.cpu.reg.pc.wrapping_add(1);
                let addr = self.read_word(value).wrapping_add(self.cpu.reg.x as u16);
                if (value - self.cpu.reg.x as u16) & 0xff00 != value & 0xff00 { self.adv_cycles(1); }
                (self.cpu.reg.a as u16).wrapping_sub(addr as u16).wrapping_sub(self.cpu.flags.carry as u16)
            }
            AddressMode::ZeroPageX => {
                self.adv_pc(2);
                self.adv_cycles(6);

                let value = self.cpu.reg.pc.wrapping_add(1);
                let addr = self.read_word(value) & 0xff + self.cpu.reg.x as u16;
                (self.cpu.reg.a as u16).wrapping_sub(addr as u16).wrapping_sub(self.cpu.flags.carry as u16)
            }
            _ => unimplemented!("Mode not supported {:?}", mode)
        };

        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.reg.a = result as u8;
    }
    // Reset CPU to initial power up state
    pub fn reset(&mut self) {
        // TODO PPU reset
        // Read reset vector
        self.cpu.reg.pc = self.read_word(0xfffc);
        self.cpu.reg.sp = 0xfd;
        self.cpu.flags.carry = false;
        self.cpu.flags.zero = false;
        // Interrupt disable flag should be on on power up / reset
        self.cpu.flags.interrupt = true;
        self.cpu.flags.decimal = false;
        self.cpu.flags.brk = false;
        self.cpu.flags.overflow = false;
    }
}