use interconnect::{MemoryMapper};
use opcode::Instruction;
use memory::{Ram};
use rom::Cartridge;
use ppu::Ppu;
use apu::Apu;
use std::fmt;

impl MemoryMapper for ExecutionContext {
    fn read(&mut self, addr: u16) -> u8 {
        self.adv_cycles(1);

        // See https://wiki.nesdev.com/w/index.php/CPU_memory_map
        match addr {
            0...0x07ff => self.ram.memory[addr as usize] as u8,
            0x0800 ... 0x1fff => self.ram.memory[addr as usize & 0x07ff],
            0x2000 ... 0x3fff => self.ppu.read(addr),
            0x4000 ... 0x4017 => self.apu.read(addr),
            0x4018 ... 0x401f => unimplemented!("Read to CPU Test space"),
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
            _ => unimplemented!("Reads to ${:04x} is not implemented", addr),
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
        self.adv_cycles(1); // Advance cycle for each write
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
    pub cycles: u16,
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
    // TODO Remove or refactor
    fn bump(&mut self, bytes: u16, cycles: u16) {
        self.cpu.reg.prev_pc = self.cpu.reg.pc;
        self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(bytes);
        self.cpu.cycles = self.cpu.cycles.wrapping_add(cycles);
        // Page check
        // TODO add more address modes
        if self.absy().1 || self.absx().1 || self.indirect_indx().1{
            self.cpu.cycles = self.cpu.cycles.wrapping_add(1);
        }
    }
    fn adv_pc(&mut self, amount: u16) { self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(amount); }
    fn adv_cycles(&mut self, cycles: u16) { self.cpu.cycles = self.cpu.cycles.wrapping_add(cycles); }

    // Addressing modes
    fn imm(&mut self) -> u16 {
        self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(1);
        self.cpu.reg.pc - 1
    }
    fn imm16(&mut self) -> u16 {
        self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(2);
        self.cpu.reg.pc - 2
    }
    fn abs(&mut self) -> u16 {
        let imm16 = self.imm16();
        self.read16(imm16) as u16
    }
    // TODO Check if we should set flags here?
    pub fn zp(&mut self) -> u16 {
        let imm = self.imm();
        (self.read(imm) & 0xff) as u16
    }
    pub fn zpx(&mut self) -> u16 {
        let imm = self.imm();
        (self.read(imm & 0xff) as u16 + self.cpu.reg.x as u16)
    }
    pub fn zpy(&mut self) -> u16 {
        let imm = self.imm();
        (self.read16(imm as u16) & 0xff) + self.cpu.reg.y as u16
    }
    pub fn absx(&mut self) -> (u16, bool) {
        let imm16 = self.imm16();
        let value = self.read16(imm16) + self.cpu.reg.x as u16;
        if (value - self.cpu.reg.x as u16) & 0xff00 != value & 0xff00 {
            (value, true)
        } else {
            (value, false)
        }
    }
    pub fn absy(&mut self) -> (u16, bool) {
        let imm16 = self.imm16();
        let value = self.read16(imm16) + self.cpu.reg.y as u16;
        if (value - self.cpu.reg.y as u16) & 0xff00 != value & 0xff00 {
            (value, true)
        } else {
            (value, false)
        }
    }
    // Indirect X
    pub fn indx_indirect(&mut self) -> (u16, bool) {
        let imm = self.imm();
        let data = (self.read(imm).wrapping_add(self.cpu.reg.x & 0xff) as u8);
        // let addr = self.read(data as u16) | self.read((data + 1) as u16) << 8;
        let addr = self.read(data as u16) | self.read((data + 1) as u16).wrapping_shl(8);
        let value = (self.read(addr as u16) & 0xff) as u16;
        if (value.wrapping_sub(self.cpu.reg.x as u16) & 0xff00) != value & 0xff00 {
            (value, true)
        } else {
            (value, false)
        }
    }
    // Indirect Y
    // TODO FIX
    pub fn indirect_indx(&mut self) -> (u16, bool) {
        let imm = self.imm();
        let data = self.read(imm) + self.cpu.reg.y & 0xff;
        let addr = (self.read(data as u16) as u16 | self.read(data as u16 + 1) as u16) << 8;
        let value: u16 = self.read(addr as u16) as u16;
        if (value.wrapping_sub(self.cpu.reg.y as u16) & 0xff00) != value & 0xff00 {
            (value, true)
        } else {
            (value, false)
        }
    }
    pub fn indirect(&mut self) -> u16 {
        let abs = self.abs();
        self.read16(abs) as u16
    }
    pub fn decode(&mut self) {
        let imm = self.imm();
        let opcode = self.read(imm);
        println!("{}", Instruction::mnemonic(opcode));
        self.cpu.opcode = opcode;
        // Debug print CPU values
        println!("{:?}", self.cpu);
        self.cpu.reg.prev_pc = self.cpu.reg.pc;

        // Address mode
        let mut m;
        match opcode {
            0x00 => self.brk(),
            0x01 => { m = self.imm(); self.bpl(m); },
            0x02 => ::std::process::exit(0x100),
            0x03 => { m = self.indirect_indx().0; self.slo(m); },
            0x04 => { m = self.imm(); self.rti(m); },
            0x05 => { m = self.zp(); self.ora(m); },
            0x06 => { m = self.zp(); self.asl(m); },
            0x07 => { m = self.zp(); self.slo(m); },
            0x08 => self.php(),
            0x09 => { m = self.imm(); self.ora(m); },
            0x0d => { m = self.abs(); self.ora(m); },
            0x10 => { m = self.imm(); self.bpl(m); },
            0x13 => { m = self.indirect_indx().0; self.slo(m); },
            0x15 => { m = self.zpx(); self.ora(m); },
            0x16 => { m = self.zp(); self.asl(m); },
            0x17 => { m = self.zp(); self.slo(m); },
            0x18 => self.clc(),
            0x1b => { m = self.absy().0; self.slo(m); },
            0x1c => self.nop(),
            0x1f => { m = self.abs(); self.slo(m); },
            0x0c => self.nop(),
            0x0e => { m = self.abs(); self.asl(m); },
            0x0a => self.asla(),
            0xa0 => { m = self.imm(); self.ldy(m); },
            0xa1 => { m = self.indirect_indx().0; self.lda(m); },
            0xa2 => { m = self.imm(); self.ldx(m); },
            0xa4 => { m = self.zp(); self.ldy(m); },
            0xa5 => { m = self.zp(); self.lda(m); },
            0xa6 => { m = self.zp(); self.ldx(m); },
            0xa8 => self.tay(),
            0xa9 => { m = self.imm(); self.lda(m); },
            0xaa => self.tax(),
            0xac => { m = self.abs(); self.ldy(m); },
            0xad => { m = self.abs(); self.lda(m); },
            0xae => { m = self.abs(); self.ldx(m); },
            0xaf => { m = self.abs(); self.lax(m); },
            0xb0 => { m = self.imm(); self.bcs(m); },
            0xb1 => { m = self.indirect_indx().0; self.lda(m); },
            0xb5 => { m = self.zp(); self.lda(m); },
            0xb6 => { m = self.zpy(); self.ldx(m); },
            0xb8 => self.clv(),
            0xb9 => { m = self.absy().0; self.lda(m); },
            0xba => self.tsx(),
            0xbc => { m = self.absx().0; self.ldy(m); },
            0xbd => { m = self.absx().0; self.lda(m); },
            0xbe => { m = self.absy().0; self.ldx(m); },
            0xc0 => { m = self.imm(); self.cpy(m); },
            0xc1 => { m = self.indirect_indx().0; self.cmp(m); },
            0xc5 => { m = self.zp(); self.cmp(m); },
            0x40 => { m = self.read(self.cpu.reg.pc + 1) as u16; self.rti(m); },
            0x41 => { m = self.indirect_indx().0; self.eor(m); },
            0x4a => self.lsra(),
            0x4e => { m = self.abs(); self.lsr(m); },
            0x4d => { m = self.abs(); self.eor(m); },
            0x45 => { m = self.zp(); self.eor(m); },
            0x46 => { m = self.zp(); self.lsr(m); },
            0x48 => self.pha(),
            0x49 => { m = self.imm(); self.eor(m); },
            0x28 => self.plp(),
            0x2a => self.rola(),
            0x2c => { m = self.abs(); self.bit(m); },
            0x2e => { m = self.abs(); self.rol(m); },
            0x20 => { m = self.abs(); self.jsr(m); },
            0x21 => { m = self.indirect_indx().0; self.and(m); },
            0x24 => { m = self.zp(); self.bit(m); },
            0x29 => { m = self.imm(); self.and(m); },
            0x2d => { m = self.abs(); self.and(m); },
            0x30 => { m = self.imm(); self.bmi(m); },
            0x35 => { m = self.zpx(); self.and(m); },
            0x36 => { m = self.zpx(); self.rol(m); },
            0x38 => self.sec(),
            0x3e => { m = self.absx().0; self.rol(m); },
            0x4c => { m = self.abs(); self.jmp(m); },
            0x50 => { m = self.imm(); self.bvc(m); },
            0x51 => { m = self.indx_indirect().0; self.eor(m); },
            0x54 => self.ign(),
            0x55 => { m = self.zpx(); self.eor(m); },
            0x5a => self.nop(),
            0x59 => { m = self.absy().0; self.eor(m); },
            0x5d => { m = self.absx().0; self.eor(m); },
            0x60 => self.rts(),
            0x61 => { m = self.indx_indirect().0; self.adc(m); },
            // TODO Dummy read for NOPs?
            0x64 => self.dop(AddressMode::ZeroPage),
            0x65 => { m = self.zp(); self.adc(m); },
            0x66 => { m = self.zp(); self.ror(m); },
            0x68 => self.pla(),
            0x6a => self.rora(),
            0x6c => { m = self.indirect(); self.jmp(m); },
            0x6d => { m = self.abs(); self.adc(m); },
            0x69 => { m = self.imm(); self.adc(m); },
            0x6e => { m = self.abs(); self.ror(m); },
            0x70 => { m = self.imm(); self.bvs(m); },
            0x72 => self.nop(),
            0x73 => self.nop(),
            0x78 => self.sei(),
            0x79 => { m = self.indirect_indx().0; self.adc(m); },
            0x7e => { m = self.absx().0; self.ror(m); },
            0x84 => { m = self.zp(); self.sty(m); },
            0x85 => { m = self.zp(); self.sta(m); },
            0x86 => { m = self.zp(); self.stx(m); },
            0x88 => self.dey(),
            0x8c => { m = self.abs(); self.sty(m); },
            0x8d => { m = self.abs(); self.sta(m); },
            0x8e => { m = self.abs(); self.stx(m); },
            0x90 => { m = self.imm(); self.bcc(m); },
            0x91 => { m = self.indx_indirect().0; self.sta(m); },
            0x95 => { m = self.zpx(); self.sta(m); },
            0x96 => { m = self.zpy(); self.stx(m); },
            0x98 => self.tya(),
            0x99 => { m = self.indirect_indx().0; self.sta(m); },
            0x9a => self.txs(),
            0x9d => { m = self.absx().0; self.sta(m); },
            0xc3 => self.dcp(),
            0xc4 => { m = self.zp(); self.cpy(m); },
            0xc6 => { m = self.zp(); self.dec(m); },
            0xc9 => { m = self.imm(); self.cmp(m); },
            0xca => self.dex(),
            0xce => { m = self.abs(); self.dec(m); },
            0xcc => { m = self.zp(); self.cpy(m); },
            0xd0 => { m = self.imm(); self.bne(m); },
            0xd2 => self.hlt(),
            0xd3 => self.dcp(),
            0xd6 => { m = self.zpx(); self.dec(m); },
            0xd8 => self.cld(),
            0xd9 => { m = self.absy().0; self.cmp(m); },
            0xde => { m = self.absx().0; self.dec(m); },
            0xdf => self.dcp(),
            0xe1 => { m = self.indx_indirect().0; self.sbc(m); },
            0xe4 => { m = self.zp(); self.cpx(m); },
            0xe5 => { m = self.zp(); self.sbc(m); },
            0xe6 => { m = self.zp(); self.inc(m); },
            0xe8 => self.inx(),
            0xe9 => { m = self.imm(); self.sbc(m); },
            0xea => self.nop(),
            0xec => { m = self.abs(); self.cpx(m); },
            0xed => { m = self.abs(); self.sbc(m); },
            0xc8 => self.iny(),
            0xf0 => { m = self.imm(); self.beq(m); },
            0xf1 => { m = self.indirect_indx().0; self.sbc(m); },
            0xf5 => { m = self.zp(); self.sbc(m); },
            0xf6 => { m = self.zp(); self.inc(m); },
            0xf7 => self.isc(AddressMode::ZeroPageX),
            0xf8 => self.sed(),
            0xfe => { m = self.absx().0; self.inc(m); },
            0xfd => { m = self.absx().0; self.sbc(m); },
            0xff => self.isc(AddressMode::AbsoluteX),
            0x1e => { m = self.absx().0; self.asl(m); },
            _ => unimplemented!("Unknown opcode:{:04x}", opcode),
        }
    }

    fn adc(&mut self, value: u16) {
        let a = self.cpu.reg.a as u16;
        let (result, overflow) = a.overflowing_add((self.read(value) as u16).wrapping_add(self.cpu.flags.carry as u16));
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
    }

    // Arithmetic shift left
    fn asl(&mut self, addr: u16) {
        // ASL shifts all bits left one position. 0 is shifted into bit 0
        // and the original bit 7 is shifted into the carry slot
        // Affected flags: S Z C

        let pc = self.cpu.reg.pc;
        let value: u8 = self.read(addr as u16);
        self.write(addr as u16, value as u8);
        // Shift data left by one and write it back to memory
        self.write(addr as u16, value << 1);
        self.cpu.flags.carry = value >> 1 & 0xfe != 0;
        self.cpu.flags.negative = (value & 0x80) != 0;
        self.cpu.flags.zero = (value & 0xff) == 0;
    }

    fn and(&mut self, value: u16) {
        let result = self.cpu.reg.a as u16 & self.read(value) as u16;
        self.cpu.reg.a = result as u8;
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
    }
    // Branch if Carry Set
    fn bcs(&mut self, value: u16) {
        if self.cpu.flags.carry {
            let offset = self.read(value) as i8 as u16;
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
        }
    }
    fn bcc(&mut self, value: u16) {
        if !self.cpu.flags.carry {
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            let offset = self.read(value) as i8 as u16;
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
            self.adv_cycles(1);
        }
        self.adv_cycles(2);
    }
    // Branch on Equal
    fn beq(&mut self, value: u16) {
        if self.cpu.flags.zero {
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            let offset = self.read(value) as i8 as u16;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
            self.adv_cycles(1);
        }
        self.adv_cycles(2);
    }
    // Branch if Minus
    fn bmi(&mut self, value: u16) {
        // 2 cycles (+ 1 if branch succeeds, +2 if to a new page)
        // This is for all branch instructions
        if self.cpu.flags.negative {
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            let offset = self.read(value) as i8 as u16;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
            self.adv_cycles(1);
        } else {
        }
        self.adv_cycles(2);
    }
    // Branch on Plus (if positive)
    fn bpl(&mut self, value: u16) {
        if !self.cpu.flags.negative {
            let offset = self.read(value) as i8 as u16;
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
            self.adv_cycles(3);
        } else {
            self.adv_cycles(2);
        }
    }

    fn brk(&mut self) {
        let addr = self.cpu.reg.pc.wrapping_add(1);
        self.push_word(addr);
        self.cpu.flags.brk = true;
        // TODO NMI (handle multiple vectors)
        self.cpu.reg.pc = self.read16(0xfffe);
    }
    // Test Bits N Z V
    fn bit(&mut self, value: u16) {
        // Read data at address
        let data = self.read(value) as u16;
        let a = self.cpu.reg.a as u16;
        self.cpu.flags.zero = (data & a) == 0;
        self.cpu.flags.negative = (data & 0x80) != 0;
        self.cpu.flags.overflow = (data & 0x40) != 0;
    }
    fn bne(&mut self, value: u16) {
        // If zero flag is 0 branch
        if !self.cpu.flags.zero {
            let offset = self.read(value) as i8 as u16;
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
            self.adv_cycles(3);
        } else {
            self.adv_cycles(2)
        }
    }
    // Branch on overflow clear
    fn bvc(&mut self, value: u16) {
        if !self.cpu.flags.overflow {
            let offset = self.read(value) as i8 as u16;
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
            self.adv_cycles(3);
        } else {
            self.adv_cycles(2);
        }

    }
    // Branch on overflow set
    fn bvs(&mut self, value: u16) {
        if self.cpu.flags.overflow {
            let offset = self.read(value) as i8 as u16;
            self.cpu.reg.prev_pc = self.cpu.reg.pc;
            self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
            self.adv_cycles(3);
        } else {
            self.adv_cycles(2);
        }
    }
    // Flag clear instructions
    fn clc(&mut self) { self.cpu.flags.carry = false; }
    fn cld(&mut self) { self.cpu.flags.decimal = false; }
    fn cli(&mut self) { self.cpu.flags.interrupt = false; }
    fn clv(&mut self) { self.cpu.flags.overflow = false; }

    // Compare with accumulator
    fn cmp(&mut self, value: u16) {
        let result = (self.cpu.reg.a as u16).wrapping_sub(self.read(value) as u16);
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.carry = (result & 0x01) != 0;
    }
    fn cpx(&mut self, value: u16) {
        let result = (self.cpu.reg.x as u16).wrapping_sub(self.read(value) as u16);
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.carry = (result & 0x01) != 0;
    }
    fn cpy(&mut self, value: u16) {
        let result = (self.cpu.reg.y as u16).wrapping_sub(self.read(value) as u16);
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.carry = (result & 0x01) != 0;
    }

    fn dec(&mut self, value: u16) {
        let result = self.read(value).wrapping_sub(1);
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
    }
    // Decrement Y register
    fn dey(&mut self) {
        self.cpu.reg.y = self.cpu.reg.y.wrapping_sub(1);
        self.cpu.flags.negative = (self.cpu.reg.y & 0x80) != 0;
        self.cpu.flags.zero = self.cpu.reg.y & 0xff == 0;
    }
    // Decrement X register
    fn dex(&mut self) {
        self.cpu.reg.x = self.cpu.reg.x.wrapping_sub(1);
        self.cpu.flags.negative = (self.cpu.reg.x & 0x80) != 0;
        self.cpu.flags.zero = self.cpu.reg.x & 0xff == 0;
    }
    // Decrement & compare
    fn dcp(&mut self) {
        println!("DCP (illegal opcode)");
        unimplemented!();
    }
    // Double NOP
    fn dop(&mut self, mode: AddressMode) {
        match mode {
            AddressMode::ZeroPage => {
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
    fn eor(&mut self, value: u16) {
        // Exclusive OR is performed on the accumulator's contents with the contents of a byte
        // of memory
        self.cpu.reg.a = (self.read(value) as u8 ^ self.cpu.reg.a) as u8;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.cpu.flags.zero = (self.cpu.reg.a & 0xff) == 0;
    }
    fn hlt(&self) {
        // Halt CPU
        eprintln!("HLT! Opcode:{:04x}", self.cpu.opcode);
        ::std::process::exit(0);
    }
    fn lax(&mut self, value: u16) {
        // Load both the accumulator and the X register with contents of a memory location
        // Part of the undocumented 6502 opcodes
        // (Sub-instructions: LDA, LDX)
        self.cpu.reg.a = self.read(value);
        self.cpu.reg.x = self.read(value);
        self.cpu.flags.zero = (value & 0xff) == 0;
        self.cpu.flags.negative = (value & 0x80) != 0;
    }
    fn ldy(&mut self, value: u16) {
        self.cpu.reg.y = self.read(value);
        self.cpu.flags.zero = (value & 0xff) == 0;
        self.cpu.flags.negative = (value & 0x80) != 0;
    }
    fn ldx(&mut self, value: u16) {
        self.cpu.reg.x = self.read(value);
        self.cpu.flags.zero = (value & 0xff) == 0;
        self.cpu.flags.negative = (value & 0x80) != 0;
    }
    fn lda(&mut self, value: u16) {
        self.cpu.reg.a = self.read(value);
        self.cpu.flags.zero = (value & 0xff) == 0;
        self.cpu.flags.negative = (value & 0x80) != 0;
    }
    fn lsr(&mut self, value: u16) {
        let data: u8 =  value as u8;
        let result = (data >> 1) | ((self.cpu.flags.carry as u8) << 7);
        self.write(value as u16, result);
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.carry = data & 1 != 0;
    }

    // Logical Shift Right (Accumulator)
    fn lsra(&mut self) {
        // Flags affected
        let carry = (self.cpu.reg.a & 1) != 0;
        self.cpu.reg.a = (self.cpu.reg.a >> 1) | ((self.cpu.flags.carry as u8) << 7);
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.cpu.flags.zero = (self.cpu.reg.a & 0xff) == 0;
        self.cpu.flags.carry = carry;
    }
    fn nop(&mut self) { self.bump(1,2); }
    fn ora(&mut self, value: u16) {
        self.cpu.reg.a = (self.read(value) as u8 | self.cpu.reg.a) as u8;
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
    }
    // Rotate one bit right memory or accumulator
    fn rol(&mut self, value: u16) {
        // Original bit is shifted into carry slot & carry is shifted into bit 7.
        let result = value << 1 & 0xfe;
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
    }
    fn ror(&mut self, value: u16) {
        // TODO check if result is correct
        // Assuming the slot is the same but the bit shift direction changes due to it being a
        // Rotate Right instruction
        // Original bit is shifted into carry slot & carry is shifted into bit 7.
        let result = value >> 1 & 0xfe;
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
    // TODO Cycles
    fn rts(&mut self) {
        let addr = self.pop16().wrapping_add(1);
        // let addr = self.pop_byte();

        // Set program counter for debug output
        self.cpu.reg.prev_pc = self.cpu.reg.pc;
        self.cpu.reg.pc = addr as u16;
    }
    // Return from interrupt
    fn rti(&mut self, value: u16) {
        // Pull processor flags from stack
        self.pop_byte();

        let sp = self.cpu.reg.sp;
        // TODO Check bitmask
        self.cpu.flags.negative = sp & 0x80 != 0;
        self.cpu.flags.zero = sp & 0x40 != 0;
        self.cpu.flags.carry = sp & 0x10 != 0;
        self.cpu.flags.interrupt = sp & 0x04 != 0;
        self.cpu.flags.decimal = sp & 0x01 != 0;
        // self.adv_cycles(6);
    }
    fn sed(&mut self) {
        self.cpu.flags.decimal = true;
    }

    fn sbc(&mut self, value: u16) {
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
    fn sloa(&mut self, value: u16) {
        let value = self.read(value) << 1;
        self.cpu.reg.a |= value;
        self.cpu.flags.carry = value & 0x80 != 0;
        self.cpu.flags.zero = value & 0xff == 0;
    }
    // Shift left one bit in memory, then OR the result with the accumulator
    // Part of undocumented opcodes
    fn slo(&mut self, value: u16) {
        let result = value << 1 & 0xff;
        self.cpu.reg.a |= result as u8;
        self.cpu.flags.carry = result & 0x80 != 0;
        self.cpu.flags.zero = result & 0xff == 0;
    }
    // TODO Check cpu flags (if we need to check zero & negative on zpx addr mode
    fn sta(&mut self, value: u16) { self.write(value, self.cpu.reg.a); }
    fn sty(&mut self, value: u16) { self.write(value, self.cpu.reg.y); }
    // TODO Check cpu flags (if we need to check zero & negative on zpx addr mode
    fn stx(&mut self, value: u16) { self.write(value, self.cpu.reg.x); }
    // Transfer Accumulator to X
    fn tax(&mut self) {
        self.cpu.reg.x = self.cpu.reg.a;
        self.cpu.flags.zero = (self.cpu.reg.x & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.x & 0x80) != 0;
    }
    // Transfer Y to Accumulator
    fn tay(&mut self) {
        self.cpu.reg.y = self.cpu.reg.a;
        self.cpu.flags.zero = (self.cpu.reg.y & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.y & 0x80) != 0;
    }
    // Transfer Y to Accumulator
    fn tya(&mut self) {
        self.cpu.reg.a = self.cpu.reg.y;
        self.cpu.flags.zero = (self.cpu.reg.a & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
    }
    // Transfer X to Accumulator
    fn txa(&mut self) {
        self.cpu.reg.a = self.cpu.reg.x;
        self.cpu.flags.zero = (self.cpu.reg.a & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
    }
    // Transfer X to Stack Pointer
    fn txs(&mut self) {
        let src = self.cpu.reg.x;
        self.cpu.flags.negative = (src & 0x80) != 0;
        self.cpu.flags.zero = (src & 0xff) != 0;
        self.cpu.reg.sp = self.cpu.reg.x;
    }
    // Transfer Stack Pointer to X
    fn tsx(&mut self) {
        let src = self.cpu.reg.sp;
        self.cpu.flags.negative = (src & 0x80) != 0;
        self.cpu.flags.zero = (src & 0xff) != 0;
        self.cpu.reg.x = self.cpu.reg.sp;
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
    // Pull
    fn pop_byte(&mut self) -> u8 {
        let sp = self.cpu.reg.sp;
        self.cpu.reg.sp = sp.wrapping_add(1);
        self.read(0x100 + self.cpu.reg.sp as u16)
    }
    fn pop16(&mut self) -> u16 {
        ((self.pop_byte() as u16) | (self.pop_byte() as u16) << 8)
    }
    // Push accumulator
    fn pha(&mut self) { self.push_byte(self.cpu.reg.a); }
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
        // Flag values OR BKR flag OR reserved flag (set to true for both)
       let flags = ps | 0x20 | 0x10;
        self.push_byte(flags);
    }

    // PuL1 (POP) Accumulator
    fn pla(&mut self) {
        // Pulls an 8-bit value from the stack and into the accumulator.
        // Flags affected zero & negative
        let value = self.pop_byte();
        self.cpu.flags.zero = (value & 0xff) == 0;
        self.cpu.flags.negative = (value & 0x80) != 0;
        self.cpu.reg.a = value & 0xff;
    }
    fn plp(&mut self) {
        let status = self.pop_byte();
        self.set_status_flags(status);
    }
    // Increment Memory
    fn inc(&mut self, value: u16) {
        let byte = self.read16(value) as u8;
        self.write(value, byte);
        self.cpu.flags.carry = value & 0x80 != 0;
        self.cpu.flags.zero = value & 0xff == 0;
    }
    // Increment X (implied mode)
    fn inx(&mut self) {
        self.cpu.reg.x = self.cpu.reg.x.wrapping_add(1);
        self.cpu.flags.zero = (self.cpu.reg.x & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.x & 0x80) != 0;
    }
    fn iny(&mut self) {
        self.cpu.reg.y = self.cpu.reg.y.wrapping_add(1);
        self.cpu.flags.zero = (self.cpu.reg.y & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.y & 0x80) != 0;
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
    }

    // Jump to Subroutine
    fn jsr(&mut self, value: u16) {
        self.cpu.reg.prev_pc = self.cpu.reg.pc;
        // let addr = self.read16(value);

        // Push to stack
        let pc = self.cpu.reg.pc;
        self.cpu.reg.prev_pc = self.cpu.reg.pc;
        self.push_word(pc - 1);
        // self.adv_cycles(3); // 6 if no jump?
        self.cpu.reg.pc = value;
    }
    fn jmp(&mut self, value: u16) {
        self.cpu.reg.prev_pc = self.cpu.reg.pc;
        self.cpu.reg.pc = value;
    }
    fn sec(&mut self) {
        self.cpu.flags.carry = true;
    }
    fn sei(&mut self) { self.cpu.flags.interrupt = true; }
    // ISC (Increase memory by one)
    fn isc(&mut self, mode: AddressMode) {
        let result = match mode {
            AddressMode::AbsoluteX => {
                // self.adv_pc(3);
                self.adv_cycles(7);

                let value = self.cpu.reg.pc.wrapping_add(1);
                let addr = self.read16(value).wrapping_add(self.cpu.reg.x as u16);
                if (value - self.cpu.reg.x as u16) & 0xff00 != value & 0xff00 { self.adv_cycles(1); }
                (self.cpu.reg.a as u16).wrapping_sub(addr as u16).wrapping_sub(self.cpu.flags.carry as u16)
            }
            AddressMode::ZeroPageX => {
                // self.adv_pc(2);
                self.adv_cycles(6);

                let value = self.cpu.reg.pc.wrapping_add(1);
                let addr = self.read16(value) & 0xff + self.cpu.reg.x as u16;
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
        self.cpu.reg.pc = self.read16(0xfffc);
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