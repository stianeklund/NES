use crate::interconnect::{MemoryMapper, AddressMatch};
use crate::opcode::Instruction;
use crate::memory::Ram;
use crate::rom::Cartridge;
use crate::ppu::{Ppu, FrameBuffer};
use crate::apu::Apu;
use log::{info, warn, debug, error};
use std::fmt::{LowerHex, Formatter, Error};

impl MemoryMapper for ExecutionContext {
    fn read8(&self, addr: u16) -> u8 {
        // self.adv_cycles(1);

        // See https://wiki.nesdev.com/w/index.php/CPU_memory_map
        match addr {
            0 ..= 0x07ff => self.ram.memory[addr as usize] as u8,
            0x0800 ..= 0x1fff => self.ram.memory[addr as usize & 0x07ff],
            0x2000 ..= 0x3fff => self.ppu.read8(addr),
            0x4000 ..= 0x4017 => self.apu.read8(addr),
            0x4018 ..= 0x401f => unimplemented!("Read to CPU Test space"),
            // $6000-$7FFF = Battery Backed Save or Work RAM
            0x6000 ..= 0x7fff => self.ram.sram[addr as usize] as u8,
            0x8000 ..= 0xffff => {
                let mask_amount = if self.cart.header.prg_rom_page_size != 1 { 0x7fff } else { 0x3fff };
                self.cart.prg[addr as usize & mask_amount]
            },
            _ => unimplemented!("Reads to ${:04x} is not implemented", addr),
        }
    }
    fn write8(&mut self, addr: u16, byte: u8) {
        match addr {
            0 ..= 0x07ff => self.ram.memory[addr as usize] = byte,
            0x0800 ..= 0x1fff => self.ram.memory[addr as usize & 0x07ff] = byte,

            // $2000-2FFF is normally mapped to the 2kB NES internal VRAM,
            // providing 2 nametables with a mirroring configuration controlled by the cartridge,
            // but it can be partly or fully remapped to RAM on the cartridge,
            // allowing up to 4 simultaneous nametables.

            0x2000 ..= 0x3fff => self.ppu.write8(addr,byte),
            0x4000 ..= 0x4017 => self.apu.write8(addr, byte),
            0x6000 ..= 0x7fff => { self.ram.sram[addr as usize] = byte; },
            // Some tests store ASCII characters in SRAM. Output as characters when writing to SRAM
            // println!("Status: {:04x}", self.ram.sram[0x6000]);
            0x8000 ..= 0xffff => self.cart.prg[addr as usize & 0x3fff] = byte,
            _ => eprintln!("Trying to write to memory address {:04x}", addr),
        };
        if self.debug {
            //Print known address names
            println!("{}", AddressMatch::resolve_addr(byte, addr));
        }
        // TODO
        // self.adv_cycles(1); // Each write uses one CPU cycle
    }
}
#[derive(Debug, Clone, Copy)]
pub struct AddressMode<T> {
    address: u16,
    data: T,
    byte_length: u8,
    cycle_length: u8,
}

// Implement lowerhex for addresses
impl<T> LowerHex for AddressMode<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let v: u16 = self.address;
        write!(f,"{:02x}", v)
    }

}
impl From<AddressMode<u8>> for AddressMode<u16> {
    fn from(item: AddressMode<u8>) -> Self {
        Self {
            address: item.address,
            data: u16::from(item.data),
            byte_length: item.byte_length,
            cycle_length: item.cycle_length
        }
    }
}
impl<T>AddressMode<T> {
    fn new(address: u16, data:T, byte_length: u8, cycle_length: u8) -> Self {
        Self {
            address,
            data,
            byte_length,
            cycle_length
        }
    }
    fn with_byte_length(address: u16, data: T, byte_length: u8, cycle_length:u8) -> Self {
        Self::new(address, data, byte_length, cycle_length)
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
        Self {
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
    p: u8,
}

impl Cpu {
    pub fn default() -> Self {
        Self {
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
            p: 0,
        }
    }
}
pub struct ExecutionContext {
    pub cpu: Cpu,
    pub cart: Cartridge,
    pub ram: Ram,
    pub ppu: Ppu,
    pub apu: Apu,
    debug: bool,
}
impl ExecutionContext {
    pub fn new() -> Self {
        Self {
            cpu: Cpu::default(),
            cart: Cartridge::default(),
            ram: Ram::default(),
            ppu: Ppu::default(),
            apu: Apu::default(),
            debug: false,
        }
    }
    // Helper functions for incrementing and decrementing PC register and cycle count.
    // fn adv_pc(&mut self, amount: u16) { self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(amount); }
    fn adv_pc(&mut self, offset: u16) {
        self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(offset);
    }
    fn adv_cycles(&mut self, amount: u16) {
       self.cpu.cycles = self.cpu.cycles.wrapping_add(amount);
    }
    fn adv(&mut self, mode: AddressMode<u16>) {
        if self.cpu.opcode == 0x4c | 0x6c | 0x20 | 0x90 | 0xb0 | 0xf0 | 0x30 | 0xd0 | 0x10 | 0x40 | 0x60 {
            self.adv_cycles(mode.cycle_length.into());
        } else {
            self.adv_pc(mode.byte_length.into());
            self.adv_cycles(mode.cycle_length.into());
        }
    }
    fn fetch_byte(&mut self) -> u8 {
        let v = self.read8(self.cpu.reg.pc);
        self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(1);
        v as u8
    }
    fn fetch_word(&mut self) -> u16 {
        let v = self.read16(self.cpu.reg.pc);
        self.cpu.reg.pc = self.cpu.reg.pc.wrapping_add(2);
        v
    }
    fn implied(&self, cycles: u8) -> AddressMode<u8> {
        let address = u16::from(self.read8(self.cpu.reg.pc));
        AddressMode {
            address,
            data: self.read8(address),
            byte_length: 0, // should be 1
            cycle_length: cycles
        }
    }

    // Addressing modes (same byte & cycle length as accumulator operations)
    fn imm(&self) -> AddressMode<u8> {
        let address = u16::from(self.read8(self.cpu.reg.pc));
        AddressMode {
            address,
            data: self.read8(address),
            byte_length: 1,
            cycle_length: 2
        }
    }
    // TODO Remove? This isn't a real mode
    fn imm16(&self) -> AddressMode<u16> {
        let address = self.read16(self.cpu.reg.pc);
        AddressMode{
            address,
            data: self.read16(address),
            byte_length: 2,
            cycle_length: 2
        }

    }
    fn abs(&self) -> AddressMode<u16> {
        let address = self.read16(self.cpu.reg.pc);
        AddressMode{
            address,
            data: u16::from(self.read8(address)),
            byte_length: 0, // 3 bytes (but we don't want to increment PC)
            cycle_length: 4
        }
    }
    fn abs_x(&self) -> AddressMode<u16> {
        // let addr = self.fetch_word() + u16::from(self.cpu.reg.x);
        let address = self.read16(self.cpu.reg.pc) + u16::from(self.cpu.reg.x);
        AddressMode {
            address,
            data: u16::from(self.read8(address)),
            byte_length: 2,
            cycle_length: 4
        }
    }
    fn abs_y(&self) -> AddressMode <u16> {
        // let address: u16 = self.fetch_word() + u16::from(self.cpu.reg.y);
        let address: u16 = self.read16(self.cpu.reg.pc) + u16::from(self.cpu.reg.y);
        AddressMode {
            address,
            data: u16::from(self.read8(address)),
            byte_length: 2,
            cycle_length: 4
        }
    }
    fn branch(&self) -> AddressMode <u8> {
        let address = u16::from(self.read8(self.cpu.reg.pc));
        AddressMode {
            address,
            data: self.read8(address),
            byte_length: 1, // 2 bytes
            cycle_length: 2
        }
    }
    fn zp(&self) -> AddressMode <u16> {
        let address = u16::from(self.read8(self.cpu.reg.pc));
        AddressMode {
            address,
            data: u16::from(self.read8(address)),
            byte_length: 1, // (should be 2), but we already increment PC by one in the fetch
            cycle_length: 3
        }
    }
    fn zp_x (&self) -> AddressMode <u8> {
        let address = (self.read16(self.cpu.reg.pc) + u16::from(self.cpu.reg.x)) & 0xff;
        let data = self.read8(address);
        eprintln!("{:x}, data:{:x}", address, data);
        AddressMode {
            address,
            data: self.read8(address),
            byte_length: 1,
            cycle_length: 4
        }
    }
    fn zp_y (&self) -> AddressMode <u8> {
        let address = self.read16(self.cpu.reg.pc) + u16::from(self.cpu.reg.y & 0xff);
        AddressMode {
            address,
            data: self.read8(address),
            byte_length: 2,
            cycle_length: 4
        }
    }
    // JMP uses this
    fn indirect(&self) -> AddressMode <u16> {
        let address = self.read16(self.cpu.reg.pc);
        let addr = self.read16(self.cpu.reg.pc + 1);
        AddressMode {
            address,
            data: self.read16(address),
            byte_length: 0, // Byte length 3
            cycle_length: 5
        }
    }
    fn indirect_x(&self) -> AddressMode <u16> {
        let address = self.read8(self.cpu.reg.pc);
        AddressMode {
            address: u16::from(address),
            data: self.read16((address & 0xff as u8).wrapping_add(self.cpu.reg.x).into()),
            byte_length: 2,
            cycle_length: 6
        }
    }
    fn indirect_y(&self) -> AddressMode <u16> {
        let address = self.read8(self.cpu.reg.pc);
        AddressMode {
            address: u16::from(address),
            data: self.read16((address & 0xff as u8).wrapping_add(self.cpu.reg.y).into()),
            byte_length: 2,
            cycle_length: 6
        }
    }

    pub fn decode(&mut self) {
        let opcode = self.fetch_byte(); // fetch increments PC by one.
        self.cpu.opcode = opcode as u8;
        self.cpu.p = self.get_status_flags();
        // Make debug printing look like Nintendulator
        if !self.debug {
            info!("{:04X}  {:0X} {:02X}     {} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
                  self.cpu.reg.pc - 1 , opcode, // Addr
                  self.read8(self.cpu.reg.pc),  // Operand
                  Instruction::short_mnemonic(opcode),
                  self.cpu.reg.a, self.cpu.reg.x, self.cpu.reg.y,
                  self.cpu.p, self.cpu.reg.sp);
        }
        // let mode = AddressMode::new(self.cpu.reg.pc, self.read8(self.cpu.reg.pc), 1, 2);
        let mode: AddressMode<u16> = match opcode {

            0x00 => self.brk(),
            0x01 => self.ora(self.indirect_x()),
            0x02 => ::std::process::exit(0x100),
            // 0x03 => self.slo(&self.indirect_x()),
            0x04 | 0x0c | 0x1c | 0x5a | 0x54 | 0x72 | 0x73 | 0xea => self.nop(), // 0x04 ZP?
            0x05 => self.ora(self.zp()),
            0x06 => self.asl(self.zp()),
            // 0x07 => self.slo(&self.zp().into()),
            0x08 => self.php(AddressMode::from(self.implied(3))),
            0x09 => self.ora(self.imm().into()),
            0x0d => self.ora(self.abs()),
            0x10 => self.bpl(self.branch()),
            0x11 => self.ora(self.indirect_y()),
            0x13 => self.slo(self.indirect_y()),
            0x15 => self.ora(self.zp_x().into()),
            0x16 => self.asl(self.zp_x().into()),
            // 0x17 => self.slo(&self.zp().into()),
            0x18 => self.clc(AddressMode::from(self.implied(2))),
            0x19 => self.ora(self.abs_y()),
            0x1d => self.ora(self.abs_x()),
            0x1b => self.slo(self.abs_y()),
            0x1f => self.slo(self.abs()),
            0x0e => self.asl(self.abs()),
            0x0a => self.asla(AddressMode::from(self.implied(2))),
            0xa0 => self.ldy(self.imm().into()),
            0xa1 => self.lda(self.indirect_x()),
            0xa2 => self.ldx(self.imm().into()),
            0xa4 => self.ldy(self.zp()),
            0xa5 => self.lda(self.zp()),
            0xa6 => self.ldx(self.zp()),
            0xa8 => self.tay(AddressMode::from(self.implied(2))),
            0xa9 => self.lda(self.imm().into()),
            0xaa => self.tax(AddressMode::from(self.implied(2))),
            0xac => self.ldy(self.abs()),
            0xad => self.lda(self.abs()),
            0xae => self.ldx(self.abs()),
            0xaf => self.lax(self.abs()),
            0xb0 => self.bcs(self.branch()),
            0xb1 => self.lda(self.indirect_y()),
            0xb5 => self.lda(self.zp_x().into()),
            0xb6 => self.ldx(self.zp_y().into()),
            0xb8 => self.clv(AddressMode::from(self.implied(2))),
            0xb9 => self.lda(self.abs_y()),
            0xba => self.tsx(AddressMode::from(self.implied(2))),
            0xbc => self.ldy(self.abs_x()),
            0xbd => self.lda(self.abs_x()),
            0xbe => self.ldx(self.abs_y()),
            0xc0 => self.cpy(self.imm().into()),
            0xc1 => self.cmp(self.indirect_y()),
            0xc5 => self.cmp(self.zp()),
            0x40 => self.rti(AddressMode::from(self.implied(6))),
            0x41 => self.eor(self.indirect_y()),
            0x4a => self.lsra(AddressMode::from(self.implied(2))),
            0x4e => self.lsr(self.abs()),
            0x4d => self.eor(self.abs()),
            0x45 => self.eor(self.zp()),
            0x46 => self.lsr(self.zp()),
            0x48 => self.pha(AddressMode::from(self.implied(3))),
            0x49 => self.eor(self.imm().into()),
            0x20 => self.jsr(self.abs()),
            0x21 => self.and(self.indirect_y()),
            0x24 => self.bit(self.zp()),
            0x25 => self.and(self.zp()),
            0x26 => self.rol(self.zp()),
            0x28 => self.plp(AddressMode::from(self.implied(4))),
            0x29 => self.and(self.imm().into()),
            0x2a => self.rola(AddressMode::from(self.implied(2))),
            0x2c => self.bit(self.abs()),
            0x2e => self.rol(self.abs()),
            0x2d => self.and(self.abs()),
            0x30 => self.bmi(self.branch()),
            0x35 => self.and(self.zp_x().into()),
            0x36 => self.rol(self.zp_x().into()),
            0x38 => self.sec(AddressMode::from(self.implied(2))),
            0x3e => self.rol(self.abs_x()),
            0x4c => self.jmp(self.abs()),
            0x50 => self.bvc(self.branch()),
            0x51 => self.eor(self.indirect_x()),
            0x55 => self.eor(self.zp_x().into()),
            0x59 => self.eor(self.abs_y()),
            0x5d => self.eor(self.abs_x()),
            0x60 => self.rts(AddressMode::from(self.implied(6))),
            0x61 => self.adc(self.indirect_x()),
            // TODO Dummy read for NOPs?
            // 0x64 => self.dop(),
            0x65 => self.adc(self.zp()),
            0x66 => self.ror(self.zp()),
            0x68 => self.pla(AddressMode::from(self.implied(4))),
            0x6a => self.rora(AddressMode::from(self.implied(2))),
            0x6c => self.jmp(self.indirect()),
            0x6d => self.adc(self.abs()),
            0x69 => self.adc(self.imm().into()),
            0x6e => self.ror(self.abs()),
            0x70 => self.bvs(self.branch()),
            0x78 => self.sei(AddressMode::from(self.implied(2))),
            0x79 => self.adc(self.indirect_y()),
            0x7e => self.ror(self.abs_x()),
            0x81 => self.sta(self.indirect_x()),
            0x84 => self.sty(self.zp()),
            0x85 => self.sta(self.zp()),
            0x86 => self.stx(self.zp()),
            0x88 => self.dey(AddressMode::from(self.implied(2))),
            0x8a => self.txa(AddressMode::from(self.implied(2))),
            0x8c => self.sty(self.abs()),
            0x8d => self.sta(self.abs()),
            0x8e => self.stx(self.abs()),
            0x90 => self.bcc(self.branch()),
            0x91 => self.sta(self.indirect_y()),
            0x95 => self.sta(AddressMode::from(self.zp_x())),
            0x96 => self.stx(AddressMode::from(self.zp_y())),
            0x98 => self.tya(AddressMode::from(self.implied(2))),
            0x99 => self.sta(self.abs_y()),
            0x9a => self.txs(AddressMode::from(self.implied(2))),
            0x9d => self.sta(self.abs_x()),
            // 0xc3 => self.dcp(), // ILLEGAL
            0xc4 => self.cpy(self.zp()),
            0xc6 => self.dec(self.zp()),
            0xc9 => self.cmp(self.imm().into()),
            0xca => self.dex(AddressMode::from(self.implied(2))),
            0xce => self.dec(self.abs()),
            0xcd => self.cmp(self.abs()),
            0xcc => self.cpy(self.abs()),
            0xd0 => self.bne(self.branch()),
            // 0xd2 => self.hlt(mode),
            // 0xd3 | 0xdf => self.dcp(),
            0xd6 => self.dec(self.zp_x().into()),
            0xd8 => self.cld(AddressMode::from(self.implied(2))),
            0xd9 => self.cmp(self.abs_y()),
            0xde => self.dec(self.abs_x()),
            0xe0 => self.cpx(self.imm().into()),
            0xe1 => self.sbc(self.indirect_x()),
            0xe4 => self.cpx(self.zp()),
            0xe5 => self.sbc(self.zp()),
            0xe6 => self.inc(self.zp()),
            0xe8 => self.inx(AddressMode::from(self.implied(2))),
            0xe9 => self.sbc(self.imm().into()),
            0xec => self.cpx(self.abs()),
            0xed => self.sbc(self.abs()),
            0xc8 => self.iny(AddressMode::from(self.implied(2))),
            0xf0 => self.beq(self.branch()),
            0xf1 => self.sbc(self.indirect_y()),
            0xf5 => self.sbc(self.zp_x().into()),
            0xf6 => self.inc(self.zp_x().into()),
            0xf7 => self.isc(self.zp()),
            0xf8 => self.sed(AddressMode::from(self.implied(2))),
            0xfe => self.inc(self.abs_x()),
            0xfd => self.sbc(self.abs_x()),
            0xff => self.isc(self.abs_x()),
            0x1e => self.asl(self.abs_x()),
            _ => unimplemented!("Unknown opcode:{:04x}", opcode),
        };
        self.adv(mode);
    }

    fn adc(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        let a = u16::from(self.cpu.reg.a);
        // let operand = self.read(value);
        let result = self.cpu.reg.a.wrapping_add(value.data as u8).wrapping_add(self.cpu.flags.carry as u8);
        self.cpu.reg.a = result as u8;
        // Set overflow flag when A and the operand have the same sign
        // and A and the result have different sign
        self.cpu.flags.overflow = !(a ^ value.data as u16) & (a ^ u16::from(result)) & 0x80 != 0;
        self.cpu.flags.carry = a >= u16::from(result);
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = self.cpu.reg.a == 0;
        value
    }
    // ASL (Accumulator) helper function for ASL Accumulator
    fn asla(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        let a: u8 =  self.cpu.reg.a;
        let result = a << 1;
        self.cpu.reg.a = result;
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = result == 0;
        // Check old contents
        self.cpu.flags.carry = (a & 0x80) != 0;
        mode
    }

    // Arithmetic shift left
    fn asl(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        // ASL shifts all bits left one position. 0 is shifted into bit 0
        // and the original bit 7 is shifted into the carry slot
        // Affected flags: S Z C
        // let operand: u8 =  self.read(addr);
        let result = (value.data as u8) << 1;
        self.write8(value.data as u16, result);
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = result == 0;
        self.cpu.flags.carry = (value.data & 0x80) != 0;
        value
    }

    fn and(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        self.cpu.reg.a &= value.data as u8;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.cpu.flags.zero = self.cpu.reg.a == 0;
        value
    }
    // Branch if Carry Set
    fn bcs(&mut self, offset: AddressMode <u8>) -> AddressMode<u16> {
        if self.cpu.flags.carry {
            self.adv_pc(offset.address);
        }
        self.check_branch(offset);
        AddressMode::from(offset)
    }
    fn bcc(&mut self, offset: AddressMode <u8>) -> AddressMode<u16> {
        if !self.cpu.flags.carry {
            self.adv_pc(offset.address);
        } else {

            self.check_branch(offset);
        }
        AddressMode::from(offset)
    }
    // Branch on Equal
    fn beq(&mut self, offset: AddressMode <u8>) -> AddressMode<u16> {
        if self.cpu.flags.zero {
            self.adv_pc(offset.address);
        }
        self.check_branch(offset);
        AddressMode::from(offset)
    }
    fn check_branch(&mut self, v: AddressMode <u8>) -> AddressMode<u16> {
        if self.cpu.reg.prev_pc & 0xFF00 != self.cpu.reg.pc & 0xFF00 {
            self.adv_cycles(2);
        } else {
            self.adv_cycles(1)
        };
        AddressMode::from(v)
    }
    // Branch if Minus
    fn bmi(&mut self, offset: AddressMode <u8>) -> AddressMode<u16> {
        if self.cpu.flags.negative {
            self.adv_pc(u16::from(offset.data));
        }
        self.check_branch(offset);
        AddressMode::from(offset)
    }
    // Branch on Plus (if positive)
    fn bpl(&mut self, offset: AddressMode <u8>) -> AddressMode<u16> {
        /*if !self.cpu.flags.negative {
            self.adv_pc(offset.data);
        }*/
        self.check_branch(offset);
        AddressMode::from(offset)
    }
    fn bne(&mut self, offset: AddressMode <u8>) -> AddressMode<u16> {
        if !self.cpu.flags.zero {
            self.adv_pc(offset.address);
        }
        // TODO Double check cycles
        // Have we crossed a boundary?
        self.check_branch(offset);
        AddressMode::from(offset)
    }
    // Branch on overflow clear
    fn bvc(&mut self, offset: AddressMode <u8>) -> AddressMode<u16> {
        if !self.cpu.flags.overflow {
            self.adv_pc(offset.address);
        }
        self.check_branch(offset);
        AddressMode::from(offset)
    }
    // Branch on overflow set
    fn bvs(&mut self, offset: AddressMode <u8>) -> AddressMode<u16> {
        if self.cpu.flags.overflow {
            self.adv_pc(offset.address);
        }
        self.check_branch(offset);
        AddressMode::from(offset)
    }
    fn brk(&mut self) -> AddressMode<u16> {
        self.cpu.flags.brk = true;
        self.push_word(self.cpu.reg.pc + 1);
        // Set PC to IRQ vector
        self.cpu.reg.pc = self.read16(0xfffe);
        // For now Panic here
        // panic!("BRK PC:{:04x}", self.cpu.reg.pc);
        AddressMode::with_byte_length(self.cpu.reg.pc, u16::from(self.read8(self.cpu.reg.pc)), 1, 7)
    }
    // Test Bits N Z V
    fn bit(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        let a = self.cpu.reg.a;
        // warn!("a:{:02x} pointer:{:04x}", a, self.read(data as u16));
        self.cpu.flags.zero = (value.data as u8 & a) == 0;
        self.cpu.flags.negative = (value.data as u8 & 0x80) != 0;
        self.cpu.flags.overflow = (value.data as u8 & 0x40) != 0;
        value
    }

    // Flag clear instructions
    fn clc(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> { self.cpu.flags.carry = false; mode }
    fn cld(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> { self.cpu.flags.decimal = false; mode }
    fn cli(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> { self.cpu.flags.interrupt = false; mode }
    fn clv(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> { self.cpu.flags.overflow = false; mode }

    // Compare with accumulator
    fn cmp(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        // let value = self.read8(addr);
        let result = self.cpu.reg.a.wrapping_sub(value.data as u8);
        self.cpu.flags.zero = result == 0;
        self.cpu.flags.carry = self.cpu.reg.a >= value.data as u8;
        self.cpu.flags.negative = result & 0x80 != 0;
        value
    }
    fn cpx(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        // let value = self.read8(value);
        let result = self.cpu.reg.x.wrapping_sub(value.data as u8);
        self.cpu.flags.zero = result == 0;
        self.cpu.flags.carry = self.cpu.reg.x>= value.data as u8;
        self.cpu.flags.negative = result & 0x80 != 0;
        value
    }
    fn cpy(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        // let value = self.read8(value);
        let result = self.cpu.reg.y.wrapping_sub(value.data  as u8);
        self.cpu.flags.zero = result == 0;
        // self.cpu.flags.zero = y == value;
        self.cpu.flags.carry = self.cpu.reg.y >= value.data as u8;
        self.cpu.flags.negative = result & 0x80 != 0;
        value
    }

    fn dec(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        let result = value.data.wrapping_sub(1) as u8;
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = (result & 0xff) == 0;
        value
    }
    // Decrement Y register
    fn dey(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        self.cpu.reg.y = self.cpu.reg.y.wrapping_sub(1);
        self.cpu.flags.negative = (self.cpu.reg.y & 0x80) != 0;
        self.cpu.flags.zero = self.cpu.reg.y == 0;
        mode
    }
    // Decrement X register
    fn dex(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        self.cpu.reg.x = self.cpu.reg.x.wrapping_sub(1);
        self.cpu.flags.negative = (self.cpu.reg.x & 0x80) != 0;
        self.cpu.flags.zero = self.cpu.reg.x & 0xff == 0;
        mode
    }
    // Decrement & compare
    // fn dcp(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> { println!("DCP (illegal opcode)"); mode}
    // Double NOP
    // fn dop(&mut self) { self.adv_cycles(1); }
    // Exclusive OR (XOR)
    fn eor(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        // Exclusive OR is performed on the accumulator's contents with the contents of a byte
        // of memory
        let a = self.cpu.reg.a;
        self.cpu.reg.a = (value.data ^ a as u16) as u8;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.cpu.flags.zero = self.cpu.reg.a == 0;
        value
    }
    fn hlt(&self) { panic!("HLT! Opcode:{:04x}", self.cpu.opcode); }
    fn lax(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        // Load both the accumulator and the X register with contents of a memory location
        // Part of the undocumented 6502 opcodes
        // (Sub-instructions: LDA, LDX)
        // let result = self.read8(value);
        let result = value.data;
        self.cpu.reg.a = result as u8;
        self.cpu.reg.x = result as u8;
        self.cpu.flags.zero = result == 0;
        self.cpu.flags.negative = (result & 0x80) != 0;
        value
    }
    fn ldy(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        // let result = self.read8(value);
        let result = value.data as u8;
        self.cpu.reg.y = result;
        self.cpu.flags.zero = result == 0;
        self.cpu.flags.negative = (result & 0x80) != 0;
        value
    }
    fn ldx(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        // debug!("LDX {:x} {:x}", self.read8(self.cpu.reg.pc), self.read8(self.cpu.reg.pc + 1));
        self.cpu.reg.x = value.data as u8;
        self.cpu.flags.zero = value.data == 0;
        self.cpu.flags.negative = (value.data & 0x80) != 0;
        value
    }
    fn lda(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        self.cpu.reg.a = value.data as u8;
        self.cpu.flags.zero = value.data  == 0;
        self.cpu.flags.negative = (value.data & 0x80) != 0;
        value
    }

    // LSR (only memory operations, see lsra for Accumulator)
    fn lsr(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        // let value: u8 =  self.read8(addr.);
        let data = value.data as u8;
        let result = data >> 1;
        self.write8(value.address, result);
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = result == 0;
        self.cpu.flags.carry = (data & 0x1) != 0;
        value
    }

    // Logical Shift Right (Accumulator)
    fn lsra(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        // Flags affected
        let value = self.cpu.reg.a;
        let result = value >> 1;
        self.cpu.reg.a  = result;
        self.cpu.flags.negative = result & 0x80 != 0;
        self.cpu.flags.zero = result == 0;
        self.cpu.flags.carry = (value & 0x1) != 0;
        mode
    }
    fn nop(&self) -> AddressMode<u16> {
        AddressMode::with_byte_length(self.cpu.reg.pc, self.read8(self.cpu.reg.pc) as u16, 0, 2)
    }
    fn ora(&mut self, value: AddressMode<u16>) -> AddressMode<u16> {
        self.cpu.reg.a = (value.data as u8) | (self.cpu.reg.a) as u8;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        self.cpu.flags.zero = self.cpu.reg.a == 0;
        value
    }
    // ROL (accumulator)
    fn rola(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        let old_carry = self.cpu.flags.carry;
        self.cpu.flags.carry = (self.cpu.reg.a & 0x80) != 0;
        let mut result = self.cpu.reg.a << 1;
        if old_carry { result |= 1; }
        self.cpu.reg.a = result;
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = result == 0;
        mode
    }
    // Rotate one bit right memory
    fn rol(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        // Move each of the bits in either A or M one place to the left.
        // Bit 0 is filled with the current value of the carry flag
        // the old bit 7 becomes the new carry flag value.
        let operand = self.read8(value.address);
        let old_carry = self.cpu.flags.carry;
        self.cpu.flags.carry = (operand & 0x80) != 0;
        let mut result = operand << 1;
        if old_carry { result |= 1; }
        self.write8(value.address, result);
        // Set remaining flags
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = result == 0;
        value
    }
    fn rora(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        // Bit 0 is filled with the current value of the carry flag.
        // Old bit 7 become new carry flag value
        let operand = self.cpu.reg.a;
        let old_carry = self.cpu.flags.carry;
        self.cpu.flags.carry = (operand  & 0x01) != 0;
        let mut result = operand >> 1;
        if old_carry { result |= 0x80; }
        self.cpu.reg.a = result;
        // Set remaining flags
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = result == 0;
        // AddressMode::with_byte_length(self.cpu.reg.pc, self.read8(self.cpu.reg.pc).into(), 1, 2)
        mode
    }
    fn ror(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        let operand = value.data;
        let old_carry = self.cpu.flags.carry;
        // Set carry flag with bit 0 of operand
        self.cpu.flags.carry = (operand  & 0x01) != 0;
        let mut result = operand >> 1;
        if old_carry { result |= 0x80; }
        self.write8(value.address, result as u8);
        // Set remaining flags
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.flags.zero = result == 0;
        value
    }
    fn rts(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        // let addr = self.pop16().wrapping_add(1);
        // Set program counter for debug output
        self.cpu.reg.pc = self.pop16() + 1;
        self.adv_cycles(6);
        // TODO Impl proper mode?
        mode
    }
    // Return from interrupt
    fn rti(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        // TODO value is not used
        // Pull processor flags from stack
        let flags = self.pop_byte();
        self.set_status_flags(flags);
        self.cpu.reg.pc = self.pop16();
        mode
    }
    fn sed(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> { self.cpu.flags.decimal = true; mode }

    fn sbc(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        let a = self.cpu.reg.a as u16;
        let operand = !value.data as u8;
        let result = self.cpu.reg.a.wrapping_add(operand).wrapping_add(self.cpu.flags.carry as u8);
        // TODO This doesn't work, find out why
        //if result > 255 { self.cpu.flags.carry = true; }
        self.cpu.reg.a = result as u8;
        self.cpu.flags.overflow = !(a ^ operand as u16) & (a ^ result as u16) & 0x80 != 0;
       //  self.cpu.flags.overflow = (a ^ operand) & (a ^ result) & 0x80 != 0;
        // Check if there's a carry between bit 6 & 7 and the operand is not 0 set carry
        self.cpu.flags.carry = (result as u16) <= (a as u16) && operand != 0;
        self.cpu.flags.negative = result & 0x80 != 0;
        self.cpu.flags.zero = result == 0;
        value
    }
    // SLO Accumulator: UNOFFICIAL OPCODE
    fn sloa(&mut self, value: AddressMode <u16>) -> AddressMode<u16>  {
        let result = (value.data << 1) as u8;
        self.cpu.reg.a |= result;
        self.cpu.flags.carry = result & 0x80 != 0;
        self.cpu.flags.zero = result == 0;
        value
    }
    // Shift left one bit in memory, then OR the result with the accumulator
    // Part of undocumented opcodes
    fn slo(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        let result = value.data << 1;
        self.cpu.reg.a |= result as u8;
        self.cpu.flags.carry = result & 0x80 != 0;
        self.cpu.flags.zero = result == 0;
        value
    }
    fn sta(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> { self.write8(mode.address, self.cpu.reg.a); mode }
    fn sty(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> { self.write8(mode.address, self.cpu.reg.y); mode }
    fn stx(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> { self.write8(mode.address, self.cpu.reg.x); mode }
    // Transfer Accumulator to X
    fn tax(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        self.cpu.reg.x = self.cpu.reg.a;
        self.cpu.flags.zero = self.cpu.reg.x == 0;
        self.cpu.flags.negative = (self.cpu.reg.x & 0x80) != 0;
        mode
    }
    // Transfer Y to Accumulator
    fn tay(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        self.cpu.reg.y = self.cpu.reg.a;
        self.cpu.flags.zero = self.cpu.reg.y == 0;
        self.cpu.flags.negative = (self.cpu.reg.y & 0x80) != 0;
        mode
    }
    // Transfer Y to Accumulator
    fn tya(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        self.cpu.reg.a = self.cpu.reg.y;
        self.cpu.flags.zero = self.cpu.reg.a == 0;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        mode
    }
    // Transfer X to Accumulator
    fn txa(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        self.cpu.reg.a = self.cpu.reg.x;
        self.cpu.flags.zero = self.cpu.reg.a == 0;
        self.cpu.flags.negative = (self.cpu.reg.a & 0x80) != 0;
        mode
    }
    // Transfer X to Stack Pointer
    fn txs(&mut self, mode: AddressMode<u16>) -> AddressMode<u16>  { self.cpu.reg.sp = self.cpu.reg.x; mode }
    // Transfer Stack Pointer to X
    fn tsx(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        self.cpu.reg.x = self.cpu.reg.sp;
        self.cpu.flags.zero = self.cpu.reg.x == 0;
        self.cpu.flags.negative = (self.cpu.reg.x & 0x80) != 0;
        mode
    }
    fn push_word(&mut self, value: u16) {
        let sp = self.cpu.reg.sp;
        self.write16(0x100 + u16::from(sp.wrapping_sub(1)), value);
        self.cpu.reg.sp = self.cpu.reg.sp.wrapping_sub(2);
    }
    // Push register
    fn push_byte(&mut self, byte: u8) {
        let sp = self.cpu.reg.sp;
        self.write8(0x100 + u16::from(sp), byte);
        self.cpu.reg.sp = self.cpu.reg.sp.wrapping_sub(1);
    }
    // Pull
    fn pop_byte(&mut self) -> u8 {
        let sp = self.cpu.reg.sp;
        self.cpu.reg.sp = sp.wrapping_add(1);
        self.read8(0x100_u16.wrapping_add(u16::from(self.cpu.reg.sp)))
    }
    fn pop16(&mut self) -> u16 {
        u16::from(self.pop_byte()) | u16::from(self.pop_byte()).wrapping_shl(8)
    }
    // Push accumulator
    fn pha(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> { self.push_byte(self.cpu.reg.a); mode }
    pub fn get_status_flags(&self) -> u8 {
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
        // TODO FIX BRK & Reserved are not actual flags but exist in stack copies
        // They only exist when pushing the status register onto the stack!
        // self.cpu.flags.reserved = value & 0x20 == 0x20;
        // self.cpu.flags.brk = value & 0x10 == 0x10;
        self.cpu.flags.decimal = value & 0x08 == 0x08;
        self.cpu.flags.interrupt = value & 0x04 == 0x04;
        self.cpu.flags.zero = value & 0x02 == 0x02;
        self.cpu.flags.carry = value & 0x01 == 0x01;
    }
    // Push Processor Status
    fn php(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        // Pushes a copy of the status flags to the stack
        let ps = self.get_status_flags();
        // Flag values OR BKR flag OR reserved flag (set to true for both)
        let flags = ps | 0x20 | 0x10;
        self.push_byte(flags);
        mode
    }

    // PuL1 (POP) Accumulator
    fn pla(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        // Pulls an 8-bit value from the stack and into the accumulator.
        // Flags affected zero & negative
        let value = self.pop_byte();
        self.cpu.flags.zero = (value & 0xff) == 0;
        self.cpu.flags.negative = (value & 0x80) != 0;
        self.cpu.reg.a = value & 0xff;
        mode
    }
    fn plp(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        let status = self.pop_byte();
        self.set_status_flags(status);
        mode
    }
    // Increment Memory
    fn inc(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        // let byte = self.read_word(value) as u8;
        self.write8(value.address, value.data as u8);
        self.cpu.flags.carry = value.data & 0x80 != 0;
        self.cpu.flags.zero = value.data & 0xff == 0;
        value
    }
    // Increment X (implied mode)
    fn inx(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        self.cpu.reg.x = self.cpu.reg.x.wrapping_add(1);
        self.cpu.flags.zero = (self.cpu.reg.x & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.x & 0x80) != 0;
        mode
    }
    fn iny(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> {
        self.cpu.reg.y = self.cpu.reg.y.wrapping_add(1);
        self.cpu.flags.zero = (self.cpu.reg.y & 0xff) == 0;
        self.cpu.flags.negative = (self.cpu.reg.y & 0x80) != 0;
        mode
    }
    // Jump to Subroutine
    fn jsr(&mut self, mode: AddressMode<u16>) -> AddressMode<u16>  {

        // Push to stack
        let pc = self.cpu.reg.pc;
        self.push_word(pc - 1);
        debug!("{:02x}", mode);
        self.cpu.reg.pc = mode.address;
        mode
    }
    fn jmp(&mut self, mode: AddressMode <u16>) -> AddressMode<u16> {
        // debug!("pc:{:x}, mode.address{:x}", self.cpu.reg.pc, mode.address);
        self.cpu.reg.pc = mode.address;
        mode
    }

    fn sec(&mut self, mode: AddressMode<u16>) -> AddressMode<u16> { self.cpu.flags.carry = true; mode }
    fn sei(&mut self, mode:AddressMode<u16>) -> AddressMode<u16> { self.cpu.flags.interrupt = true; mode }

    // ISC (Increase memory by one) UNOFFICIAL OPCODE
    fn isc(&mut self, value: AddressMode <u16>) -> AddressMode<u16> {
        let addr = self.read16(value.data).wrapping_add(self.cpu.reg.x as u16);
        if (value.data - self.cpu.reg.x as u16) & 0xff00 != value.data & 0xff00 { self.adv_cycles(1); }
        let result = (self.cpu.reg.a as u16).wrapping_sub(addr as u16).wrapping_sub(self.cpu.flags.carry as u16);
        self.cpu.flags.zero = (result & 0xff) == 0;
        self.cpu.flags.negative = (result & 0x80) != 0;
        self.cpu.reg.a = result as u8;
        value
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
