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
// TODO Implement Addressing modes for opcodes
#[derive(Debug)]
enum AddressMode {
    ZeroPage,  // Zero Page addressing, $00nn
    ZeroPageX, // $00nn + X
    ZeroPageY, // $00nn + y
    Immediate, // Immediate addressing; immediately following the opcode.
    Absolute,  // Absolute addressing. Fetches the next two memory slots & combines them into a word.
    AbsoluteX, // X indexed. Fetches the next two memory slots & combines them into a word, then adds X.
    AbsoluteY, // Y indexed. Fetches the next two memory slots & combines them into a word, then adds Y.
    Indirect,  // Indirect addressing; special for JMP
    IndirectX, // X indexed addressing.
    IndirectY, // Y indexed addressing.
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
            0xa0 => self.ldy(),
            0xa1 => self.lda_izx(),
            0xa2 => self.lda(AddressMode::AbsoluteX),
            0xad => self.lda_a16(),
            0xa9 => self.lda_a8(),
            0xb8 => self.clv(),
            0x40 => self.rti(),
            0x4e => self.lsr(),
            0x48 => self.pha(),
            0x1a => self.rol(),
            0x20 => self.jsr(),
            0x24 => self.bit(AddressMode::ZeroPage),
            0x29 => self.and_d8(),
            0x2d => self.and_d16(),
            0x4c => self.jmp(),
            0x5a => self.nop(),
            0x60 => self.rts(),
            0x61 => self.adc(),
            0x65 => self.adc(),
            0x6c => self.jmp(),
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
            0xc3 => self.dcp(),
            0xd0 => self.bne(),
            0xd3 => self.dcp(),
            0xd8 => self.cld(),
            0xdf => self.dcp(),
            0xe6 => self.inc(AddressMode::ZeroPage),
            0xe8 => self.inx(),
            0xc8 => self.iny(),
            0xf0 => self.beq(),
            0xf6 => self.inc(AddressMode::ZeroPage),
            0xff => self.isc(AddressMode::AbsoluteX),
            _ => eprintln!("Not implemented"),
        }
        self.cpu.opcode = opcode as u8;
        self.cpu.reg.prev_pc = self.cpu.reg.pc;
        // println!("Decoded instruction: {:?}", Instruction::decode(opcode));

        println!("{:?}", self.cpu);
    }
    fn adc(&mut self) {
        println!("ADC");
        self.adv_pc(2);
        let mem_value = self.ram.memory[self.cpu.reg.pc as usize];
        let result = (self.cpu.reg.a as u16).wrapping_add(mem_value as u16).wrapping_add(self.cpu.flags.carry as u16);
        // TODO Flags
        self.cpu.flags.carry = (result & 0x0100) != 0;
        self.adv_cycles(3)
    }
    // Arithmetic shift left zero page
    fn asl(&mut self) {
        println!("ASL");
        let carry = (self.cpu.reg.a & 1) != 0;
        let value = self.ram.memory[self.cpu.reg.pc as usize];
        // Check if 7th bit has been set
        // Shift left by one
        let mut result = value << 1;

        self.cpu.flags.carry = value & 0x80 != 0;

        self.cpu.flags.negative;
        self.cpu.flags.zero;
        self.cpu.flags.carry = carry;
        self.adv_pc(2);
        self.adv_cycles(6);
    }
    // TODO Use addressing modes
    fn and_d16(&mut self) {
        println!("AND D16");
        let d16 = self.read_word(self.cpu.reg.pc + 1);
        self.cpu.reg.a = ((self.cpu.reg.a as u16) & d16) as u8;
        self.adv_pc(2);
        self.adv_cycles(2);

        self.cpu.flags.negative = self.cpu.reg.a & 0x80 != 0;
        self.cpu.flags.zero = self.cpu.reg.a & 0xff == 0;
    }
    // AND with immediate data
    fn and_d8(&mut self) {
        println!("AND D8");
        let d8 = self.read(self.cpu.reg.pc + 1);
        self.cpu.reg.a &= d8;
        self.adv_pc(2);
        self.adv_cycles(2);

        self.cpu.flags.negative = self.cpu.reg.a & 0x80 != 0;
        self.cpu.flags.zero = self.cpu.reg.a & 0xff == 0;
    }

    // Branch on Equal
    fn beq(&mut self) {
        println!("BEQ");
        if self.read(self.cpu.reg.pc + 1) == 0xf9 {
            if self.cpu.flags.zero {
                self.cpu.reg.pc -= 7;
                self.adv_cycles(1);
            }
        }
        self.adv_cycles(2);
        self.adv_pc(2);
    }
    // Branch on Plus
    fn bpl(&mut self) {
        println!("BPL");
        // Cycles 3+ / 2
        self.adv_pc(3);
        self.adv_cycles(3);
    }

    fn brk(&mut self) {
        println!("BRK");
        self.cpu.flags.brk = true;
        let pc = self.cpu.reg.pc;
        self.push_word(pc + 1);
        self.adv_pc(2);
        self.cpu.reg.pc = self.read_word(0xfffe);
        self.adv_cycles(7);
    }
    // Test Bits N Z V
    fn bit(&mut self, mode: AddressMode) {
        println!("BIT");
        match mode {
            AddressMode::ZeroPage => {
                // BIT $44 HEX: $24, LEN: 2, TIME: 3
                let value = self.read(self.cpu.reg.pc + 1);
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
        println!("BNE");
        if !self.cpu.flags.zero {
            self.adv_pc(3);
            self.adv_cycles(3);
        } else {
        self.adv_cycles(2)}
        self.adv_pc(1);
    }


    // Branch on overflow set
    fn bvs(&mut self) {
        println!("BVS");
        if self.cpu.flags.overflow {
            self.adv_pc(2);
            self.adv_cycles(3);
        } else {
            self.adv_cycles(2);
            self.adv_pc(1);
        }
    }
    // Clear decimal
    fn cld(&mut self) {
        println!("CLD");
        self.cpu.flags.decimal = false;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // Clear overflow
    fn clv(&mut self) {
        println!("CLV");
        self.cpu.flags.overflow = false;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // Decrement & compare
    fn dcp(&mut self) {
        println!("DCP (illegal opcode)");
        let data = self.cart.prg[self.cpu.reg.pc as usize + 2];
        println!("Data:{:04x}", data);
        self.adv_pc(2);
        self.adv_cycles(7);
    }
    // LDA A8
    fn lda_a8(&mut self) {
        println!("LDA A8");
        let d8 = self.read(self.cpu.reg.pc + 1);
        self.cpu.reg.a = d8 as u8;
        self.cpu.flags.zero = d8 & 0xff == 0;
        self.cpu.flags.negative = d8 & 0x80 != 0;
        self.adv_pc(2);
        self.adv_cycles(2)
    }
    fn lda_a16(&mut self) {
        println!("LDA A16");
        let d16 = self.read_word(self.cpu.reg.pc + 1);
        // println!("d16:{:04x}", d16);
        self.cpu.reg.a = d16 as u8;
        self.cpu.flags.zero = d16 & 0xff == 0;
        self.cpu.flags.negative = d16 & 0x80 != 0;
        self.adv_pc(3);
        self.adv_cycles(4);
    }
    fn lda_izx(&mut self) {
        println!("LDA A8 X IZX");
        let d8 = self.read(self.cpu.reg.pc + 1);
        self.cpu.reg.x = d8 as u8;
        self.cpu.flags.zero = d8 & 0xff == 0;
        self.cpu.flags.negative = d8 & 0x80 != 0;
        self.adv_pc(2);
        self.adv_cycles(3)
    }
    // LDA (A8, X)
    fn lda(&mut self, mode: AddressMode, ) {
        // Immediate?
        let mut data: u16 = 0;
        match mode {
            AddressMode::Absolute => {
                data = self.read_word(self.cpu.reg.pc + 2);
                self.adv_cycles(4);
            },
            AddressMode::AbsoluteX => {
                println!("LDA (A8, X)");
                data = self.read_word(self.cpu.reg.pc + 1);
                // TODO Cycles is 5 if page boundry is crossed
                self.adv_cycles(4);
                self.cpu.reg.x = data as u8;
            },
            _ => eprintln!("Not included"),
        }
        // let a8 = self.read(self.cpu.reg.pc + 1);
        // self.cpu.reg.x = a8;
        self.cpu.flags.zero = data & 0xff == 0;
        self.cpu.flags.negative = data & 0x80 != 0;

        self.adv_pc(2);
        self.adv_cycles(6);
    }
    // LDA (A8, Y)
    fn lday(&mut self) {
        println!("LDA (A8, Y)");
        let a8 = self.read(self.cpu.reg.pc + 2);
        self.cpu.reg.y = a8;
        self.cpu.flags.zero = a8 & 0xff == 0;
        self.cpu.flags.negative = a8 & 0x80 != 0;
        self.adv_cycles(4);
        self.adv_pc(3);
    }
    // LDY Load Y Register (d8)
    fn ldy(&mut self) {
        // TODO Bitmasks
        println!("LDY D8");
        let d8 = self.read(self.cpu.reg.pc + 1);
        self.cpu.reg.y = d8;
        self.cpu.flags.zero = d8 & 0xff == 0;
        self.cpu.flags.negative = d8 & 0x80 != 0;
        self.adv_cycles(2);
        self.adv_pc(2);
    }
    // Load X Register
    fn ldax(&mut self) {
        println!("LDX D8");
        let d8 = self.read(self.cpu.reg.pc + 1);
        self.cpu.reg.x = d8;
        self.cpu.flags.zero = d8 & 0xff == 0;
        self.cpu.flags.negative = d8 & 0x80 != 0;
        self.adv_pc(2);
        self.adv_cycles(3);
    }
    // Logical Shift Right
    fn lsr(&mut self) {
        println!("LSR");
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

    fn rol(&mut self) {
        println!("ROL");
        self.adv_pc(2);
        self.adv_cycles(5);
    }
    fn rts(&mut self) {
        println!("RTS");
        self.adv_cycles(6);
    }
    fn rti(&mut self) {
        println!("RTI");
        // TODO store flags & pop pc
        self.adv_pc(1);
        self.adv_cycles(6);
    }
    fn sta(&mut self, mode: AddressMode) {
        // Ex: Absolute: STA $4400 Hex: $8D Len: 3 Cycles:4
        match mode {
            AddressMode::Absolute => {
                println!("STA ABS");
                let addr = self.read_word(self.cpu.reg.pc + 1);
                let a = self.cpu.reg.a;
                // Write value of accumulator to memory address
                self.write(addr, a);
                self.adv_cycles(4);
                self.adv_pc(3);
            },
            AddressMode::IndirectY => {
                println!("STA Indirect Y");
                let value = self.cpu.reg.pc + 1;
                let y = self.cpu.reg.y;
                let addr = self.read_word(value) + y as u16;
                self.write(addr, y);
                self.adv_cycles(4);
                self.adv_pc(2);
            },
            AddressMode::ZeroPage=> {
                println!("STA Zero Page");
                eprintln!("Zero page addressing is not implemented");
                let addr = self.read_word(self.cpu.reg.pc + 1);
                let a = self.cpu.reg.a;
                // Write value of accumulator to memory address
                self.write(addr, a);
                self.adv_cycles(4);
                self.adv_pc(3);
            }
            _ => eprintln!("{:?} not covered", mode),
        }
    }
    fn sty(&mut self, mode: AddressMode) {
        // Ex: Absolute: STA $4400 Hex: $8D Len: 3 Cycles:4
        match mode {

            AddressMode::Absolute => {
                println!("STY ABS");
                let addr = self.read_word(self.cpu.reg.pc + 1);
                let y = self.cpu.reg.y;
                self.write(addr, y);
                self.adv_pc(2);
                self.adv_cycles(3);
            },
            AddressMode::ZeroPage=> {
                println!("STY ZeroPage");
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
                println!("STX ABS");
                let addr = self.read_word(self.cpu.reg.pc + 1);
                let x = self.cpu.reg.x;
                // Write value of accumulator to memory address
                self.write(addr, x);
                self.adv_cycles(4);
                self.adv_pc(3);
            }
            AddressMode::ZeroPage => {
                println!("STX ZeroPage");
                let addr = self.read_word(self.cpu.reg.pc + 1);
                let x = self.cpu.reg.x;
                // Write value of accumulator to memory address
                self.write(addr, x);
                self.adv_cycles(3);
                self.adv_pc(2);
            }
            _ => eprintln!("{:?} not covered", mode),
        }
    }

    // Transfer X to Stack Pointer
    fn txs(&mut self) {
        println!("TXS");
        // TODO TXS is like POP?
        let x = self.cpu.reg.x;
        self.push_byte(x);
        self.adv_pc(1);
        self.adv_cycles(2);
    }

    fn push_stack(&mut self, value: u16) {
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
    fn push_word(&mut self, value: u16) {
        let sp = self.cpu.reg.sp;
        self.write_word(0x100 + (sp.wrapping_sub(1)) as u16, value);
        self.cpu.reg.sp = self.cpu.reg.sp.wrapping_sub(2);
    }
    // Push accumulator
    fn pha(&mut self) {
        println!("PHA");
        let a = self.cpu.reg.a;
        self.push_byte(a);
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // Increment Memory
    fn inc(&mut self, mode: AddressMode) {
        println!("INC");
        match mode {
            AddressMode::ZeroPage => {
                let value = self.read(self.cpu.reg.pc + 1) as u16;
                self.cpu.flags.zero = value & 0xff == 0;
                self.cpu.flags.negative = value & 0x80 != 0;
                self.write(value, 0xe6);
                self.adv_cycles(5);
                self.adv_pc(2);
            }
            _ => eprintln!("{:?} not covered", mode)
        }

    }
    // Increment X (implied mode)
    fn inx(&mut self) {
        println!("INX");
        self.cpu.reg.x = self.cpu.reg.x.wrapping_add(1);
        self.adv_cycles(2);
        self.cpu.flags.zero = self.cpu.reg.x & 0xff == 0;
        self.cpu.flags.negative = self.cpu.reg.x & 0x80 != 0;
        self.adv_pc(1);

    }
    fn iny(&mut self) {
        println!("INY");
        self.cpu.reg.y = self.cpu.reg.y.wrapping_add(1);
        self.adv_cycles(2);
        self.cpu.flags.zero = self.cpu.reg.y & 0xff == 0;
        self.cpu.flags.negative = self.cpu.reg.y & 0x80 != 0;
        self.adv_pc(1);

    }
    // Jump to Subroutine
    fn jsr(&mut self) {
        // Get value at word in PC & advance pc by 2
        let addr = self.read_word(self.cpu.reg.pc + 1);
        println!("JSR");

        self.adv_pc(3);

        // Push to stack
        let pc = self.cpu.reg.pc;
        self.push_stack(pc - 1);
        self.adv_cycles(3); // 6 if no jump?
        self.cpu.reg.pc = addr;
    }
    fn jmp(&mut self) {
        println!("JMP");
        let pc = self.cpu.reg.pc + 1;
        let addr = self.read_word(pc);
        println!("Destination: {:04x}", addr);
        self.cpu.reg.pc = addr;
        self.adv_cycles(3);
    }
    fn sei(&mut self) {
        println!("SEI");
        self.cpu.flags.interrupt = true;
        self.adv_pc(1);
        self.adv_cycles(2);
    }
    // ISC (Increase memory by one)
    fn isc(&mut self, mode: AddressMode) {
        println!("ISC {:?}", mode);
        match mode {
            AddressMode::AbsoluteX => {
                let value = self.cpu.reg.pc.wrapping_add(1);
                let addr = self.read_word(value);
                println!("Value:{:04x}, Word:{:04x}", value, addr);
                let result = (self.cpu.reg.a as u16).wrapping_sub(addr as u16).wrapping_sub(self.cpu.flags.carry as u16);
                self.cpu.flags.zero = result & 0xff == 0;
                self.cpu.flags.negative = result & 0x80 != 0;
                // self.cpu.reg.a -= addr;
                self.cpu.reg.a = result as u8;

                self.adv_pc(3);
                self.adv_cycles(5);
            }
            _ => eprintln!("Unknown address mode"),
        }
    }
}
