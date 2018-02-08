use std::fmt;
use cpu::{StatusRegister};

pub struct Instruction {
    pub opcode: u8,
    pub mnemonic: &'static str,
    pub bytes: u8,
    pub cycles: (u8, u8),
    pub flags: &'static str
}


impl fmt::UpperHex for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let val = self;
        write!(f, "{:X}", val)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd)]
pub enum Register {
    A,
    X,
    Y,
}

impl fmt::UpperHex for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let val = self;
        write!(f, "{:X}", val)
    }
}
impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:04x} {:02x} {} {:?} {}",
        self.opcode, self.bytes, self.mnemonic, self.cycles, self.flags)
    }
}

impl Instruction {
    pub fn default() -> Self {
        Instruction {
            opcode: 0,
            bytes: 0,
            mnemonic: "",
            cycles: (0,0),
            flags: "",
        }
    }

    // Returns the mnemonic of a instruction
    pub fn mnemonic(opcode: u8) -> &'static str {
        match opcode {
            0x00 => "BRK            ",
            0x01 => "BPL            ",
            0x02 => "ORA (a8, X)    ",
            0x03 => "SLO Indirect X ",
            0x04 => "RTI            ",
            0x06 => "ASL            ",
            0x07 => "SLO Zero Page  ",
            0x13 => "SLO Indirect Y ",
            0x17 => "SLO Zero Page X",
            0x0f => "SLO Absolute   ",
            0xa0 => "LDY IMM        ",
            0xa1 => "LDA Indirect X ",
            0xa2 => "LDA IMM        ",
            0xa5 => "LDA Zero Page  ",
            0xad => "LDA Absolute   ",
            0xa9 => "LDA IMM        ",
            0xb1 => "LDA Indirect Y ",
            0xb5 => "LDA Zero Page X",
            0xb8 => "CLV            ",
            0xb9 => "LDA Absolute Y ",
            0xbd => "LDA Absolute X ",
            0x40 => "RTI            ",
            0x4e => "LSR            ",
            0x48 => "PHA            ",
            0x1a => "ROL            ",
            0x1f => "SLO Absolute X ",
            0x1b => "SLO Absolute Y ",
            0x20 => "JSR            ",
            0x24 => "BIT Zero Page  ",
            0x29 => "AND A8         ",
            0x2d => "AND D16        ",
            0x4c => "JMP            ",
            0x5a => "NOP            ",
            0x60 => "RTS            ",
            0x61 => "ADC            ",
            0x65 => "ADC            ",
            0x68 => "PLA            ",
            0x6c => "JMP            ",
            0x6e => "LSR            ",
            0x70 => "BVS            ",
            0x72 => "NOP            ",
            0x73 => "NOP            ",
            0x78 => "SEI            ",
            0x84 => "STY Zero Page  ",
            0x85 => "STA Zero Page  ",
            0x86 => "STX Zero Page  ",
            0x8d => "STA ABS        ",
            0x8e => "STX ABS        ",
            0x91 => "STA Indirect Y ",
            0x9a => "TXS            ",
            0x9d => "STA Absolute X ",
            0xc3 => "DCP            ",
            0xd0 => "BNE            ",
            0xd3 => "DCP            ",
            0xd8 => "CLD            ",
            0xdf => "DCP            ",
            0xe6 => "INC Zero Page  ",
            0xe8 => "INX            ",
            0xc8 => "INY            ",
            0xf0 => "BEQ            ",
            0xf6 => "INC Zero Page  ",
            0xf7 => "ISC Zero Page X",
            0xff => "ISC Absolute X ",
            _    => "Illegal opcode ",
        }
    }
}


