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
            0x02 => "KIL            ",
            0x03 => "SLO Indirect X ",
            0x04 => "RTI            ",
            0x05 => "ORA Zero Page  ",
            0x06 => "ASL Zero Page  ",
            0x08 => "PHP            ",
            0x09 => "ORA Immediate  ",
            0x07 => "SLO Zero Page  ",
            0x0a => "ASL Accumulator",
            0x0c => "NOP            ",
            0x0d => "ORA Absolute   ",
            0x10 => "BPL            ",
            0x13 => "SLO Indirect Y ",
            0x15 => "ORA Zero Page X",
            0x16 => "ASL Zero Page X",
            0x17 => "SLO Zero Page X",
            0x18 => "CLC            ",
            0x1a => "ROL            ",
            0x1e => "ASL Absolute X ",
            0x1f => "SLO Absolute X ",
            0x1b => "SLO Absolute Y ",
            0x1c => "NOP            ",
            0x0e => "ASL Absolute   ",
            0x0f => "SLO Absolute   ",
            0xa0 => "LDY IMM        ",
            0xaa => "TAX            ",
            0xa1 => "LDA Indirect X ",
            0xa2 => "LDX IMM        ",
            0xa4 => "LDY Zero Page  ",
            0xa5 => "LDA Zero Page  ",
            0xa6 => "LDX Zero Page  ",
            0xa8 => "TAY            ",
            0xac => "LDY Absolute   ",
            0xad => "LDA Absolute   ",
            0xae => "LDX Absolute   ",
            0xaf => "LAX Absolute   ",
            0xa9 => "LDA IMM        ",
            0xb0 => "BCS            ",
            0xb1 => "LDA Indirect Y ",
            0xb5 => "LDA Zero Page X",
            0xb6 => "LDX Zero Page X",
            0xb8 => "CLV            ",
            0xb9 => "LDA Absolute Y ",
            0xba => "TSX            ",
            0xbd => "LDA Absolute X ",
            0xbe => "LDX Absolute Y ",
            0x40 => "RTI            ",
            0x46 => "LSR Zero Page  ",
            0x4a => "LSR A          ",
            0x4e => "LSR            ",
            0x48 => "PHA            ",
            0x50 => "BVC            ",
            0x5d => "EOR Absolute X ",
            0x6a => "ROR A          ",
            0x6d => "ADC Absolute   ",
            0x64 => "DOP            ",
            0x66 => "ROR Zero Page  ",
            0x69 => "ADC Immediate  ",
            0x20 => "JSR Absolute   ",
            0x21 => "AND Indirect X ",
            0x24 => "BIT Zero Page  ",
            0x28 => "PLP            ",
            0x29 => "AND A8         ",
            0x2c => "BIT Absolute   ",
            0x30 => "BMI            ",
            0x35 => "AND Zero Page X",
            0x36 => "ROL Zero Page X",
            0x38 => "SEC            ",
            0x2d => "AND D16        ",
            0x41 => "EOR Indirect X ",
            0x45 => "EOR Zero Page  ",
            0x49 => "EOR Immediate  ",
            0x4c => "JMP            ",
            0x4d => "EOR Absolute   ",
            0x50 => "BVS            ",
            0x51 => "EOR Indirect Y ",
            0x54 => "IGN (NOP)      ",
            0x55 => "EOR Zero Page X",
            0x59 => "EOR Absolute Y ",
            0x5a => "NOP            ",
            0x60 => "RTS            ",
            0x61 => "ADC            ",
            0x65 => "ADC            ",
            0x68 => "PLA            ",
            0x6c => "JMP            ",
            0x6e => "ROR Absolute   ",
            0x70 => "BVS            ",
            0x72 => "NOP            ",
            0x73 => "NOP            ",
            0x78 => "SEI            ",
            0x79 => "ADC Indirect Y ",
            0x7e => "ROR Absolute X ",
            0x84 => "STY Zero Page  ",
            0x85 => "STA Zero Page  ",
            0x86 => "STX Zero Page  ",
            0x88 => "DEY            ",
            0x8a => "TXA            ",
            0x8c => "STY Absolute   ",
            0x8d => "STA Absolute   ",
            0x8e => "STX Absolute   ",
            0x90 => "BCC            ",
            0x91 => "STA Indirect Y ",
            0x95 => "STA Zero Page X",
            0x96 => "STX Zero Page Y",
            0x98 => "TYA            ",
            0x99 => "STA Indirect Y ",
            0x9a => "TXS            ",
            0x9d => "STA Absolute X ",
            0xc0 => "CPY Immediate  ",
            0xc1 => "CMP Indirect Y ",
            0xc4 => "CPY Zero Page  ",
            0xc5 => "CMP Zero Page  ",
            0xcc => "CPY Absolute   ",
            0xc3 => "DCP            ",
            0xc6 => "DEC Zero Page  ",
            0xd0 => "BNE            ",
            0xd2 => "HLT            ",
            0xd3 => "DCP            ",
            0xd8 => "CLD            ",
            0xd9 => "CMP Absolute Y ",
            0xdf => "DCP            ",
            0xe0 => "CPX Immediate  ",
            0xe4 => "CPX Zero Page  ",
            0xe5 => "SBC Zero Page  ",
            0xe9 => "SBC Immediate  ",
            0xec => "CPX Absolute   ",
            0xea => "NOP            ",
            0xe1 => "SBC Indirect X ",
            0xe6 => "INC Zero Page  ",
            0xe8 => "INX            ",
            0xc8 => "INY            ",
            0xc9 => "CMP Immediate  ",
            0xca => "DEX            ",
            0xce => "DEC Absolute   ",
            0xf0 => "BEQ            ",
            0xf1 => "SBC Indirect Y ",
            0xf5 => "SBC Zero Page X",
            0xf6 => "INC Zero Page  ",
            0xf7 => "ISC Zero Page X",
            0xf8 => "SED            ",
            0xf9 => "SBC Absolute Y ",
            0xfd => "SBC Absolute X ",
            0xfe => "INC Absolute X ",
            0xff => "ISC Absolute X ",
            _    => "Opcode not in print table",
        }
    }
}


