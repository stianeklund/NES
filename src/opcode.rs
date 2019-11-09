use std::fmt;
use crate::cpu::{StatusRegister};

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
    pub fn default() ->     Self {
        Instruction {
            opcode: 0,
            bytes: 0,
            mnemonic: "",
            cycles: (0,0),
            flags: "",
        }
    }
    pub fn short_mnemonic(opcode: u8) -> &'static str {
        match opcode {
            0x00 => "BRK",
            0x01 => "ORA",
            0x02 => "KIL",
            0x03 => "SLO",
            0x04 => "RTI",
            0x05 => "ORA",
            0x06 => "ASL",
            0x08 => "PHP",
            0x09 => "ORA",
            0x07 => "SLO",
            0x0a => "ASL",
            0x0c => "NOP",
            0x0d => "ORA",
            0x10 => "BPL",
            0x11 => "ORA",
            0x13 => "SLO",
            0x15 => "ORA",
            0x16 => "ASL",
            0x17 => "SLO",
            0x18 => "CLC",
            0x1a => "ROL",
            0x1e => "ASL",
            0x1f => "SLO",
            0x1b => "SLO",
            0x1c => "NOP",
            0x0e => "ASL",
            0x0f => "SLO",
            0xa0 => "LDY",
            0xaa => "TAX",
            0xa1 => "LDA",
            0xa2 => "LDX",
            0xa4 => "LDY",
            0xa5 => "LDA",
            0xa6 => "LDX",
            0xa8 => "TAY",
            0xac => "LDY",
            0xad => "LDA",
            0xae => "LDX",
            0xaf => "LAX",
            0xa9 => "LDA",
            0xb0 => "BCS",
            0xb1 => "LDA",
            0xb5 => "LDA",
            0xb6 => "LDX",
            0xb8 => "CLV",
            0xb9 => "LDA",
            0xba => "TSX",
            0xbd => "LDA",
            0xbe => "LDX",
            0x40 => "RTI",
            0x46 => "LSR",
            0x4a => "LSR",
            0x4e => "LSR",
            0x48 => "PHA",
            0x50 => "BVC",
            0x5d => "EOR",
            0x6a => "ROR",
            0x6d => "ADC",
            0x64 => "DOP",
            0x66 => "ROR",
            0x69 => "ADC",
            0x20 => "JSR",
            0x21 => "AND",
            0x24 => "BIT",
            0x25 => "AND",
            0x26 => "ROL",
            0x28 => "PLP",
            0x29 => "AND",
            0x2a => "ROL",
            0x2c => "BIT",
            0x2e => "ROL",
            0x30 => "BMI",
            0x31 => "AND",
            0x35 => "AND",
            0x36 => "ROL",
            0x38 => "SEC",
            0x2d => "AND",
            0x41 => "EOR",
            0x45 => "EOR",
            0x49 => "EOR",
            0x4c => "JMP",
            0x4d => "EOR",
            0x51 => "EOR",
            0x54 => "IGN",
            0x55 => "EOR",
            0x59 => "EOR",
            0x5a => "NOP",
            0x60 => "RTS",
            0x61 => "ADC",
            0x65 => "ADC",
            0x68 => "PLA",
            0x6c => "JMP",
            0x6e => "ROR",
            0x70 => "BVS",
            0x72 => "NOP",
            0x73 => "NOP",
            0x78 => "SEI",
            0x79 => "ADC",
            0x7e => "ROR",
            0x81 => "STA",
            0x84 => "STY",
            0x85 => "STA",
            0x86 => "STX",
            0x88 => "DEY",
            0x8a => "TXA",
            0x8c => "STY",
            0x8d => "STA",
            0x8e => "STX",
            0x90 => "BCC",
            0x91 => "STA",
            0x95 => "STA",
            0x96 => "STX",
            0x98 => "TYA",
            0x99 => "STA",
            0x9a => "TXS",
            0x9d => "STA",
            0xcd => "CMP",
            0xc0 => "CPY",
            0xc1 => "CMP",
            0xc4 => "CPY",
            0xc5 => "CMP",
            0xcc => "CPY",
            0xc3 => "DCP",
            0xc6 => "DEC",
            0xd0 => "BNE",
            0xd2 => "HLT",
            0xd3 => "DCP",
            0xd6 => "DEC",
            0xd8 => "CLD",
            0xd9 => "CMP",
            0xdf => "DCP",
            0xee => "ROL",
            0xe0 => "CPX",
            0xe4 => "CPX",
            0xe5 => "SBC",
            0xe9 => "SBC",
            0xec => "CPX",
            0xea => "NOP",
            0xed => "SBC",
            0xe1 => "SBC",
            0xe6 => "INC",
            0xe8 => "INX",
            0xc8 => "INY",
            0xc9 => "CMP",
            0xca => "DEX",
            0xce => "DEC",
            0xf0 => "BEQ",
            0xf1 => "SBC",
            0xf5 => "SBC",
            0xf6 => "INC",
            0xf7 => "ISC",
            0xf8 => "SED",
            0xf9 => "SBC",
            0xfd => "SBC",
            0xfe => "INC",
            0xff => "ISC",
        _    => "Opcode not in print table, or invalid opcode",
        }
    }
}


