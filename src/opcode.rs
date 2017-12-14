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
            0x00 => "BRK",
            0x01 => "ORA (a8, X)",
            0x02 => "KIL",
            0x03 => "SLO",
            0x78 => "SEI",
            _ => "Unknown opcode",
        }



    }
    pub fn decode(opcode: u8) -> Instruction {
        match opcode {
            0x00 => {
                Instruction {
                    opcode,
                    mnemonic: "BRK",
                    bytes: 2,
                    cycles: (7, 0),
                    flags: "interrupt"
                }
            },
            0x01 => {
                Instruction {
                    opcode,
                    mnemonic: "ORA (a8, X)",
                    bytes: 2,
                    cycles: (7, 0),
                    flags: "negative, zero"
                }
            },
            0x02 => {
                Instruction {
                    opcode,
                    mnemonic: "KIL",
                    bytes: 1,
                    cycles: (7, 0),
                    flags: ""
                }
            },
            0x03 => {
                Instruction {
                    opcode,
                    mnemonic: "SLO",
                    bytes: 2,
                    cycles: (8, 0),
                    flags: "interrupt"
                }
            },
            0x78 => {
                Instruction {
                    opcode,
                    mnemonic: "SEI",
                    bytes: 1,
                    cycles: (2, 0),
                    flags: "interrupt"
                }
            }            _ => {
                Instruction {
                    opcode: 0,
                    mnemonic: "Unknown",
                    bytes: 0,
                    cycles: (0, 0),
                    flags: ""
                }
            }
        }
    }
}


