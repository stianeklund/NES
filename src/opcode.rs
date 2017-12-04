use std::fmt;
use cpu::{StatusRegister};

pub struct Instruction {
    pub opcode: u8,
    pub mnemonic: &'static str,
    pub bytes: u8,
    pub cycles: (u8, u8),
    pub flags: [u8; 6],
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
        write!(f, "{:04x} {:02x} {:?} {:?} {:?}",
        self.opcode, self.bytes, self.mnemonic, self.cycles, &self.flags[0..6])
    }
}

impl Instruction {
    pub fn default() -> Self {
        Instruction {
            opcode: 0,
            bytes: 0,
            mnemonic: "",
            cycles: (0,0),
            flags: [0; 6],
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
                    flags: [0,0,0,1,0,0],
                }
            },
            0x01 => {
                Instruction {
                    opcode,
                    mnemonic: "ORA (a8, X)",
                    bytes: 2,
                    cycles: (7, 0),
                    flags: [0,0,0,1,0,0],
                }
            },
            0x02 => {
                Instruction {
                    opcode,
                    mnemonic: "KIL",
                    bytes: 1,
                    cycles: (7, 0),
                    flags: [0,0,0,0,0,0],
                }
            },
            0x03 => {
                Instruction {
                    opcode,
                    mnemonic: "SLO",
                    bytes: 2,
                    cycles: (7, 0),
                    flags: [0,0,0,1,0,0],
                }
            }            _ => {
                Instruction {
                    opcode: 0,
                    mnemonic: "Unknown",
                    bytes: 0,
                    cycles: (0, 0),
                    flags: [0,0,0,0,0,0],
                }
            }
        }
    }
}


