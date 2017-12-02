use std::fmt;

pub struct Opcode {
    pub cycle_lenght: u8,
    pub mnemonic: String,
    pub opcode: u8,
}
impl fmt::LowerHex for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = self;
        write!(f, "{:04x}", value)
    }
}
impl fmt::Debug for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = self;
        write!(f, "{:?}", value)
    }
}
impl Opcode {
   pub fn new() -> Opcode {
       Opcode {
           cycle_lenght: 0,
           mnemonic: String::new(),
           opcode: 0,
       }
   }
}