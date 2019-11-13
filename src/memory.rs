use std::ops::{Index, IndexMut};
use std::fmt::{UpperHex, LowerHex};
use std::fmt::{Debug, Formatter, Result};
use crate::cartridge::{Cartridge, RomHeader};


pub struct Ram {
    pub memory: Box<[u8; 0x800]>,
    pub sram: Box<[u8; 0x8000]>
}

impl Index<u16> for Ram {
    type Output = u8;
    fn index(&self, index:u16) -> &u8 {
        &self.memory[index as usize]
    }
}
impl IndexMut<u16> for Ram {
    fn index_mut(&mut self, index:u16) -> &mut u8 {
        &mut self.memory[index as usize]
    }
}
impl LowerHex for Ram {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let value = self;
        write!(f, "{:04x}", value)
    }
}
impl Debug for Ram {
    fn fmt(&self, f: &mut Formatter) -> Result {
    let value = self;
        write!(f, "{:?}", value)
    }
}

impl Ram {
    pub fn new() -> Self {
        Ram {
            memory: Box::new([0; 0x0800]),
            sram: Box::new([0; 0x08000]),
        }
    }
}

