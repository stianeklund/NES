use std::ops::{Index, IndexMut};
use std::fmt::{UpperHex, LowerHex};
use std::fmt::{Debug, Formatter, Result};
use std::path::Path;
use std::fs::File;
use std::io::Read;
use rom::{Cartridge, RomHeader};


pub struct Ram {
    pub memory: Box<[u8; 0x800]>
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
    pub fn new() -> Ram {
        Ram {
            memory: Box::new([0; 0x0800]),
        }
    }
}

// TODO Implement remaining mappers
// Memory Map https://wiki.nesdev.com/w/index.php/CPU_memory_map
// We need a way to split up memory in chunks for easy access from different areas.
// I.e PPU wants to access $2000 - 2007. All of this needs to be created in `Interconnect` however
// it can be structured & setup here.
pub trait Mapper {
    fn read_byte(&mut self, addr: u16) -> u8;
    fn write_byte(&mut self, addr: u16, byte: u8);
    fn read_word(&mut self, addr: u16) -> u16;
    fn write_word(&mut self, addr: u16, word: u16);
}

