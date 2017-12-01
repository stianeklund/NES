use std::ops::{Index, IndexMut};
use std::fmt::{UpperHex, LowerHex};
use std::fmt::{Debug, Formatter, Result};
use std::path::Path;
use std::fs::File;
use std::io::Read;


pub trait Memory {
    fn high_nibble(byte: u8) -> u8;
    fn low_nibble(byte: u8) -> u8;
    fn read_byte(&mut self, addr: u16) -> u8;
    fn read_word(&mut self, addr: u16) -> u16;
    fn write_byte(&mut self, addr: u16, value: u8);
    fn write_word(&mut self, addr: u16, value: u16);

}
// 2KB of on-board work RAM
pub struct Ram {
    pub memory: Vec<u8>
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
impl Memory for Ram {
    fn high_nibble(byte: u8) -> u8 {
        byte & 0x000F
    }

    fn low_nibble(byte: u8) -> u8 {
        (byte & 0x00F0) >> 4
    }

    fn read_byte(&mut self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }
    fn read_word(&mut self, addr:u16) -> u16 {
        (self.read_byte(addr + 1) as u16) << 8 | self.read_byte(addr) as u16

    }

    fn write_byte(&mut self, addr: u16, byte: u8) {
        self.memory[addr as usize] = byte
    }
    fn write_word(&mut self, addr: u16, word: u16) {
        self.write_byte(addr, word as u8);
        self.write_byte((addr + 1), (word >> 8) as u8);
    }
}
impl Ram {
    pub fn new() -> Ram {
        Ram {
            memory: vec![0; 0x800],
        }
    }

}

