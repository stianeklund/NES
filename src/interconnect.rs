use super::rom::Cartridge;
use super::cpu::{ExecutionContext, Cpu, Registers};
use super::memory::{Ram, Mapper};

pub trait MemoryHandler {
    fn read(&self, addr: u16) -> u8 ;
    fn read_word(&self, addr: u16) -> u16{
        (self.read(addr) as u16) << 8 | (self.read(addr) as u16)
    }
    fn write(&mut self, addr: u16, byte: u8);
    fn write_word(&mut self, addr: u16, word: u16) {
        self.write(addr, (word as u8));
        self.write(addr + 1, ((word << 8) as u8));
    }
}

pub struct Interconnect {
    pub cart: Cartridge,
    pub ram: Ram,
    pub registers: Registers,
}

impl Interconnect {
    pub fn default() -> Box<Interconnect> {

        Box::new(Interconnect {
            cart: Cartridge::default(),
            ram: Ram::default(),
            registers: Registers::default(),
        })
    }
}

impl MemoryHandler for Interconnect {

    // See https://wiki.nesdev.com/w/index.php/CPU_memory_map
    // TODO PPU address space
    fn read(&self, addr: u16) -> u8 {
        match addr {
            0 ... 0x07ff => self.ram.memory[addr as usize],
            0x0800 ... 0x1fff => self.ram.memory[addr as usize & 0x07ff],
            0x8000 ... 0xffff => self.cart.prg[addr as usize & 0x7fff],
            _ => panic!("Unrecognized addr: {:04x}", addr)
        }
    }


    fn write(&mut self, addr: u16, byte: u8) {
        match addr {
            0...0x07ff => self.ram.memory[addr as usize] = byte,
            0x0800...0x1fff => self.ram.memory[addr as usize & 0x07ff] = byte,
            0x8000...0xffff => self.cart.prg[addr as usize & 0x7fff] = byte,
            _ => eprintln!("Unable to write to memory address"),
        };

    }

}


