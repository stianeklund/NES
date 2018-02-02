use rom::Cartridge;
use cpu::{ExecutionContext, Cpu, Registers};
use memory::Ram;
use ppu::Ppu;

pub trait MemoryMapper {
    fn read(&self, addr: u16) -> u8 ;
    fn read_word(&self, addr: u16) -> u16 {
        (self.read(addr) as u16) | (self.read(addr + 1) as u16) << 8
    }
    fn write(&mut self, addr: u16, byte: u8);
    fn write_word(&mut self, addr: u16, word: u16) {
        self.write(addr, (word as u8));
        self.write(addr + 1, ((word >> 8) as u8));
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

impl MemoryMapper for Interconnect {

    // See https://wiki.nesdev.com/w/index.php/CPU_memory_map
    // TODO PPU address space
    fn read(&self, addr: u16) -> u8 {
        match addr {
            0 ... 0x07ff => self.ram.memory[addr as usize],
            0x0800 ... 0x1fff => self.ram.memory[addr as usize & 0x07ff],
            0x2000 ... 0x3fff => panic!("Trying to read from PPU registers. Not implemented"),
            0x8000 ... 0xffff => {
                let mut prg_size = 0;
                if self.cart.header.prg_rom_size == 1 { prg_size = 0x3fff; }
                else { prg_size = 0x7fff; }
                self.cart.prg[addr as usize & prg_size]
            },
            _ => panic!("Unrecognized addr: {:04x}", addr)
        }
    }


    fn write(&mut self, addr: u16, byte: u8) {
        match addr {
            0...0x07ff => self.ram.memory[addr as usize] = byte,
            0x0800...0x1fff => self.ram.memory[addr as usize & 0x07ff] = byte,
            0x2000 ... 0x3fff => eprintln!("Writing to PPU registers is not implemented"),
            0x8000...0xffff => self.cart.prg[addr as usize & 0x3fff] = byte,
            _ => eprintln!("Unable to write to memory address"),
        };

    }

}


