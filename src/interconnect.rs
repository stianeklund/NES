use crate::memory::Ram;
use crate::ppu::Ppu;
use crate::rom::Cartridge;
use crate::cpu::{ExecutionContext, Registers};


pub trait MemoryMapper {
    fn read8(&mut self, addr: u16) -> u8 ;
    fn read16(&mut self, addr: u16) -> u16 {
        u16::from(self.read8(addr)) | u16::from(self.read8(addr + 1)) << 8
    }
    fn write8(&mut self, addr: u16, byte: u8);
    fn write16(&mut self, addr: u16, word: u16) {
        self.write8(addr, word as u8);
        self.write8(addr + 1, (word >> 8) as u8);
    }
}
pub struct AddressMatch {
    byte: u8,
    address: u16,
    lookup: String
}
impl AddressMatch {
    pub fn resolve_addr(byte: u8, addr: u16) -> String {

        // Resolve known addresses for debug purposes
        let lookup = String::from(match addr {
            0 ..= 0x07ff => "RAM",
            0x0800 ..= 0x1fff => "RAM MIRROR",
            0x2000 => "PPU_CTRL",
            0x2001 => "PPU_MASK",
            0x2002 => "PPU_STATUS",
            0x2003 => "OAM_ADDR",
            0x2004 => "OAM_DATA",
            0x2006 => "PPU_ADDR",
            0x2007 => "PPU_DATA",
            0x2008 ..= 0x2fff => "PPU VRAM",
            0x3000 ..= 0x3eff => "PPU VRAM MIRROR",
            0x3f00 ..= 0x3fff => "INTERNAL PALETTE CONTROL",
            0x4000 ..= 0x4003 => "APU PULSE 1",
            0x4004 ..= 0x4007 => "APU PULSE 2",
            0x4008 ..= 0x400b => "APU TRIANGLE",
            0x400c ..= 0x400f => "APU NOISE",
            0x4010 ..= 0x4013 => "APU DMC",
            0x4015 => "APU REG CTRL",
            0x4017 => "APU FRAME COUNTER",
            0x6000 ..= 0x7fff => "SRAM",
            0x8000 ..= 0xffff => "CART",
            _ => "Uncovered address",
        });
        format!("Writing {:04x} to ${:04x} {}", byte, addr, lookup)
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

// TODO Remove, we don't need to implement mapping here?
/* impl MemoryMapper for Interconnect {

    // See https://wiki.nesdev.com/w/index.php/CPU_memory_map
    // TODO PPU address space
    fn read(&self, addr: u16) -> u8 {
        match addr {
            0 ..= 0x07ff => self.ram.memory[addr as usize],
            0x0800 ..= 0x1fff => self.ram.memory[addr as usize & 0x07ff],
            0x2000 ..= 0x3fff => panic!("Trying to read from PPU registers. Not implemented"),
            0x8000 ..= 0xffff => {
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
            0..=0x07ff => self.ram.memory[addr as usize] = byte,
            0x0800..=0x1fff => self.ram.memory[addr as usize & 0x07ff] = byte,
            0x2000 ..= 0x3fff => eprintln!("Writing to PPU registers is not implemented"),
            0x8000..=0xffff => self.cart.prg[addr as usize & 0x3fff] = byte,
            _ => eprintln!("Unable to write to memory address"),
        };

    }

}*/



