use crate::rom::Cartridge;
pub struct NROM {
    chr_rom: Vec<u8>,
    chr_ram: Vec<u8>,
    palette_ram: Vec<u8>,
    prg_upper: Vec<u8>,
    prg_lower: Vec<u8>,

}

impl MemoryMapper for NROM {
    fn read8(&self, addr: u16) -> u8 {
        let mask_amount = if self.cart.header.prg_rom_page_size != 1 { 0x7fff } else { 0x3fff };
        self.cart.prg[addr as usize & mask_amount]
    }
    fn read16(&self, addr: u16) -> u16 {
        u16::from_le_bytes([self.read8(addr), self.read8(addr + 1)])
    }
    fn write8(&mut self, addr: u16, byte: u8);
    fn write16(&mut self, addr: u16, word: u16) {
        self.write8(addr, word as u8);
        self.write8(addr + 1, (word >> 8) as u8);
    }
}