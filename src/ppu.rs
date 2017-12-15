use interconnect::MemoryMapper;

pub struct Ppu {
    pub chr: Vec<u8>,
    pub vram: Vec<u8>,
    addr: Vec<u16>
}
pub struct Oam {
    oam_addr: Vec<u8>,
    oam_data: Vec<u8>,
    oam_dma: Vec<u8>
}

impl Ppu {
    pub fn default() -> Self {
        Ppu {
            chr: vec![0; 0x2000],
            vram: vec![0; 16384],
            addr: vec![0u16],
        }
    }
}
// The PPU addresses a 16kB space, $0000-3FFF.
impl MemoryMapper for Ppu {
        fn read(&self, addr: u16) -> u8 {
        match addr {
            0... 0x1fff => self.chr[addr as usize],
            // TODO PPU Mirror? Is PPU size to `$3fff`?
            // Pattern Tables to be split up or just use one array & mask off what we need?
            0x2000 ... 0x2fff => self.vram[addr as usize],
            // Mirror
            0x3000 ... 0x3eff => self.vram[addr as usize & 0x2eff],
            0x3f00 ... 0x3fff => panic!("Internal palette control; not implemented"),
            _ => panic!("Unrecognized addr: {:04x}", addr)
        }
    }
    fn write(&mut self, addr: u16, byte: u8) {
        match addr {
            0... 0x1fff => self.chr[addr as usize] = byte,
            // TODO PPU Mirror? Is PPU size to `$3fff`?
            // Pattern Tables to be split up or just use one array & mask off what we need?
            0x2000 ... 0x2fff => self.vram[addr as usize] = byte,
            // Mirror
            0x3000 ... 0x3eff => self.vram[addr as usize & 0x2eff] = byte,
            0x3f00 ... 0x3fff => panic!("Internal palette control; not implemented"),
            _ => panic!("Unrecognized addr: {:04x}", addr)
        };

    }

}
