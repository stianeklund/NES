use interconnect::MemoryMapper;
pub struct Ppu {
    pub chr: Vec<u8>,
    pub vram: Vec<u8>,
    pub addr: Vec<u16>,
    pub reg: Registers,
    pub cycles: u8,
}
pub struct Registers {
    ppu_ctrl: u8,
    ppu_mask: u8,
    ppu_oam: u8,
    ppu_status: u8,
    oam_addr: u8,
    oam_data: u8,
    ppu_scroll: u8,
    ppu_addr: u8,
    ppu_data: u8,
    oam_dma: u8,
}

impl Registers {
    pub fn default() -> Self {
        Registers {
            ppu_ctrl: 0,
            ppu_mask: 0,
            ppu_oam: 0,
            ppu_status: 0,
            oam_addr: 0,
            oam_data: 0,
            ppu_scroll: 0,
            ppu_addr: 0,
            ppu_data: 0,
            oam_dma: 0,
        }
    }
    // Write helper for PPUCTRL ($2000) register
    // More info: https://wiki.nesdev.com/w/index.php/PPU_registers#PPUCTRL
    pub fn ppu_ctrl_write(&mut self, value: u8) {
        // TODO handle writing specific bits of the control register
        self.ppu_ctrl = value >> 3 & 0; // 0 = $2000; 1 = $2400; 2 = $2800; 3 = $2c00;
        self.ppu_ctrl = value >> 2 & 1; // VRAM addr (increment per CPU r/w of PPUDATA
        self.ppu_ctrl = value  >> 3 & 1;
        self.ppu_ctrl = value >> 4 & 1;
        self.ppu_ctrl = value >> 5 & 1;
        self.ppu_ctrl = value >> 6 & 1;
        self.ppu_ctrl = value >> 7 & 1;
        self.ppu_ctrl = value >> 0 & 1;

    }
    pub fn ppu_mask_write(&mut self, value: u8) {
        self.ppu_mask = value >> 0 & 1;
        self.ppu_mask = value >> 1 & 1;
        self.ppu_mask = value >> 2 & 1;
        self.ppu_mask = value >> 3 & 1;
        self.ppu_mask = value >> 4 & 1;
        self.ppu_mask = value >> 5 & 1;
        self.ppu_mask = value >> 6 & 1;
        self.ppu_mask = value >> 7 & 1;

    }
    pub fn ppu_data_write(&mut self, value: u8) {
        self.ppu_data = value >> 0 & 1;
        self.ppu_data = value >> 1 & 1;
        self.ppu_data = value >> 2 & 1;
        self.ppu_data = value >> 3 & 1;
        self.ppu_data = value >> 4 & 1;
        self.ppu_data = value >> 5 & 1;
        self.ppu_data = value >> 6 & 1;
        self.ppu_data = value >> 7 & 1;
    }
}
// Internal data bus for CPU communications
pub struct PpuDataBus {
// TODO
}
impl Ppu {
    pub fn default() -> Self {
        Ppu {
            chr: vec![0; 0x2000],
            vram: vec![0; 16384],
            addr: vec![0u16],
            reg: Registers::default(),
            cycles: 0,
        }
    }
}
// The PPU addresses a 16kB space, $0000-3FFF.
// TODO Improve mapper to handle writes to registers that have write enable
impl MemoryMapper for Ppu {
        fn read(&mut self, addr: u16) -> u8 {
            println!("PPU Read ${:04x}", addr);
            self.cycles.wrapping_add(1);
        match addr {
            0... 0x1fff => self.chr[addr as usize],
            // TODO PPU Mirror? Is PPU size to `$3fff`?
            // Pattern Tables to be split up or just use one array & mask off what we need?
            0x2000 => self.reg.ppu_ctrl,
            0x2001 => self.reg.ppu_mask,
            // https://wiki.nesdev.com/w/index.php/PPU_registers#PPUSTATUS
            0x2002 => self.reg.ppu_status,
            // R/W to this addr should increment VRAM by amount specified at $2000:2
            0x2007 => self.reg.ppu_data,
            0x2008 ... 0x2fff => self.vram[addr as usize],
            0x3000 ... 0x3eff => self.vram[addr as usize & 0x2eff],
            0x3f00 ... 0x3fff => panic!("Internal palette control; not implemented"),
            _ => panic!("PPU Read: unrecognized address ${:04x}", addr)
        }
    }
    fn write(&mut self, addr: u16, byte: u8) {
        match addr {
            0... 0x1fff => self.chr[addr as usize] = byte,
            0x2000 => self.reg.ppu_ctrl_write(byte),
            0x2001 => self.reg.ppu_mask_write(byte),
            0x2002 => self.reg.ppu_status = byte,
            0x2006 => self.reg.ppu_addr = byte,
            0x2007 =>  self.reg.ppu_data_write(byte),
            // TODO PPU Mirror? Is PPU size to `$3fff`?
            // Pattern Tables to be split up or just use one array & mask off what we need?
            0x2008 ... 0x2fff => self.vram[addr as usize] = byte,
            // Mirror
            0x3000 ... 0x3eff => self.vram[addr as usize & 0x2eff] = byte,
            0x3f00 ... 0x3fff => panic!("Internal palette control; not implemented"),
            _ => panic!("PPU Write: Unrecognized address ${:04x}", addr)
        };
        println!("PPU Write {:04x} to ${:04x}", byte, addr);

    }

}
