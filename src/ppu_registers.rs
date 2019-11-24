use std::fmt::LowerHex;
use std::fmt::{Debug, Formatter, Result};
use super::ppu::Ppu;

//#[derive(Debug, Default)]
#[derive(Default)]
pub struct Registers {
    pub ppu_ctrl: PpuCtrl,
    pub ppu_mask: PpuMask,
    pub ppu_status: PpuStatus,
    pub oam_data: OamData,
    pub oam_addr: OamAddr,
    pub ppu_scroll: PpuScroll,
    pub ppu_addr: PpuAddr,
    pub ppu_data: PpuData,
    pub oam_dma: OamDma,
}
// https://wiki.nesdev.com/w/index.php/PPU_registers

// PPUCTRL $2000 (write)
#[derive(Debug, Default)]
pub struct PpuCtrl {
    pub base_nametable_addr: u8,
    pub vram_addr_increment: u8,
    pub sprite_pattern_table: u8,
    pub backgrnd_pattern_table_addr: u8,
    pub sprite_size: u8,
    pub master_slave_select: u8,
    pub nmi_occurred: u8,
}
// PPUMASK $2001 (write)
#[derive(Debug, Default)]
pub struct PpuMask {
    pub greyscale: bool,
    pub show_left_background: bool,
    pub show_left_sprites: bool,
    pub show_background: bool,
    pub show_sprites: bool,
    pub emphasize_red: bool,
    pub emphasize_green: bool,
    pub emphasize_blue: bool,
}
// PPUSTATUS $2002 (read)
#[derive(Debug, Default)]
pub struct PpuStatus {
    pub sprite_zero_hit: bool,
    pub sprite_overflow: bool,
    pub vblank_start: bool,
}
// OAMADDR $2003 (write)
#[derive(Debug, Default)]
pub struct OamAddr { pub addr : usize }

// OAMDATA (read/write)
// #[derive(Debug, Default)]
pub struct OamData { pub oam_data: [u8; 256] }

impl Default for OamData {
    fn default() -> Self {
        OamData {
            oam_data: [0_u8; 256]
        }
    }
}

// PPUSCROLL $2005 (write)
#[derive(Debug, Default)]
pub struct PpuScroll { pub data: u8 }
// PPUADDR  $2006 (write)
#[derive(Debug, Default)]
pub struct PpuAddr { pub addr: u8 }

// PPUDATA $2007 (read/write)
// VRAM read/write data register. After access, the video memory address will increment by an amount determined by bit 2 of $2000.
#[derive(Debug, Default)]
pub struct PpuData { pub data: u8 }
// OAMDMA $4014 (write)
#[derive(Debug, Default)]
pub struct OamDma { pub data: u8}

impl LowerHex for Registers {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let value = self;
        write!(f, "{:04x}", value)
    }
}
// TODO We need to figure out a way to set values from a CPU read
// Mutability is an issue here as the CPU's `read8()` method, or all `read8()` methods
// derived from the MemoryMapper trait only take a self reference, no mutability involved.
// Certain PPU flags are changed when a read is performed to PPU address space.
// More info: https://wiki.nesdev.com/w/index.php/PPU_registers#PPUCTRL
// In the order bit 0 - 7

impl Registers {
    pub fn write_ppu_ctrl(&mut self, value: u8)  {
        self.ppu_ctrl.base_nametable_addr = value & 3;
        self.ppu_ctrl.vram_addr_increment = value >> 2 & 1;
        self.ppu_ctrl.sprite_pattern_table = value >> 3 & 1;
        self.ppu_ctrl.backgrnd_pattern_table_addr = value >> 4 & 1;
        self.ppu_ctrl.sprite_size = value >> 5 & 1;
        self.ppu_ctrl.master_slave_select = value >> 6 & 1;
        self.ppu_ctrl.nmi_occurred = value >> 7 & 1;
        // eprintln!("PPUCTRL:{:x?}", self.ppu_ctrl);
        // eprintln!("Background PPU Table Addr:{:04x}", self.ppu_ctrl.backgrnd_pattern_table_addr);
    }
    pub fn write_ppu_mask(&mut self, value: u8) {
        self.ppu_mask.greyscale = (value & 0x01) != 0;
        self.ppu_mask.show_left_background = (value & 0x02) != 0;
        self.ppu_mask.show_left_sprites = (value & 0x04) != 0;
        self.ppu_mask.show_background = (value & 0x08) != 0;
        self.ppu_mask.show_sprites = (value & 0x10) != 0;
        self.ppu_mask.emphasize_red = (value & 0x20) != 0;
        self.ppu_mask.emphasize_green = (value & 0x40) != 0;
        self.ppu_mask.emphasize_blue = (value & 0x80) != 0;
    }
    pub fn read_status(&mut self) -> u8 {
        let result = if self.ppu_status.vblank_start { 0x80 } else { 0x0 } |
            if self.ppu_status.sprite_zero_hit { 0x40 } else { 0x0 } |
            if self.ppu_status.sprite_overflow { 0x20 } else { 0x0 };

        // Rest of the bits not used? See nesdev wiki
        self.ppu_status.vblank_start = false;
        result
    }
    pub fn write_oam_addr(&mut self, addr: usize) {
        self.oam_addr.addr = addr;
    }
    pub fn write_oam_data(&mut self, value: u8) {
        self.oam_data.oam_data[self.oam_addr.addr as usize];
        self.oam_addr.addr += 1;

    }
    pub fn read_oam_addr(&self) -> u8 { self.oam_addr.addr as u8 }
    pub fn read_oam_data(&self) -> u8 {
        self.oam_data.oam_data[self.oam_addr.addr as usize]
    }
    pub fn write_ppu_scroll(&mut self, value: u8) { self.ppu_scroll.data = value; }
    pub fn write_ppu_addr(&mut self, addr: u8) {
        // Writes to $2006 need to be done twice, once for the high byte and once for the low byte
        // This address points for example to the pallet location (PPU Address $3f00 - $3f10
        self.ppu_addr.addr = addr;

    }
    // $2007
    pub fn write_ppu_data(&mut self, value: u8) {
        self.ppu_data.data = value;
    }
    pub fn read_ppu_data(&mut self) -> u8 {
        let data = self.ppu_data.data;
        // self.ppu_addr.addr += 1;
        // eprintln!("PPU ADDR:{:x}", self.ppu_addr.addr);
        // Per NESDEV this address should be incremented by the amount
        // determined by bit 2 of $2000 PPUCTRL
        self.ppu_addr.addr += self.ppu_ctrl.vram_addr_increment;
        data
    }
    pub fn write_oam_dma(&mut self, value:u8) { self.oam_dma.data = value; }
}
