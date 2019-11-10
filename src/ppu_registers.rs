use std::fmt::LowerHex;
use std::fmt::{Debug, Formatter, Result};
#[derive(Debug, Default)]
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
    pub nmi_result: u8,
}
// PPUMASK $2001 (write)
#[derive(Debug, Default)]
pub struct PpuMask {
    pub greyscale: u8,
    pub show_left_background: u8,
    pub show_left_sprites: u8,
    pub show_background: u8,
    pub show_sprites: u8,
    pub emphasize_red: u8,
    pub emphasize_green: u8,
    pub emphasize_blue: u8,
}
// PPUSTATUS $2002 (read)
#[derive(Debug, Default)]
pub struct PpuStatus {
    pub sprite_zero_hit: u8,
    pub sprite_overflow: u8,
    pub vblank_start: u8,
}
// OAMADDR $2003 (write)
#[derive(Debug, Default)]
pub struct OamAddr { pub oam_addr: u8 }
// OAMDATA (read/write)
#[derive(Debug, Default)]
pub struct OamData { pub oam_data: Vec<u8> }
// PPUSCROLL $2005 (write)
#[derive(Debug, Default)]
pub struct PpuScroll { pub data: u8 }
// PPUADDR  $2006 (write)
#[derive(Debug, Default)]
pub struct PpuAddr { pub data: u8 }
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
        self.ppu_ctrl.base_nametable_addr = (value >> 0) & 1;
        self.ppu_ctrl.vram_addr_increment = (value >> 2) & 1;
        self.ppu_ctrl.sprite_pattern_table = (value >> 3) & 1;
        self.ppu_ctrl.backgrnd_pattern_table_addr = (value >> 4) & 1;
        self.ppu_ctrl.sprite_size = (value >> 5) & 1;
        self.ppu_ctrl.master_slave_select = (value >> 6) & 1;
        self.ppu_ctrl.nmi_result = (value >> 7) & 1;
    }
    pub fn write_ppu_mask(&mut self, value: u8) {
        self.ppu_mask.greyscale = (value >> 0) & 1;
        self.ppu_mask.show_left_background = (value >> 1) & 1;
        self.ppu_mask.show_left_sprites = (value >> 2) & 1;
        self.ppu_mask.show_background = (value >> 3) & 1;
        self.ppu_mask.show_sprites = (value >> 4) & 1;
        self.ppu_mask.emphasize_red = (value >> 5) & 1;
        self.ppu_mask.emphasize_green = (value >> 6) & 1;
        self.ppu_mask.emphasize_blue = (value >> 7) & 1;
    }
    pub fn read_status(&self)  -> u8 {
        let mut result = 0;
        result |= self.ppu_status.sprite_overflow << 5;
        result |= self.ppu_status.sprite_zero_hit << 6;
        result |= self.ppu_status.vblank_start << 7;
        result as u8
    }
    pub fn write_oam_addr(&mut self, value: u8) {
        self.oam_addr.oam_addr = value;
    }
    pub fn write_oam_data(&mut self, value: u8) {
        self.oam_data.oam_data.push(value);
        self.oam_addr.oam_addr += 1; // writes to oam_data increment the address
    }
    pub fn read_oam_addr(&self) -> u8 {
        self.oam_addr.oam_addr
    }
    pub fn read_oam_data(&self) -> u8 {
        self.oam_data.oam_data[self.oam_addr.oam_addr as usize]
    }
    pub fn write_ppu_scroll(&mut self, value: u8) {
        self.ppu_scroll.data = value;
    }
    pub fn write_ppu_addr(&mut self, value: u8) {
        self.ppu_addr.data = value;
    }
    pub fn write_ppu_data(&mut self, value: u8) {
        self.ppu_data.data = value;
    }
    pub fn read_ppu_data(&self) -> u8 {
        self.ppu_data.data
        // TODO Increment here?
    }
    pub fn write_oam_dma(&mut self, value:u8) {
        self.oam_dma.data = value;
    }
}
