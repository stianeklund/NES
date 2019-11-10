use std::fmt::LowerHex;
use std::fmt::{Debug, Formatter, Result};

#[derive(Debug)]
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
impl LowerHex for Registers {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let value = self;
        write!(f, "{:04x}", value)
    }
}
#[derive(Default, Debug)]
pub struct PpuCtrl { pub value: u8 }

// TODO We need to figure out a way to set values from a CPU read
// Mutability is an issue here as the CPU's `read8()` method, or all `read8()` methods
// derived from the MemoryMapper trait only take a self reference, no mutability involved.
// Certain PPU flags are changed when a read is performed to PPU address space.
impl PpuCtrl {
    fn vram_inc(&mut self)  {
        if self.value & 0x02 == 0 {
           self.value = 0;
        } else {
            self.value = 240
        }
    }
}
#[derive(Default, Debug)]
pub struct PpuMask { pub data: u8 }
#[derive(Default, Debug)]
pub struct PpuStatus { pub value: u8 }
#[derive(Default, Debug)]
pub struct OamData { pub value: u8 }
#[derive(Default, Debug)]
pub struct OamAddr { pub addr: u16 }
#[derive(Default, Debug)]
pub struct PpuScroll { pub value: u8 }
#[derive(Default, Debug)]
pub struct PpuAddr { pub addr: u16 }
#[derive(Default, Debug)]
pub struct PpuData { pub data: u8 }
#[derive(Default, Debug)]
pub struct OamDma { pub data: u8 }

impl Registers {
    pub fn new() -> Self {
        Registers {
            ppu_ctrl: PpuCtrl::default(),
            ppu_mask: PpuMask::default(),
            ppu_status: PpuStatus::default(),
            oam_data: OamData::default(),
            oam_addr: OamAddr::default(),
            ppu_scroll: PpuScroll::default(),
            ppu_addr: PpuAddr::default(),
            ppu_data: PpuData::default(),
            oam_dma: OamDma::default(),
        }
    }
    pub fn ppu_read(self, addr: u16) -> bool {
        match addr {
            0x2002 => self.ppu_status.value & !0x80,
            0x2004 => self.oam_data.value,
            _ => 0,
        };
        true
    }
    // Write helper for PPUCTRL ($2000) register
    // More info: https://wiki.nesdev.com/w/index.php/PPU_registers#PPUCTRL
    pub fn ppu_ctrl_write(&mut self, value: u8) {
        // TODO handle writing specific bits of the control register
        self.ppu_ctrl.value = value >> 3 & 0; // 0 = $2000; 1 = $2400; 2 = $2800; 3 = $2c00;
        self.ppu_ctrl.value = value >> 2 & 1; // VRAM addr (increment per CPU r/w of PPUDATA
        self.ppu_ctrl.value = value >> 3 & 1;
        self.ppu_ctrl.value = value >> 4 & 1;
        self.ppu_ctrl.value = value >> 5 & 1;
        self.ppu_ctrl.value = value >> 6 & 1;
        self.ppu_ctrl.value = value >> 7 & 1;
        self.ppu_ctrl.value = value >> 0 & 1;
    }
    pub fn ppu_mask_write(&mut self, value: u8) {
        self.ppu_mask.data = value >> 0 & 1;
        self.ppu_mask.data = value >> 1 & 1;
        self.ppu_mask.data = value >> 2 & 1;
        self.ppu_mask.data = value >> 3 & 1;
        self.ppu_mask.data = value >> 4 & 1;
        self.ppu_mask.data = value >> 5 & 1;
        self.ppu_mask.data = value >> 6 & 1;
        self.ppu_mask.data = value >> 7 & 1;
    }
    pub fn ppu_data_write(&mut self, value: u8) {
        self.ppu_data.data = value >> 0 & 1;
        self.ppu_data.data = value >> 1 & 1;
        self.ppu_data.data = value >> 2 & 1;
        self.ppu_data.data = value >> 3 & 1;
        self.ppu_data.data = value >> 4 & 1;
        self.ppu_data.data = value >> 5 & 1;
        self.ppu_data.data = value >> 6 & 1;
        self.ppu_data.data = value >> 7 & 1;
    }
}
