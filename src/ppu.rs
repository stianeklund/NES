use log::{debug, error, info, warn};
use minifb::{Scale, Window, WindowOptions};
use std::fmt::LowerHex;
use std::fmt::{Debug, Formatter, Result};

use crate::cartridge::Cartridge;
use crate::cpu::ExecutionContext;
use crate::interconnect::{Interconnect, MemoryMapper};
use crate::palette::{palette, palette_u32, PALETTE, PALETTE_U32, TEST_PALETTE};
use crate::ppu_registers::*;
use std::ops::{Index, IndexMut};

const WIDTH: usize = 240;
const HEIGHT: usize = 256;
const PALLET_SIZE: usize = 0x20;
const NAMETABLE_SIZE: usize = 0x800;
const SCANLINE_CYCLES: usize = 340;
const SCANLINE_FRAMES: usize = 261;

pub struct Ppu {
    pub frame: u8,    // frame counter
    pub cycle: usize, // 0 - 340?
    pub addr: u16,
    scanline: usize,
    pub frame_complete: bool,

    // Interrupt Flags
    pub vblank: bool,
    pub nmi_occurred: bool,

    pub system_palette: Vec<u8>,
    pub frame_palette: Vec<u8>,
    pub pattern_tables: Vec<u8>,
    pub nametable: Vec<u8>,
    pub reg: Registers,
    pub attribute_table: Vec<u8>,
    pub vram: Vec<u8>,
    pub buffer: Vec<u32>,
    // pub buffer: Box<[u16; 258 * 240]>,
}
impl Index<u16> for Ppu {
    type Output = u8;
    fn index(&self, index: u16) -> &u8 {
        &self.pattern_tables[index as usize]
    }
}
impl IndexMut<u16> for Ppu {
    fn index_mut(&mut self, index: u16) -> &mut u8 {
        &mut self.pattern_tables[index as usize]
    }
}
pub struct FrameBuffer {
    pub ppu: Ppu,
    pub fb: Vec<u32>,
    pub window: Window,
}
impl FrameBuffer {
    pub fn new() -> Self {
        let mut window = Window::new(
            "NES",
            WIDTH as usize,
            HEIGHT as usize,
            WindowOptions {
                borderless: false,
                title: true,
                resize: true,
                scale: Scale::X2,
            },
        )
        .unwrap();
        Self {
            ppu: Ppu::new(),
            fb: vec![0; WIDTH as usize * HEIGHT as usize],
            window,
        }
    }
}
impl Ppu {
    pub fn new() -> Self {
        Self {
            frame: 0,
            cycle: 0,
            addr: 0,
            scanline: 0,
            frame_complete: false,

            vblank: false,
            nmi_occurred: false,
            pattern_tables: vec![0; 0x16000],
            system_palette: vec![0; 64],
            frame_palette: vec![0; 8],
            nametable: vec![0; 0x16000],
            reg: Registers::default(),
            attribute_table: vec![],
            buffer: vec![0xff00_0000; WIDTH as usize * 1024 as usize * 4],
            // buffer: Box::new([0_u16; 258 * 240]),
            vram: vec![0; 0x16000],
        }
    }
    pub fn write_ppu_reg(&mut self, addr: u16, byte: u8) {
        self.addr = addr;
        match addr {
            0x2000 => {
                self.reg.write_ppu_ctrl(byte);
            }
            0x2001 => self.reg.write_ppu_mask(byte),
            0x2002 => panic!("Writes to PPUSTATUS is not allowed"),
            0x2003 => self.reg.write_oam_addr(byte as usize),
            0x2004 => self.reg.write_oam_data(byte),
            0x2005 => self.reg.write_ppu_scroll(byte),
            0x2006 => self.reg.write_ppu_addr(byte),
            0x2007 => self.reg.write_ppu_data(byte),
            0x4014 => self.reg.write_oam_dma(byte),
            /*// Pattern Tables to be split up or just use one array & mask off what we need?
            0x2008 ..= 0x2fff => self.nametable[addr as usize] = byte,
            // Mirror
            0x3000 ..= 0x3eff => self.nametable[addr as usize & 0x2eff] = byte,
            0x3f00 ..= 0x3fff => self.nametable[addr as usize] = byte,*/
            _ => panic!("PPU Write: Unrecognized address ${:04x}", addr),
        }
        eprintln!("PPU Write:0x{:x} to ${:04x}", byte, addr);
    }
    pub fn read_ppu_reg(&mut self, addr: u16) -> u8 {
        let result = match addr {
            0x2000..=0x2001 => self.dummy_read(addr),
            0x2002 => self.reg.read_status(),
            0x2003 => self.reg.read_oam_addr(),
            0x2004 => self.reg.read_oam_data(),
            0x2007 => self.reg.read_ppu_data(),
            _ => panic!("Attempting to read PPU with address:${:04x}", addr),
        };
        eprintln!("PPU Read:${:04x}, returned:{:02x}", addr, result);
        result
    }
    pub fn dummy_read(&mut self, _addr: u16) -> u8 {
        // eprintln!("PPU Dummy Read: ${:04x}", _addr);
        self.cycle += 2;
        0_u8
    }
    // Copy the first 8kB of the pattern table (from CHR rom)
    pub fn fill_pattern_table(&mut self, cart: &Cartridge) {
        self.pattern_tables = cart.chr.clone();
        self.nametable = cart.chr.clone();
        self.vram = cart.chr.clone();
    }

    // PPU Reset
    pub fn reset_ppu(&mut self) {
        self.frame = 0;
        self.cycle = 0;
        self.scanline = 0;
        self.reg.write_ppu_ctrl(0);
        self.reg.write_ppu_mask(0);
        self.reg.write_oam_addr(0);
    }
    pub fn draw_name_tables(&mut self) -> &Vec<u32> {
        for row in 0..HEIGHT {
            for column in 0..WIDTH {
                // Row of a tile is 0..=7
                let tile_number = self.vram[((row / 8 * 32) + (column / 8)) as usize];
                let tile_attribute = self.vram[0];
                let tile_addr = self.vram[0x2000 + (row / 8 * 0x100) + (row % 8) + (column / 8) * 0x10];
                let addr: u16 = (self.reg.ppu_ctrl.backgrnd_pattern_table_addr + (tile_number * 0x10) + row as u8 % 8).into();
                let pixel = ((self.vram[addr as usize] >> (7 - (column % 8))) & 1)
                    + ((self.vram[addr as usize + 8]) >> (7 - (column % 8)) & 1) * 3;
                let index = (row * WIDTH) + column as usize;

                self.buffer[index] = palette_u32(pixel);
                // self.buffer[index + 1] = palette_u32(pixel);
                // self.buffer[index + 2] = palette_u32(pixel);
                // self.buffer[(row * WIDTH ) + column] = PALETTE_U32[tile_pixel as usize];
                // self.buffer[(row * WIDTH ) + column] = PALETTE_U32[tile_pixel  as usize + 2];
                // self.buffer[(row * WIDTH ) + column] = PALETTE_U32[tile_pixel as usize + 3];
            }
        }
        &self.buffer
    }
    // TODO See link below for details on each registry and bit values
    // https://wiki.nesdev.com/w/index.php/PPU_registers#PPUSTATUS
    pub fn draw_pattern_tables(&mut self) -> &Vec<u32> {
        for row in 0..WIDTH {
            for column in 0..HEIGHT {
                // Row of a tile is 0..=7
                let tile_addr = (row / 8 * 0x100) + (row % 8) + (column / 8) * 0x10;
                // Render both pattern tables
                // Bit depth of 3
                let tile_pixel = ((self.read8(tile_addr as u16) >> (7 - (column % 8))) & 1)
                    + ((self.read8((tile_addr + 8) as u16) >> (7 - (column % 8))) & 1) * 3;
                let index = row * WIDTH + column as usize;

                self.buffer[index] = palette_u32(tile_pixel);
                // self.buffer[index + 1] = palette_u32(tile_pixel);
                // self.buffer[index + 2] = palette_u32(tile_pixel);
                // self.buffer[(row * WIDTH) + column] = palette(tile_pixel);
                // self.buffer[(row * WIDTH) + column] = palette(tile_pixel);
                // self.buffer[(row * WIDTH) + column] = palette(tile_pixel);
                // self.buffer[(row * WIDTH ) + column] = PALETTE[tile_pixel  as usize + 2] as u32;
                // self.buffer[(row * WIDTH ) + column] = PALETTE[tile_pixel as usize + 3] as u32;
            }
        }
        &self.buffer
    }
    pub fn step(&mut self) {
        self.cycle += 1;
        // Each scanline consists of 340 PPU Cycles
        if self.cycle > SCANLINE_CYCLES {
            self.cycle -= 341;
            self.scanline += 1;
            if self.scanline >= SCANLINE_FRAMES {
                // 261 cycles (Y Coordinate)
                self.scanline -= 1;
                self.frame_complete = true;
            }
        }
        // If our scanline has reached the VBLANK area (241 - 260), and we've at least run one
        // PPU cycle, we're in VBLANK
        if self.scanline == 241 && self.cycle == 1 {
            self.reg.ppu_status.vblank_start = true; // Set at dot 1 of line 241 (i.e cycle 1, scanline 241)
            self.vblank = true;
            // TODO NMI should probably not fire here?
            self.nmi_occurred = true;

            if self.reg.ppu_ctrl.nmi_result > 0 {
                self.nmi_occurred = true;
                eprintln!(
                    "NMI occurred. Scanline:{}, PPU Cycle:{}",
                    self.scanline, self.cycle
                );
            }
        } else if self.scanline == 261 && self.reg.ppu_status.sprite_zero_hit {
            self.vblank = false;
            self.nmi_occurred = false;
        }
        if self.scanline == 1 && self.cycle == 1 {
            self.reg.ppu_status.vblank_start = false; // Clear at pre-render line
            self.nmi_occurred = false;
            self.vblank = false;
            self.reg.ppu_status.sprite_overflow = false;
            self.reg.ppu_status.sprite_zero_hit = false;
        }
    }
}

// The PPU addresses a 16kB space, $0000-3FFF.
impl MemoryMapper for Ppu {
    fn read8(&self, addr: u16) -> u8 {
        debug!("Internal PPU Read: ${:04x}", addr);
        match addr {
            0..=0x0fff => self.pattern_tables[addr as usize],
            0x1000..=0x1fff => self.pattern_tables[addr as usize],
            0x2000..=0x23ff => self.nametable[addr as usize],
            0x2400..=0x27ff => self.nametable[addr as usize % 8],
            0x2800..=0x2bff => self.nametable[addr as usize],
            0x2c00..=0x2fff => self.nametable[addr as usize],
            0x3000..=0x3eff => self.nametable[addr as usize & 0x2eff],
            0x3f00..=0x3f1f => self.system_palette[addr as usize],
            0x3f20..=0x3fff => self.system_palette[addr as usize & 0x3f1f],
            _ => panic!("PPU Read: unrecognized address ${:04x}", addr),
        }
    }
    fn write8(&mut self, addr: u16, byte: u8) {
        match addr {
            0x0000..=0x0fff => panic!(
                "Attempting to write:{:02x} to ${:04x} pattern table 0",
                byte, addr
            ),
            0x1000..=0x1fff => panic!(
                "Attempting to write:{:02x} to ${:04x} pattern table 1",
                byte, addr
            ),
            0x2000..=0x23ff => panic!(
                "Attempting to write:{:02x} to ${:04x} nametable 0",
                byte, addr
            ),
            0x2400..=0x27ff => panic!(
                "Attempting to write:{:02x} to ${:04x} nametable 1",
                byte, addr
            ),
            0x2800..=0x2bff => panic!(
                "Attempting to write:{:02x} to ${:04x} nametable 2",
                byte, addr
            ),
            0x3000..=0x3eff => panic!(
                "Attempting to write:{:02x} to ${:04x} nametable 2 mirror",
                byte, addr
            ),
            0x3f20..=0x3fff => panic!(
                "Attempting to write:{:02x} to ${:04x} palette ram mirrror",
                byte, addr
            ),
            _ => {
                eprintln!(
                    "Attempting to write{:02x} to ${:04x}, in PPU space",
                    byte, addr
                );
            }
        }
    }
}
