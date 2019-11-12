use minifb::{Scale, WindowOptions, Window};
use std::fmt::{Debug, Formatter, Result};
use std::fmt::LowerHex;
use log::{info, warn, debug, error};

use crate::interconnect::{MemoryMapper, Interconnect};
use crate::rom::Cartridge;
use crate::ppu_registers::*;

const WIDTH: u32 = 240;
const HEIGHT: u32 = 256;
const PALLET_SIZE: usize= 0x20;
const NAMETABLE_SIZE: usize = 0x800;
const SCANLINE_CYCLES: usize = 341;
const SCANLINE_FRAMES: usize = 261;


pub struct Ppu {
    pub frame: u8, // frame counter
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
    pub buffer: Vec<u16>,
    pub vram: Vec<u16>,
    pub cart: Cartridge,

}

pub struct FrameBuffer {
    pub ppu: Ppu,
    pub buffer: Box<[u16; 258 * 240]>,
    pub window: Window
}
    pub fn new() -> FrameBuffer {
    let mut window = Window::new(
        "NES",
        WIDTH as usize,
        HEIGHT as usize,
        WindowOptions {
            borderless: false,
            title: false,
            resize: false,
            ..WindowOptions::default()
        },
    ).unwrap();
   FrameBuffer {
       ppu: Ppu::new(),
       buffer: Box::new([0_u16; 258 * 240]),
       window
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
            pattern_tables: vec![0; 0x2000],
            system_palette: vec![0; 64],
            frame_palette: vec![0; 8],
            nametable: vec![0; 2048],
            reg: Registers::default(),
            attribute_table: vec![],
            buffer: vec![HEIGHT as u16 * WIDTH as u16],
            vram: vec![0; 0x2000],
            cart: Cartridge::new(),
        }
    }
    pub fn write_ppu_reg(&mut self, addr: u16, byte: u8) {
        self.addr = addr;
        match addr {
            0x2000 => { self.reg.write_ppu_ctrl(byte); },
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
            _ => panic!("PPU Write: Unrecognized address ${:04x}", addr)
        }
        eprintln!("PPU Write:0x{:x} to ${:04x}", byte, addr);
    }
    pub fn read_ppu_reg(&mut self, addr: u16) -> u8 {
        let result = match addr {
            0x2000 ..= 0x2001 => self.dummy_read(addr),
            0x2002 => self.reg.read_status(),
            0x2003 => self.reg.read_oam_addr(),
            0x2004 => self.reg.read_oam_data(),
            0x2007 => self.reg.read_ppu_data(),
            _ => panic!("Attempting to read PPU with address:${:04x}", addr)
        };
        eprintln!("PPU Read:${:04x}, returned:{:02x}", addr, result);
        result
    }
    pub fn dummy_read(&mut self, _addr: u16) -> u8 {
        // eprintln!("PPU Dummy Read: ${:04x}", _addr);
        self.cycle += 2;
        0_u8
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
    // TODO See link below for details on each registry and bit values
    // https://wiki.nesdev.com/w/index.php/PPU_registers#PPUSTATUS

    pub fn step(&mut self) {

        self.cycle += 1;
        if self.cycle >= SCANLINE_CYCLES { // 341 cycles (X coordinate)
            self.cycle = 0;
            self.scanline += 1;
           if self.scanline >= SCANLINE_FRAMES { // 261 cycles (Y Coordinate)
                self.scanline -= 1;
                self.frame_complete = true;

            }

        }
        // If our scanline has reached the VBLANK area (241 - 260), and we've at least run one
        // PPU cycle, we're in VBLANK
        if self.scanline == 241 && self.cycle == 1 {
            self.reg.ppu_status.vblank_start = true; // Set at dot 1 of line 241 (i.e cycle 1, scanline 241)
            self.vblank = true;
            // NMI should probably not fire here?
            // self.nmi_occurred = true;

            if self.reg.ppu_ctrl.nmi_result > 0 {
                self.nmi_occurred = true;
                eprintln!("NMI occurred. Scanline:{}, PPU Cycle:{}", self.scanline, self.cycle);
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
        println!("Internal PPU Read ${:04x}", addr);
        match addr {
            0 ..= 0x0fff =>  self.pattern_tables[addr as usize],
            0x1000 ..= 0x1fff => self.pattern_tables[addr as usize],
            0x2000 ..= 0x23ff => self.nametable[addr as usize],
            0x2400 ..= 0x27ff => self.nametable[addr as usize % 8],
            0x2800 ..= 0x2bff => self.nametable[addr as usize],
            0x2c00 ..= 0x2fff => self.nametable[addr as usize],
            0x3000 ..= 0x3eff => self.nametable[addr as usize & 0x2eff],
            0x3f00 ..= 0x3f1f => self.system_palette[addr as usize],
            0x3f20 ..= 0x3fff => self.system_palette[addr as usize & 0x3f1f],
            _ => panic!("PPU Read: unrecognized address ${:04x}", addr)
        }
    }
    fn write8(&mut self, addr: u16, byte: u8) {
       match addr {
           0x0000 ..= 0x0fff => panic!("Attempting to write:{:02x} to ${:04x} pattern table 0", byte, addr),
           0x1000 ..= 0x1fff => panic!("Attempting to write:{:02x} to ${:04x} pattern table 1", byte, addr),
           0x2000 ..= 0x23ff => panic!("Attempting to write:{:02x} to ${:04x} nametable 0", byte, addr),
           0x2400 ..= 0x27ff => panic!("Attempting to write:{:02x} to ${:04x} nametable 1", byte, addr),
           0x2800 ..= 0x2bff => panic!("Attempting to write:{:02x} to ${:04x} nametable 2", byte, addr),
           0x3000 ..= 0x3eff => panic!("Attempting to write:{:02x} to ${:04x} nametable 2 mirror", byte, addr),
           0x3f20 ..= 0x3fff => panic!("Attempting to write:{:02x} to ${:04x} palette ram mirrror", byte, addr),
           _ => { eprintln!("Attempting to write{:02x} to ${:04x}, in PPU space", byte, addr);
           }
       }
    }
}


