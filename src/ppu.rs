use minifb::{Scale, WindowOptions, Window};
use std::fmt::{Debug, Formatter, Result};
use std::fmt::LowerHex;

use crate::interconnect::{MemoryMapper, Interconnect};
use crate::rom::Cartridge;
use crate::ppu_registers::*;

pub const WIDTH: u32 = 240;
pub const HEIGHT: u32 = 256;
pub const PALLET_SIZE: usize= 0x400;
pub const NAMETABLE_SIZE: usize = 0x20;

pub struct Vram {
    pub palettes: [u8; PALLET_SIZE],
    pub nametables: [u8; NAMETABLE_SIZE],
    cart: Cartridge,
    // We need cartridge access here?
}
pub struct Ppu {
    pub vram: Vram,
    pub pattern_tables: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub addr: Vec<u16>,
    pub reg: Registers,
    pub cycle: u8,
    pub scanline: u8,
    pub nametable: Vec<u8>,
    pub attribute_table: Vec<u8>,
    pub vblank: bool,
    pub buffer: Vec<u16>,
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
       buffer: Box::new([0u16; 258 * 240]),
       window
   }
}
impl Vram {
    pub fn new() -> Self {
        Self {
            palettes: [0; PALLET_SIZE],
            nametables: [0; NAMETABLE_SIZE],
            cart: Cartridge::default(),
        }
    }
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            vram: Vram::new(),
            pattern_tables: vec![0; 0x2000],
            chr_rom: vec![0; 0x2000],
            addr: vec![0u16],
            reg: Registers::new(),
            cycle: 0,
            scanline: 0,
            nametable: vec![0; 0x1200],
            attribute_table: vec![],
            vblank: false,
            buffer: vec![HEIGHT as u16 * WIDTH as u16],
        }
    }
    // TODO See link below for details on each registry and bit values
    // https://wiki.nesdev.com/w/index.php/PPU_registers#PPUSTATUS

    pub fn step(&mut self) {
        // TODO When a read to PPU space happens, PPU registers need to change.
        // let _sprite0_hit = self.reg.ppu_status & 0x40;
        // Reads to the status register clears bit 7
        // self.reg.ppu_status = (self.reg.ppu_status & !0x80);
        // self.reg.ppu_status
        // self.reg.ppu_status = self.reg.ppu_status.value;
    }
}

// The PPU addresses a 16kB space, $0000-3FFF.
impl MemoryMapper for Ppu {
    fn read8(&self, addr: u16) -> u8 {
        println!("Internal PPU Read ${:04x}", addr);
        match addr {
            0 ..= 0x0fff => self.pattern_tables[addr as usize],
            0x1000 ..= 0x1fff => self.pattern_tables[addr as usize],
            0x2000 ..= 0x23ff => self.nametable[addr as usize],
            0x2400 ..= 0x27ff => self.vram.nametables[addr as usize],
            0x2800 ..= 0x2bff => self.vram.nametables[addr as usize],
            0x2c00 ..= 0x2fff => self.vram.nametables[addr as usize],
            0x3000 ..= 0x3eff => self.vram.nametables[addr as usize & 0x2eff],
            0x3f00 ..= 0x3f1f => self.vram.palettes[addr as usize],
            0x3f20 ..= 0x3fff => self.vram.palettes[addr as usize & 0x3f1f],
            _ => panic!("PPU Read: unrecognized address ${:04x}", addr)
        }
    }
    fn write8(&mut self, addr: u16, byte: u8) {
        match addr {
            0 ..= 0x1fff => self.chr_rom[addr as usize] = byte,
            0x2000 => self.reg.ppu_ctrl_write(byte),
            0x2001 => self.reg.ppu_mask_write(byte),
            0x2002 => self.reg.ppu_status.value = byte,
            0x2003 => self.reg.oam_addr.addr = byte as u16,
            0x2004 => self.reg.oam_data.value = byte,
            0x2005 => self.reg.ppu_scroll.value = byte,
            0x2006 => self.reg.ppu_addr.addr = byte as u16,
            0x2007 =>  self.reg.ppu_data_write(byte),
            // TODO PPU Mirror? Is PPU size to `$3fff`?
            // Pattern Tables to be split up or just use one array & mask off what we need?
            0x2008 ..= 0x2fff => self.vram.nametables[addr as usize] = byte,
            // Mirror
            0x3000 ..= 0x3eff => self.vram.nametables[addr as usize & 0x2eff] = byte,
            0x3f00 ..= 0x3fff => self.vram.palettes[addr as usize] = byte,
            _ => panic!("PPU Write: Unrecognized address ${:04x}", addr)
        };
        eprintln!("PPU write ${:04x} with:{:x}", addr, byte);
    }
}
