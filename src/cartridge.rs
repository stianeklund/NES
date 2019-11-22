use crate::interconnect::MemoryMapper;
use std::fs::File;
use std::io::Read;
use std::io::{Error, Result};
use std::ops::{Index, IndexMut};
use std::ops::{Range, RangeTo};
use std::path::Path;
use std::str;

/* ******************************************************************************************** */
// iNES HEADER INFORMATION
// The format of the header is as follows:
//  0-3: Constant $4E $45 $53 $1A ("NES" followed by MS-DOS end-of-file)
//  4: Size of PRG ROM in 16 KB units
//  5: Size of CHR ROM in 8 KB units (Value 0 means the board uses CHR RAM)
//  6: Flags 6
//  7: Flags 7
//  8: Size of PRG RAM in 8 KB units (Value 0 infers 8 KB for compatibility; see PRG RAM circuit)
//  9: Flags 9
//  10: Flags 10 (unofficial)
//  11-15 Zero filled
/* ******************************************************************************************** */

// Defined constant sizes for the PRG & CHR banks
const PRG_ROM_BANK_SIZE: usize = 16384;
const CHR_ROM_BANK_SIZE: usize = 8192;

#[derive(Debug)]
pub struct RomHeader {
    pub magic: [u8; 4],
    sram: u8,
    pub prg_rom_page_size: usize,
    pub chr_rom_page_size: usize,
    pub flags_6: u8,
    flags_7: u8,
    pub prg_ram_page_size: usize,
    pub chr_ram_page_size: usize,
    flags_9: u8,
    flags_10: u8,
    pub zero: [u8; 5],
}

impl RomHeader {
    fn new() -> Self {
        Self {
            magic: [0; 4],
            sram: 0,
            prg_rom_page_size: 0,
            chr_rom_page_size: 0,
            flags_6: 0,
            flags_7: 0,
            prg_ram_page_size: 0,
            chr_ram_page_size: 0,
            flags_9: 0,
            flags_10: 0,
            zero: [0; 5],
        }
    }
}
impl Index<u16> for Cartridge {
    type Output = u8;
    fn index(&self, index: u16) -> &u8 {
        &self.prg[index as usize]
    }
}
impl IndexMut<u16> for Cartridge {
    fn index_mut(&mut self, index: u16) -> &mut u8 {
        &mut self.prg[index as usize]
    }
}

impl MemoryMapper for Cartridge {
    fn read8(&self, addr: u16) -> u8 {
        println!("Cart read: ${:04x}", addr);
        match addr {
            0..=0x07ff => panic!("Trying to read RAM from Cartridge"),
            0x0800..=0x1fff => panic!("Trying to read RAM Mirror from Cartridge"),
            0x2000..=0x3fff => panic!("Trying to read from PPU registers. Not implemented"),
            0x8000..=0xffff => self.prg[addr as usize & 0x3fff],
            _ => panic!("Unrecognized read address: {:04x}", addr),
        }
    }
    fn read16(&self, addr: u16) -> u16 {
        u16::from_le_bytes([self.read8(addr), self.read8(addr + 1)])
    }
    fn write8(&mut self, addr: u16, byte: u8) {
        println!("Cart write: {:04x} to ${:04x}", byte, addr);
        match addr {
            0..=0x07ff => self.write8(addr, byte),
            0x0800..=0x1fff => self.write8(addr, byte),
            0x2000..=0x3fff => self.write8(addr, byte),
            0x8000..=0xffff => self.write8(addr, byte),
            _ => eprintln!("Unable to write to memory address"),
        }
    }
}

#[derive(Debug)]
pub struct Cartridge {
    pub header: RomHeader, // iNES Header
    pub prg: Vec<u8>,      // A copy of the games program rom. First 8kB is CHR rom.
    pub chr: Vec<u8>,      // Copy of the games pattern table ROM or RAM for save states.
    pub rom: Vec<u8>,      // Temporary copy of ROM contents
    pub mapper_id: u8,     // Mapper ID
}

impl Cartridge {
    pub fn new() -> Self {
        Self {
            header: RomHeader::new(),
            prg: vec![0; 4 * PRG_ROM_BANK_SIZE],
            chr: vec![0; CHR_ROM_BANK_SIZE],
            rom: vec![0; 0x85_000],
            mapper_id: 0,
        }
    }
    fn mask_addr(&self, addr: u16) -> u16 {
        let mask = (self.rom.len() - 1) as u16;
        println!("Mask addr:{:04X}", addr & mask as u16);
        addr & mask
    }
    // Returns the mapper ID (for mapper identification)
    pub fn retrieve_mapper_id(&self) -> u8 {
        (self.header.flags_7 & 0xf0) | (self.header.flags_6 >> 4)
    }

    pub fn read_rom(&mut self, file: &str) {
        let path = Path::new(file);
        let mut f = File::open(&path).expect("Couldn't find ROM");
        let mut buf = Vec::new();

        f.read_to_end(&mut buf)
            .expect("i/o error, could not read file to end");
        self.rom[16..buf.len()].clone_from_slice(&buf[16..]);

        println!(
            "\nLoaded: {} Size(KB): {:?}",
            path.to_str().unwrap(),
            (buf.len() as f64 * 0.000_976_562_5) as u32
        );
    }

    fn validate_header(&mut self, header: &Vec<u8>) -> Result<RomHeader> {
        // The iNES header is 16 bytes long
        self.mapper_id = (header[7] & 0x0f) | (header[6] >> 4);
        self.header.magic = [header[0], header[1], header[2], header[3]];

        if self.header.magic.is_ascii() {
            let magic = str::from_utf8(&self.header.magic)
                .unwrap()
                .trim_end_matches('');
            println!("ROM header: {}", magic);

            // Print bank sizes
            println!("PRG ROM {} 16KB Pages", header[4]);
            println!("CHR ROM {}  8KB Pages", header[5]);
            println!("PRG RAM {}  8KB Pages", header[8]);

            // TODO Research mappers & add more
            let (id, string) = match self.mapper_id {
                0 => (0, "NROM"),
                1 => (1, "MMC1"),
                2 => (2, "UxROM"),
                _ => panic!("Unimplemented mapper")
            };
            println!("Mapper:{}", string);
        }

        // Return header information
        Ok(RomHeader {
            magic: [header[0], header[1], header[2], header[3]],
            sram: 0,
            prg_rom_page_size: header[4] as usize,
            chr_rom_page_size: header[5] as usize,
            flags_6: header[6],
            flags_7: header[7],
            prg_ram_page_size: header[8] as usize,
            chr_ram_page_size: 0,
            flags_9: 0,
            flags_10: header[9],
            zero: [header[11], header[12], header[13], header[14], header[15]],
        })
    }

    pub fn load_rom(&mut self, mut file: &File) {
        // The iNES header is 16 bytes long
        let mut header = vec![0_u8; 16];
        file.read_exact(&mut header)
            .expect("Could not read rom file");
        self.header = self.validate_header(&header).unwrap();

        let mut prg: Vec<u8> = vec![0; PRG_ROM_BANK_SIZE];
        let mut chr: Vec<u8> = vec![0; CHR_ROM_BANK_SIZE];

        file.read_exact(&mut prg).unwrap();
        let prg_size = match self.header.prg_rom_page_size {
            1 => 16384,
            2 => 32768,
            _ => panic!("Prg size not implemented"),
        };
        for i in 0..prg_size {
            self.prg[i as usize] = prg[i as usize];
        }
        let chr_size = match self.header.chr_rom_page_size {
            1 => 8192,
            2 => 16384,
            _ => panic!("Chr size not implemented"),
        };
        file.read_exact(&mut chr).unwrap();
        for i in 0..chr_size {
            self.chr[i as usize] = chr[i as usize];
        }
    }
}
