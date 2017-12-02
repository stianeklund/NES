
use std::ops::{Range, RangeTo, Index};
use std::fmt;
use std::path::Path;
use std::io::Read;
use std::fs::File;
use std::io::{Result, Error};
use std::str;
use memory::{Memory, Ram};
use std::convert;

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
    pub magic: Box<[u8; 4]>,
    sram: u8,
    pub prg_rom_size: u8,
    pub chr_rom_size: u8,
    pub flags_6: u8,
    flags_7: u8,
    pub prg_ram_size: u8,
    pub chr_ram_size: u8,
    flags_9: u8,
    flags_10: u8,
    pub zero: Box<[u8; 5]>,
}

impl Default for RomHeader {
    fn default() -> Self {
        RomHeader {
            magic: Box::new([0; 4]),
            sram: 0,
            prg_rom_size: 0,
            chr_rom_size: 0,
            flags_6: 0,
            flags_7: 0,
            prg_ram_size: 8192,
            chr_ram_size: 0,
            flags_9: 0,
            flags_10: 0,
            zero: Box::new([0; 5])
        }
    }
}

#[derive(Debug)]
pub struct Cartridge {
    pub header: Box<RomHeader>, // iNES Header
    pub prg: Vec<u8>,           // A copy of the games program rom? Why? Is this for mirroring?
    pub chr: Vec<u8>,           // Copy of the games pattern table ROM or RAM for save states.
    pub rom: Vec<u8>,           // Temporary copy of ROM contents
    pub mapper_id: u8           // Mapper ID

}

impl Cartridge {
    pub fn new() -> Cartridge {

        Cartridge {
            header: Box::<RomHeader>::default(),
            prg: vec![0; 2 * 16000],
            chr: vec![0; 2 * 8000],
            rom: vec![0; 0x85_000],
            mapper_id: 0,
        }
    }
    // Returns the mapper ID (for mapper identification)
    pub fn retrieve_mapper_id(&self) -> u8 {
        (self.header.flags_7 & 0xf0)| (self.header.flags_6 >> 4)
    }
    pub fn read_rom(&mut self, file: &str) {
        let path = Path::new(file);
        let mut f = File::open(&path).expect("Couldn't find ROM");
        let mut buf = Vec::new();


        f.read_to_end(&mut buf).expect("i/o error, could not read file to end");
        self.rom[..buf.len()].clone_from_slice(&buf[..]);

        println!("\nLoaded: {} Size(KB): {:?}", path.to_str().unwrap(),
                 (buf.len() as f64 * 0.0009765625) as u32);
    }
    // Validates parts of the iNES file format
    pub fn validate_header(&mut self, header_data: &str) -> bool {

        let path = Path::new(header_data);
        let mut file = File::open(&path).expect("Couldn't find ROM");
        // The iNES header is 16 bytes long
        let mut header = [0u8; 16];
        file.read_exact(&mut header);

        self.header.magic = Box::from([header[0], header[1], header[2], header[3]]);
        self.header.sram = 0;
        self.header.prg_rom_size = header[4];
        self.header.chr_rom_size = header[5];
        self.header.flags_6 = header[6];
        self.header.flags_7 = header[7];
        self.header.prg_ram_size = header[8];
        self.header.flags_9 = header[9];
        self.header.flags_10 = header[10];
        self.header.zero = Box::from([header[11], header[12], header[13], header[14], header[15]]);
        self.mapper_id = (header[7] & 0x0f) | (header[6] >> 4);

        // Print out the NES character identifier if found
        if self.header.magic.is_ascii() {
            let magic = str::from_utf8(&*self.header.magic).unwrap().trim_right_matches('');
            println!("ROM header: {}", magic);

            // Print bank sizes
            println!("PRG ROM {} 16KB Pages", self.header.prg_rom_size);
            println!("CHR ROM {}  8KB Pages", self.header.chr_rom_size);
            println!("PRG RAM {}  8KB Pages", self.header.prg_ram_size);

            // TODO Research mappers & add more
            let id = match self.mapper_id {
                0 => "NROM",
                1 => "MMC1",
                2 => "UxROM",
                _ => "Unknown mapper",
            };
            println!("Mapper ID {}         ", id);

            return true;
        } else {
            return false;
            panic!("ROM not identified as NES rom");
        }
    }

    // TODO Figure out how to load cart rom into memory banks
    pub fn load_cartridge(&mut self, file: &str) {

        if self.validate_header(file) {
            self.read_rom(file);
        } else {
            panic!("Could not validate iNES header");
        }
        let prg_size = self.header.prg_rom_size as usize * PRG_ROM_BANK_SIZE;
        let chr_size = self.header.chr_rom_size as usize * CHR_ROM_BANK_SIZE;
        let mut prg = vec![0u8; prg_size].to_vec();
        let mut chr = vec![0u8; chr_size].to_vec();
        self.prg = prg;
        self.chr = chr;

    }
}

