
use std::ops::{Range, RangeTo, Index};
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

#[derive(Debug)]
pub struct RomHeader {
    pub magic: Box<[u8; 4]>,         // 0 - 3 "Magic" expects "NES" + 0x1a
    sram: u8,
    pub prg_rom_size: u8,
    pub chr_rom_size: u8,
    pub flags_6: u8,
    flags_7: u8,
    pub prg_ram_size: u8,
    pub chr_ram_size: u8,
    flags_9: u8,
    flags_10: u8,
    pub zero: Box<[u8; 5]>,          // Zero padding
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
    pub header: Box<RomHeader>, // 16 Bytes of Header data
    pub prg: Vec<u8>,           // A copy of the games program rom? Why? Is this for mirroring?
    pub chr: Vec<u8>,           // Copy of the games pattern table ROM or RAM for save states.
    pub rom: Vec<u8>,           // Temporary copy of ROM contents

}

impl Cartridge {
    pub fn new() -> Cartridge {

        Cartridge {
            header: Box::<RomHeader>::default(),
            prg: Vec::<u8>::new(),
            chr: Vec::<u8>::new(),
            rom: vec![0; 0x85_000],
        }
    }
    pub fn read_rom(&mut self, file: &str) {
        // TODO Validate Header
        let path = Path::new(file);
        let mut f = File::open(&path).expect("Couldn't find ROM");
        let mut buf = Vec::new();


        f.read_to_end(&mut buf);
        if buf.len() < 16 {
            panic!("Unable to read ROM header. {}");
        }

        self.rom[..buf.len()].clone_from_slice(&buf[..]);

        println!("Loaded: {}, Size(KB): {:?}", path.to_str().unwrap(),
                 (buf.len() as f64 * 0.0009765625) as u32);
    }
    pub fn parse_header(&mut self) {
        let rom = &self.rom;

        self.header.magic = Box::from([rom[0], rom[1], rom[2], rom[3]]);
        self.header.sram = 0;
        self.header.prg_rom_size = rom[4];
        self.header.chr_rom_size = rom[5];
        self.header.flags_6 = rom[6];
        self.header.flags_7 = rom[7];
        self.header.prg_ram_size = rom[8];

        self.header.flags_9 = rom[9];
        self.header.flags_10 = rom[10];
        self.header.zero = Box::from([rom[11], rom[12], rom[13], rom[14], rom[15]]);


        if self.header.magic.is_ascii() {
            let magic = str::from_utf8(&*self.header.magic).unwrap().trim_right_matches('');
            println!("ROM header: {}", magic);
        }
    }
    // TODO Figure out how to load cart rom into memory banks
    pub fn load_cartridge(&mut self, file: &str) {
        self.read_rom(&file);

        let rom = &self.rom;

        // Parse header data
        self.header.magic = Box::from([rom[0], rom[1], rom[2], rom[3]]);
        self.header.sram = 0;
        self.header.prg_rom_size = rom[4];
        self.header.chr_rom_size = rom[5];
        self.header.flags_6 = rom[6];
        self.header.flags_7 = rom[7];
        self.header.prg_ram_size = rom[8];

        self.header.flags_9 = rom[9];
        self.header.flags_10 = rom[10];
        self.header.zero = Box::from([rom[11], rom[12], rom[13], rom[14], rom[15]]);


        if self.header.magic.is_ascii() {
            let magic = str::from_utf8(&*self.header.magic).unwrap().trim_right_matches('');
            println!("ROM header: {}", magic);
        }

        println!("PRG ROM {} 16KB Pages", self.header.prg_rom_size);
        println!("CHR ROM {}  8KB Pages", self.header.chr_rom_size);
        println!("PRG RAM {}  8KB Pages", self.header.prg_ram_size);
        self.prg = [rom[4] * 16384].to_vec();
        self.chr = [rom[5] * 8192].to_vec();

        println!("Loaded, PRG {:?}, CHR {:?}", self.prg, self.chr);
    }
    pub fn read_into_memory(&mut self, mut memory: Ram) {
        let prg = &self.prg;
        memory.memory[..prg.len()].clone_from_slice(&prg[..]);

        // let prg = self.prg;

    }
}

