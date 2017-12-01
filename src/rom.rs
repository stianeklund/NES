// iNes Header
pub struct RomHeader {
    nes: u8,
    prg_rom_size: u8,
    chr_rom_size: u8,
    flags_6: u8,
    flags_7: u8,
    pgr_ram_size: u8,
    chr_ram_size: u8,
    flags_9: u8,
    flags_10: u8,
    zero: u8,
}

// iNES header format
pub struct Rom {
    pub header: Vec<u8>,    // 16 Bytes of Header data
    pub trainer: Vec<u8>,   // Probably not necessary? Don't plan on supporting trainers anyhow.
    pub prg: Vec<u8>,       // A copy of the games program rom? Why? Is this for mirroring?
    pub chr: Vec<u8>,       // Copy of the games pattern table ROM or RAM for save states.

}

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