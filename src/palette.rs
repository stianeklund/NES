
pub(crate) static PALETTE: [u8; 192] = [
    124, 124, 124, 0, 0, 252, 0, 0, 188, 68, 40, 188, 148, 0, 132, 168, 0, 32, 168, 16, 0, 136, 20,
    0, 80, 48, 0, 0, 120, 0, 0, 104, 0, 0, 88, 0, 0, 64, 88, 0, 0, 0, 0, 0, 0, 0, 0, 0, 188, 188,
    188, 0, 120, 248, 0, 88, 248, 104, 68, 252, 216, 0, 204, 228, 0, 88, 248, 56, 0, 228, 92, 16,
    172, 124, 0, 0, 184, 0, 0, 168, 0, 0, 168, 68, 0, 136, 136, 0, 0, 0, 0, 0, 0, 0, 0, 0, 248,
    248, 248, 60, 188, 252, 104, 136, 252, 152, 120, 248, 248, 120, 248, 248, 88, 152, 248, 120,
    88, 252, 160, 68, 248, 184, 0, 184, 248, 24, 88, 216, 84, 88, 248, 152, 0, 232, 216, 120, 120,
    120, 0, 0, 0, 0, 0, 0, 252, 252, 252, 164, 228, 252, 184, 184, 248, 216, 184, 248, 248, 184,
    248, 248, 164, 192, 240, 208, 176, 252, 224, 168, 248, 216, 120, 216, 248, 120, 184, 248, 184,
    184, 248, 216, 0, 252, 252, 248, 216, 248, 0, 0, 0, 0, 0, 0,
];
//  NES color palette
pub const PALETTE_U32: [u32;64] = [
    0x7C7C7C, 0x0000FC, 0x0000BC, 0x4428BC, 0x940084, 0xA80020, 0xA81000, 0x881400, 0x503000,
    0x007800, 0x006800, 0x005800, 0x004058, 0x000000, 0x000000, 0x000000, 0xBCBCBC, 0x0078F8,
    0x0058F8, 0x6844FC, 0xD800CC, 0xE40058, 0xF83800, 0xE45C10, 0xAC7C00, 0x00B800, 0x00A800,
    0x00A844, 0x008888, 0x000000, 0x000000, 0x000000, 0xF8F8F8, 0x3CBCFC, 0x6888FC, 0x9878F8,
    0xF878F8, 0xF85898, 0xF87858, 0xFCA044, 0xF8B800, 0xB8F818, 0x58D854, 0x58F898, 0x00E8D8,
    0x787878, 0x000000, 0x000000, 0xFCFCFC, 0xA4E4FC, 0xB8B8F8, 0xD8B8F8, 0xF8B8F8, 0xF8A4C0,
    0xF0D0B0, 0xFCE0A8, 0xF8D878, 0xD8F878, 0xB8F8B8, 0xB8F8D8, 0x00FCFC, 0xF8D8F8, 0x000000,
    0x000000 ];

pub fn palette_u32(index: usize) -> u32 {
    // PALETTE_U32[index as usize]
    ((PALETTE_U32[index as usize] as u32) << 16 | // Red
        (PALETTE_U32[index as usize] as u32) << 8 | // Green
        (PALETTE_U32[index as usize] as u32)) // Blue*/
}
pub fn palette(index: u8) -> u32 {
    // (0xff00_0000 as u32) |
        ((PALETTE[index as usize] as u32) << 16) | // Red
        ((PALETTE[index as usize] as u32) << 8) | // Green
        (PALETTE[index as usize] as u32) // Blue
}

//  u32 testing
pub const TEST_PALETTE:[u32; 6] = [
    0xff00_0000,   // Alpha
    0xff00_ff00,  // Green
    0xff00_ff00,  // Green
    0xff00_00ff,  // Red
    0xff00_00ff,  // Red
    0xffff_ffff,  // Black
];

