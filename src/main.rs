#![feature(ascii_ctype)]
extern crate minifb;

mod memory;
mod cpu;
mod rom;

use memory::Memory;
use rom::{Cartridge, RomHeader};
fn main() {

    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("[Please specify ROM as an argument]");
        return;
    }
    let file = &args[1];
    let mem = memory::Ram::new();
    let mut cart = Cartridge::new();

    cart.read_rom(file);
    cart.read_header();

    // println!("Header:{:?}\n", cart.header);
    println!("PRG ROM 16KB Pages {}", cart.header.prg_rom_size);
    println!("CHR ROM 8KB  Pages {}", cart.header.chr_rom_size);
    println!("PRG RAM 8KB  Pages {}", cart.header.prg_ram_size);

}
