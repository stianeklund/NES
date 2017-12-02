#![feature(ascii_ctype)]
extern crate minifb;

mod memory;
mod cpu;
mod rom;
mod opcode;

use memory::Memory;
use rom::{Cartridge, RomHeader};
use cpu::{ExecutionContext, Cpu};
fn main() {

    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("[Please specify ROM as an argument]");
        return;
    }
    let file = &args[1];

    let mut memory = memory::Ram::new();
    let mut exec = ExecutionContext::new();
    exec.cart.load_cartridge(&file);
    exec.cart.read_into_memory(memory);
    for _ in 0..5 {
        exec.decode();
        println!("{:?}", exec.cpu);
    }







}
