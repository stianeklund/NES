#![feature(ascii_ctype)]
extern crate minifb;

mod memory;
mod cpu;
mod rom;
mod opcode;

use memory::{Memory, Ram};
use rom::{Cartridge, RomHeader};
use cpu::{ExecutionContext, Cpu};
fn main() {

    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("[Please specify ROM as an argument]");
        return;
    }
    let file = &args[1];

    let mut exec = ExecutionContext::new();
    let mut memory = Ram::new();
    exec.cart.load_cartridge(&file);
    for _ in 0..5 {
        exec.decode();
        println!("{:?}", exec.cpu);
    }







}
