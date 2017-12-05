#![feature(ascii_ctype)]
extern crate minifb;

mod rom;
mod interconnect;
mod opcode;
mod memory;
mod cpu;

use interconnect::{Interconnect, MemoryHandler};
use cpu::ExecutionContext;
fn main() {

    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("[Please specify ROM as an argument]");
        return;
    }
    let file = &args[1];

    // TODO Cleanup use interconnect instead
    // Make CPU more ergonomic to use.
    // let mut inter = Interconnect::new();
    let mut cpu = ExecutionContext::new();

    cpu.cart.load_rom(&file);
    // Get word at memory location 0xfffc & set PC to that value
    cpu.setup_pc(0xfffc);

    for _ in 0..5 {
        cpu.decode();
    }
}
