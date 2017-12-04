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
    let pc = cpu.read(cpu.cart.get_prg_pc());
    println!("PC:{:04x}", pc);
    cpu.reg.pc = pc as u16; // cpu.cart.get_prg_pc();
    for _ in 0..5 {
        cpu.decode();
    }
}
