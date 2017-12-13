#![feature(ascii_ctype)]
extern crate minifb;

mod rom;
mod interconnect;
mod opcode;
mod memory;
mod cpu;

use std::path::Path;
use std::io::Read;
use std::fs::File;
use interconnect::{Interconnect, MemoryHandler};
use cpu::ExecutionContext;

fn main() {

    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("[Please specify ROM as an argument]");
        return;
    }
    let file = &args[1];
    let path = Path::new(file);
    let mut f = File::open(&path).expect("Couldn't find ROM");


    // TODO Cleanup use interconnect instead
    // Make CPU more ergonomic to use.
    // let mut inter = Interconnect::new();
    // Interconnect could be handling the below instead of `ExecutionContext`

    let mut ctx = ExecutionContext::new();
    ctx.cart.load_rom(&mut f);

    // Get word at memory location 0xfffc and set PC value.
    ctx.cpu.reg.pc = ctx.read_word(0xfffc);
    for _ in 0..10 {
        ctx.decode();
    }
}
