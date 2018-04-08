#![feature(ascii_ctype)]
#![feature(nll)]

extern crate minifb;

mod rom;
mod interconnect;
mod opcode;
mod memory;
mod cpu;
mod ppu;

use std::path::Path;
use std::io::{self, Read};
use std::fs::File;
use interconnect::{Interconnect, MemoryMapper};
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
    println!("Reset Vector: {:04x}", ctx.read_word(0xfffc));
    println!("NMI Vector:   {:04x}", ctx.read_word(0xfffa));
    println!("IRQ Vector:   {:04x}", ctx.read_word(0xfffe));
    ctx.cpu.reg.pc = ctx.read_word(0xfffc);
    // For nestest only
    ctx.cpu.reg.pc = 0xc000;

    // Step one instruction at a time
    loop {
        let step: bool = true;
        if step {
            io::stdin().read_line(&mut String::new()).unwrap();
            ctx.decode();
        }
    }
}

