#![warn(clippy::pedantic)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::too_many_lines)]

use minifb;
use flexi_logger;

mod rom;
mod interconnect;
mod opcode;
mod memory;
mod cpu;
mod ppu;
mod apu;

use std::path::Path;
use std::io::{self, Read};
use std::fs::File;
use interconnect::{Interconnect, MemoryMapper};
use cpu::ExecutionContext;
use flexi_logger::{Logger, LogTarget, opt_format};
use log::{info, warn, debug};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("[Please specify the ROM as an argument]");
        return;
    }
    let file = &args[1];
    let path = Path::new(file);
    let mut f = File::open(&path).expect("Unable to find ROM");

    // TODO Use logging for general purpose not debugging
    Logger::with_str("nes")
        .log_to_file()
        .directory("log")
        .format(opt_format)
        .start()
        .unwrap();

    // TODO Cleanup use interconnect instead
    // Make CPU more ergonomic to use.
    // let mut inter = Interconnect::new();
    // Interconnect could be handling the below instead of `ExecutionContext`

    let mut ctx = ExecutionContext::new();
    ctx.reset();
    ctx.cart.load_rom(&mut f);

    // For debugging purposes
    // Get word at memory location 0xfffc and set PC value.
    // Note reads add to the cycle counter..
    // println!("Reset Vector: {:04x}", ctx.read16(0xfffc));
    // println!("NMI Vector:   {:04x}", ctx.read16(0xfffa));
    // println!("IRQ Vector:   {:04x}", ctx.read16(0xfffe));
    ctx.cpu.reg.pc = ctx.read16(0xfffc);
    // For nestest only
    ctx.cpu.reg.pc = 0xc000;

    let err = ctx.read8(0x0002);
    let err1 = ctx.read8(0x0003);
    // Step one instruction at a time
    loop {
        let step: bool = false;
        if step {
            io::stdin().read_line(&mut String::new()).unwrap();
            ctx.decode();
        } else {
            ctx.decode();
        }
        if err | err1 != 0 {
            eprintln!("{:02x}, {:02x}", err, err1);
        }
    }
}

