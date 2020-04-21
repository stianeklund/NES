#![warn(clippy::pedantic)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::too_many_lines)]

use flexi_logger;
use minifb;

mod apu;
mod cpu;
mod interconnect;
mod memory;
mod opcode;
mod ppu;
mod rom;

use cpu::ExecutionContext;
use flexi_logger::{default_format, opt_format, LogTarget, Logger};
use interconnect::{Interconnect, MemoryMapper};
use log::{debug, error, info, warn};
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("[Please specify the ROM as an argument]");
        return;
    }
    let file = &args[1];
    let path = Path::new(file);
    let f = File::open(&path).expect("Unable to find ROM");

    // TODO Use logging for general purpose not debugging
    Logger::with_str("nes")
        .log_to_file()
        .directory("log")
        .format(default_format)
        .start()
        .unwrap();

    // TODO Cleanup use interconnect instead
    // Make CPU more ergonomic to use.
    // let mut inter = Interconnect::new();
    // Interconnect could be handling the below instead of `ExecutionContext`

    let mut ctx = ExecutionContext::new();
    ctx.reset();
    ctx.cart.load_rom(&f);

    // For debugging purposes
    // Get word at memory location 0xfffc and set PC value.
    // println!("Reset Vector: {:04x}", ctx.read16(0xfffc));
    // println!("NMI Vector:   {:04x}", ctx.read16(0xfffa));
    // println!("IRQ Vector:   {:04x}", ctx.read16(0xfffe));
    ctx.cpu.reg.pc = ctx.read16(0xfffc);
    // For nestest only
    ctx.cpu.reg.pc = 0xc000;

    let test_output = ctx.read8(0x6000);
    let err1 = ctx.read8(0x02);
    let err2 = ctx.read8(0x03);
    // eprintln!("{:x} {:x}", ctx.read8(0x02), ctx.read8(0x03));

    loop {
        let step: bool = false;
        if step {
            io::stdin().read_line(&mut String::new()).unwrap();
        }
        ctx.decode();
        if test_output != 0 {
            eprintln!("{:x}", test_output);
        }
        if err1 != 0 {
            eprintln!("{:x}", err1);
        }
        if err2 != 0 {
            eprintln!("{:x}", err2);
        }
    }
}
