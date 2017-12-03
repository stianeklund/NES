#![feature(ascii_ctype)]
extern crate minifb;

mod rom;
mod interconnect;
mod opcode;
mod memory;
mod cpu;

use cpu::ExecutionContext;
use interconnect::Interconnect;

fn main() {

    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("[Please specify ROM as an argument]");
        return;
    }
    let file = &args[1];

    let mut exec = ExecutionContext::new();
    exec.cart.load_rom(&file);
    for _ in 0..10 {
        exec.decode();
        println!("{:?}", exec.cpu);
    }







}
