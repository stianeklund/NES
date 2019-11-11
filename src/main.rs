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
mod ppu_registers;

use std::path::Path;
use std::io::{self, Read};
use std::fs::File;
use interconnect::{Interconnect, MemoryMapper};
use cpu::ExecutionContext;
use opcode::Instruction;
use flexi_logger::{Logger, LogTarget, opt_format, default_format};
use log::{info, error, warn, debug};
use std::borrow::{BorrowMut, Borrow};

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

    debug!("Loaded rom:{:?}", path.file_name().unwrap());

    let mut ctx = ExecutionContext::new();
    ctx.reset_cpu();
    ctx.ppu.get_mut().reset_ppu();
    ctx.cart.load_rom(&f);

    // For debugging purposes
    // Get word at memory location 0xfffc and set PC value.
    println!("Reset Vector: {:04x}", ctx.read16(0xfffc));
    println!("NMI Vector:   {:04x}", ctx.read16(0xfffa));
    println!("IRQ Vector:   {:04x}", ctx.read16(0xfffe));
    ctx.cpu.reg.pc = ctx.read16(0xfffc);

    // For nestest only
    // ctx.cpu.reg.pc = 0xc000;
    // let err1 = ctx.read8(0x02);
    // let err2 = ctx.read8(0x03);

    loop {
        let step: bool = false;
        if step {
            io::stdin().read_line(&mut String::new()).unwrap();
        }

        // run(ctx.borrow_mut());
        log(ctx.borrow());
        ctx.decode();

    }
}

fn run(ctx: &mut ExecutionContext) {
    let mut cycle =0;
    if ctx.ppu.borrow_mut().nmi_occurred {
            eprintln!("NMI enabled");
        }
        if ctx.ppu.borrow().nmi_occurred {
            ctx.ppu.borrow_mut().vblank = false;
            ctx.ppu.borrow_mut().nmi_occurred = false;
            ctx.nmi();
            eprintln!("Executing NMI, turning vblank & NMI flags off");
        }
        ctx.ppu.borrow_mut().step();
    if cycle % 3 == 0 {
        ctx.decode();
    }
    cycle += 1;
}
fn log(ctx: &ExecutionContext) {
    info!("{:04X}  {:02X} {:02X}     {} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{} CYC:{}",
          ctx.cpu.reg.pc, ctx.read8(ctx.cpu.reg.pc), // Addr
          ctx.read8(ctx.cpu.reg.pc + 1),  // Operand
          Instruction::short_mnemonic(ctx.read8(ctx.cpu.reg.pc)),
          ctx.cpu.reg.a, ctx.cpu.reg.x, ctx.cpu.reg.y,
          ctx.cpu.p, ctx.cpu.reg.sp, ctx.ppu.borrow().cycle, ctx.cpu.cycles);
}
