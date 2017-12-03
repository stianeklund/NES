use super::rom::Cartridge;
use super::cpu::Cpu;
use super::memory::{Ram, Mapper};

pub struct Interconnect {
    rom: Cartridge,
    cpu: Cpu,
    ram: Ram,
}
impl Interconnect {
    fn new() -> Box<Interconnect> {

        Box::new(Interconnect {
            rom: Cartridge::new(),
            cpu: Cpu::new(),
            ram: Ram::new(),
        })
    }
}
