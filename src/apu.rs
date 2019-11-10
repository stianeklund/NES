use crate::interconnect::MemoryMapper;

// Audio won't be implemented anytime soon..
// This is just to support rudimentary r/w APU & I/O data.

#[derive(Default, Debug)]
pub struct Apu {
    pub mem: Vec<u8>,
    pub reg: Registers,
}


// APU registers
#[derive(Default, Debug)]
pub struct Registers {
    pub pulse_1: u8,
    pub pulse_2: u8,
    pub triangle: u8,
    pub noise: u8,
    pub dmc: u8,
    pub control: u8,
    pub status: u8,
    pub frame_counter: u8,
    pub pycles: u8,
}

impl MemoryMapper for Apu {
    fn read8(&self, addr: u16) -> u8 {
        match addr {
            0x4000 ..= 0x4003 => self.reg.pulse_1,
            0x4004 ..= 0x4007 => self.reg.pulse_2,
            0x4008 ..= 0x400b => self.reg.triangle,
            0x400c ..= 0x400f => self.reg.noise,
            0x4010 ..= 0x4013 => self.reg.dmc,
            0x4015 => self.reg.control,
            0x4017 => self.reg.frame_counter,
            _ => unimplemented!("APU read to: {:04x} not implemented", addr)
        }
    }
    fn write8(&mut self, addr: u16, byte: u8) {
        match addr {
            0x4000 ..= 0x4003 => self.reg.pulse_1 = byte,
            0x4004 ..= 0x4007 => self.reg.pulse_2 = byte,
            0x4008 ..= 0x400b => self.reg.triangle = byte,
            0x400c ..= 0x400f => self.reg.noise = byte,
            0x4010 ..= 0x4013 => self.reg.dmc = byte,
            0x4015 => self.reg.control = byte,
            0x4017 => self.reg.frame_counter = byte,
            _ => unimplemented!("APU write to: {:04x} not implemented", addr)
        };
    }
}
