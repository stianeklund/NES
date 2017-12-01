// TODO Use trait for easy r/w memory I/O functionality?
pub trait Memory {
   // TODO write memory read / write functions
}
// 2KB of on-board work RAM
pub struct Ram {
    pub value: Vec<u8>
}

impl Ram {
    pub fn new() -> Ram {
        Ram {
            value: vec![0; 0x800],
        }
    }

    fn unimplmented() {
        unimplemented!()
    }
}