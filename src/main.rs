extern crate minifb;

mod memory;

use memory::Memory;

fn main() {
    memory::Ram::new();
    println!("Hello, world!");
}
