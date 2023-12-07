use crate::arch::register::Register;

struct Cpu {
    // accumulator and flags register
    registers: Register,
}


impl Cpu {
    pub fn new() -> Self {
        Cpu {
            registers: Register::new(),
        }
    }
}