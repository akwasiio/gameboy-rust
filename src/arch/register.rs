use crate::arch::register::Flags::{C, H, N, Z};

#[derive(Copy, Clone)]
pub struct Register {
    pub a: u8,
    f: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub stack_pointer: u16,
    pub program_counter: u16,
}

#[derive(Copy, Clone)]
enum Flags {
    // zero flag
    Z = 0b1000_0000,
    // subtraction flag
    N = 0b0100_0000,
    // half carry flag
    H = 0b0010_0000,
    // carry flag
    C = 0b0001_0000,
}

impl Register {
    pub fn new() -> Self {
        // https://gbdev.io/pandocs/Power_Up_Sequence.html#cpu-registers
        Self {
            a: 0x01,
            f: Z as u8 | H as u8 | C as u8,
            b: 0x00,
            c: 0x13,
            d: 0x00,
            e: 0xD8,
            h: 0x01,
            l: 0x4D,
            program_counter: 0x0100,
            stack_pointer: 0xFFFE,
        }
    }

    pub fn set_af(&mut self, nn: u16) {
        self.a = ((nn & 0xFF00) >> 8) as u8;
        self.f = (nn & 0x00F0) as u8; // F0 cos lower bits are always zero
    }

    pub fn get_af(&self) -> u16 {
        u16::from(self.a) << 8 | u16::from(self.f & 0xF0)
    }

    pub fn set_bc(&mut self, nn: u16) {
        self.b = ((nn & 0xFF00) >> 8) as u8;
        self.c = (nn & 0x00FF) as u8
    }

    pub fn get_bc(&self) -> u16 {
        u16::from(self.b) << 8 | u16::from(self.c)
    }

    pub fn set_de(&mut self, nn: u16) {
        self.d = (nn & 0xFF00 >> 8) as u8;
        self.e = (nn & 0x00FF) as u8
    }

    pub fn get_de(&self) -> u16 {
        u16::from(self.d) << 8 | u16::from(self.e)
    }

    pub fn set_hl(&mut self, nn: u16) {
        self.h = (nn & 0xFF00 >> 8) as u8;
        self.l = (nn & 0x00FF) as u8;
    }

    pub fn get_hl(&self) -> u16 {
        u16::from(self.h) << 8 | u16::from(self.l)
    }
}