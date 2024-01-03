use crate::arch::register::Flag::{C, H, Z};

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
enum Flag {
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
            program_counter: 0x0000, // start at 0x0000 if care about boot ram, else start at 0x0100
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

    pub fn set_zero_flag(&mut self, state: bool) {
        self.set_or_clear(state, Flag::Z)
    }

    pub fn set_half_carry_flag(&mut self, state: bool) {
        self.set_or_clear(state, Flag::H)
    }

    pub fn set_carry_flag(&mut self, state: bool) {
        self.set_or_clear(state, Flag::C)
    }

    pub fn set_subtract_flag(&mut self, state: bool) {
        self.set_or_clear(state, Flag::N)
    }

    pub fn get_zero_flag(&self) -> bool {
        self.get_flag_bit(Flag::Z)
    }
    pub fn get_half_carry_flag(&self) -> bool {
        self.get_flag_bit(Flag::H)
    }
    pub fn get_carry_flag(&self) -> bool {
        self.get_flag_bit(Flag::C)
    }
    pub fn get_subtract_flag(&self) -> bool {
        self.get_flag_bit(Flag::N)
    }

    fn set_or_clear(&mut self, state: bool, flag: Flag) {
        if state {
            self.set_flag_bit(flag)
        } else {
            self.clear_flag_bit(flag)
        }
    }

    fn set_flag_bit(&mut self, flag: Flag) {
        self.f |= flag as u8;
        self.f &= 0xF0;
    }

    fn clear_flag_bit(&mut self, flag: Flag) {
        let f = flag as u8;
        self.f &= !f;
        self.f &= 0xF0
    }

    fn get_flag_bit(&self, flag: Flag) -> bool {
        let f = flag as u8;
        (self.f & f) != 0
    }
}

#[cfg(test)]
mod register_tests {
    use crate::arch::register::Register;

    #[test]
    fn test_set_clear_flag_register() {
        let mut reg = Register::new();
        // this is important because of the initial state of the register
        reg.f = 0x00;

        reg.set_zero_flag(true);
        reg.set_subtract_flag(true);
        reg.set_half_carry_flag(true);

        assert_eq!(reg.get_carry_flag(), false);
        assert_eq!(reg.get_subtract_flag(), true);
        reg.set_subtract_flag(false);
        assert_eq!(reg.get_subtract_flag(), false);
    }
}