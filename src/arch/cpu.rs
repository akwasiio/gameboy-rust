use crate::arch::memory::Memory;
use crate::arch::register::Register;

type ClockCycle = u16;

pub struct Cpu {
    // accumulator and flags register
    registers: Register,
    memory: Memory,
}


impl Cpu {
    pub fn new(boot_rom: Vec<u8>) -> Self {
        let mut memory = Memory::new();
        memory.load_boot_rom(boot_rom);

        Cpu {
            registers: Register::new(),
            memory,
        }
    }

    pub fn run_cycle(&mut self) {}
}

impl Cpu {
    fn get_byte(&mut self) -> u8 {
        let byte = self.memory.read_byte(self.registers.program_counter);
        self.registers.program_counter = self.registers.program_counter.wrapping_add(1);
        return byte;
    }

    fn get_word(&mut self) -> u16 {
        let word = self.memory.read_word(self.registers.program_counter);
        self.registers.program_counter = self.registers.program_counter.wrapping_add(2);
        return word;
    }

    fn set_inc_half_carry(&mut self, a: u8, b: u8) {
        let res = (((a & 0xF) + (b & 0xF)) & 0x10) == 0x10;
        self.registers.set_half_carry_flag(res);
    }

    fn set_dec_half_carry(&mut self, a: u8, b: u8) {
        let res = (((b & 0xF) - (a & 0xF)) & 0x10) == 0x10;
        self.registers.set_half_carry_flag(res)
    }

    fn set_inc_half_carry16(&mut self, a: u16, b: u16) {
        let res = ((a & 0x0FFF) + (b & 0x0FFF)) & 0x1000 == 0x1000;
        self.registers.set_half_carry_flag(res)
    }

    fn set_dec_half_carry16(&mut self, a: u16, b: u16) {
        let res = ((a & 0x0FFF) - (b & 0x0FFF)) & 0x1000 == 0x1000;
        self.registers.set_half_carry_flag(res)
    }

    /// Helper function to increment register
    fn increment(&mut self, n: u8) -> u8 {
        let a = 1;
        let res = n.wrapping_add(a);
        self.registers.set_zero_flag(res == 0x00);
        /// https://robdor.com/2016/08/10/gameboy-emulator-half-carry-flag/
        self.set_inc_half_carry(a, n);
        self.registers.set_subtract_flag(false);

        res
    }

    /// Helper function to decrement register
    fn decrement(&mut self, n: u8) -> u8 {
        let a = 1;
        let res = n.wrapping_sub(a);

        self.set_dec_half_carry(n, a);
        self.registers.set_zero_flag(res == 0x00);
        self.registers.set_subtract_flag(true);

        res
    }

    fn jump_relative(&mut self, cond: bool) -> ClockCycle {
        if cond {
            let imm8 = self.get_byte();
            self.registers.program_counter = self.registers.program_counter + u16::from(imm8);
            3
        } else {
            self.registers.program_counter += 1;
            2
        }
    }

    fn handle_op_codes(&mut self, opcode: u8) -> ClockCycle {
        match opcode {
            0x00 => { 1 }
            0x01 => {
                // LD BC d16
                // read word
                let word = self.get_word();
                self.registers.set_bc(word);
                3
            }
            0x02 => {
                // LD (BC), A
                println!("Store the contents of register A in the memory location specified by register pair BC.");
                self.memory.write_byte(self.registers.a, self.registers.get_bc());
                // self.registers.set_bc(self.registers.a as u16);
                1
            }

            0x03 => {
                // INC BC
                self.registers.set_bc(self.registers.get_bc() + 1);
                1
            }
            0x04 => {
                // INC B
                self.registers.b = self.increment(self.registers.b);
                1
            }
            0x05 => {
                // DEC B
                self.registers.b = self.decrement(self.registers.b);
                1
            }
            0x06 => {
                // Load the 8-bit immediate operand d8 into register B.
                let d8 = self.get_byte();
                self.registers.b = d8;
                2
            }
            0x07 => {
                // RLC A, carry flag is set depending on old 7th bit
                let carry = ((self.registers.a & 0x80) >> 7) == 0x01;

                self.registers.a = self.registers.a << 1 | if carry { 0x01 } else { 0 };
                self.registers.set_carry_flag(carry);
                self.registers.set_zero_flag(self.registers.a == 0);
                self.registers.set_subtract_flag(false);
                self.registers.set_half_carry_flag(false);

                1
            }
            0x08 => {
                // LD (d16), SP
                let d16 = self.get_word();
                self.memory.write_word(self.registers.stack_pointer, d16);
                5
            }
            0x09 => {
                // ADD HL, BC
                let hl = self.registers.get_hl();
                let bc = self.registers.get_bc();

                let (res, carry) = hl.overflowing_add(bc);

                self.registers.set_hl(res);
                self.registers.set_subtract_flag(false);

                self.registers.set_carry_flag(carry);
                self.set_inc_half_carry16(hl, bc);
                2
            }
            0x0A => {
                // LD A, (BC)
                self.registers.a = self.memory.read_byte(self.registers.get_bc());

                2
            }
            0x0B => {
                // DEC BC
                self.registers.set_bc(self.registers.get_bc().wrapping_sub(1));
                2
            }
            0x0C => {
                // INC C
                self.registers.c = self.increment(self.registers.c);
                1
            }
            0x0D => {
                // DEC C
                self.registers.c = self.decrement(self.registers.c);
                1
            }
            0x0E => {
                // LD C, imm8
                self.registers.c = self.get_byte();
                2
            }
            0x0F => {
                // RRCA: Rotate A right. Old 0 bit to carry flag and bit 7 of A
                let a = self.registers.a;
                let carry = a & 0x01 == 0x01;
                let res = a >> 1 | if carry { 0x80 } else { 0 };

                self.registers.a = res;
                self.registers.set_zero_flag(res == 0);
                self.registers.set_carry_flag(carry);
                self.registers.set_half_carry_flag(false);
                self.registers.set_subtract_flag(false);
                1
            }
            0x10 => {
                // STOP
                1
            }
            0x11 => {
                // LD DE, imm16
                self.registers.set_de(self.get_word());
                3
            }
            0x12 => {
                // LD (DE), A
                self.memory.write_byte(self.registers.a, self.registers.get_de());
                2
            }
            0x13 => {
                // INC DE
                self.registers.set_de(self.registers.get_de().wrapping_add(1));
                2
            }
            0x14 => {
                // INC D
                self.registers.d = self.increment(self.registers.d);

                1
            }
            0x15 => {
                // DEC D
                self.registers.d = self.decrement(self.registers.d);
                1
            }
            0x16 => {
                // LD D, imm8
                self.registers.d = self.get_byte();
                2
            }
            0x17 => {
                // RLA
                let res = self.registers.a << 1 | if self.registers.get_carry_flag() { 0x01 } else { 0 };
                let carry = self.registers.a >> 7 & 0x01 == 0x01;
                self.registers.a = res;
                self.registers.set_zero_flag(res == 0);
                self.registers.set_carry_flag(carry);
                self.registers.set_half_carry_flag(false);
                self.registers.set_subtract_flag(false);

                1
            }
            0x18 => {
                // JR s8
                let imm8 = self.get_byte();
                self.registers.program_counter = self.registers.program_counter + u16::from(imm8);
                3
            }
            0x19 => {
                // ADD HL, DE
                let de = self.registers.get_de();
                let hl = self.registers.get_hl();

                let (res, carry) = hl.overflowing_add(de);

                self.registers.set_subtract_flag(false);
                self.set_inc_half_carry16(hl, de);
                self.registers.set_carry_flag(carry);

                self.registers.set_hl(res);
                2
            }
            0x1A => {
                // LD A, (DE)
                self.registers.a = self.memory.read_byte(self.registers.get_de());
                2
            }
            0x1B => {
                // DEC DE
                self.registers.set_de(self.registers.get_de().wrapping_sub(1));
                2
            }
            0x1C => {
                // INC E
                self.registers.e = self.increment(self.registers.e);
                1
            }
            0x1D => {
                // DEC E
                self.registers.e = self.decrement(self.registers.e);
                1
            }
            0x1E => {
                // LD E, imm8
                self.registers.e = self.get_byte();
                2
            }
            0x1F => {
                // RRA
                let carry = self.registers.a >> 7 & 0x01 == 0x01;
                self.registers.a = self.registers.a >> 1 | if self.registers.get_carry_flag() { 0x80 } else { 0 };
                self.registers.set_zero_flag(false);
                self.registers.set_subtract_flag(false);
                self.registers.set_half_carry_flag(false);
                self.registers.set_carry_flag(carry);

                1
            }
            0x20 => {
                // JR NZ, imm8
                self.jump_relative(!self.registers.get_zero_flag())
            }
            0x21 => {
                // LD HL, imm16
                self.registers.set_hl(self.get_word());
                3
            }
            0x22 => {
                // LD (HL+), A
                let hl = self.registers.get_hl();
                self.memory.write_byte(self.registers.a, hl);
                self.registers.set_hl(hl + 1);
                2
            }
            0x23 => {
                // INC HL
                self.registers.set_hl(self.registers.get_hl().wrapping_add(1));
                2
            }
            0x24 => {
                // INC H
                self.registers.h = self.increment(self.registers.h);

                1
            }
            0x25 => {
                // DEC H
                self.registers.h = self.decrement(self.registers.h);

                1
            }
            0x26 => {
                // LD H, imm8
                self.registers.h = self.get_byte();
                2
            }
            0x27 => {
                // DAA
                unimplemented!("Yet to understand how this works");
                1
            }
            0x28 => {
                // JR Z, imm8
                self.jump_relative(self.registers.get_zero_flag())
            }
            0x29 => {
                // ADD HL, HL
                let hl = self.registers.get_hl();
                let (res, carry) = hl.overflowing_add(hl);
                self.registers.set_subtract_flag(false);
                self.set_inc_half_carry16(hl, hl);
                self.registers.set_carry_flag(carry);
                self.registers.set_hl(res);
                2
            }
            0x2A => {
                // LD A, (HL+)
                let hl = self.registers.get_hl();
                self.registers.a = self.memory.read_byte(hl);
                self.registers.set_hl(hl + 1);
                2
            }
            0x2B => {
                // DEC HL
                self.registers.set_hl(self.registers.get_hl().wrapping_sub(1));

                2
            }
            0x2C => {
                // INC L
                self.registers.l = self.increment(self.registers.l);

                1
            }
            0x2D => {
                // DEC L
                self.registers.l = self.decrement(self.registers.l);
                1
            }
            0x2E => {
                // LD L, imm8
                self.registers.l = self.get_byte();
                2
            }
            0x2F => {
                // CPL - One's complement of register A
                self.registers.a = !self.registers.a;
                self.registers.set_half_carry_flag(true);
                self.registers.set_subtract_flag(true);
                1
            }
            0x30 => {
                // JR NC, imm8
                self.jump_relative(!self.registers.get_carry_flag())
            }
            0x31 => {
                // LD SP, imm16
                self.registers.stack_pointer = self.get_word();
                3
            }
            0x32 => {
                // LD (HL-), A
                let hl = self.registers.get_hl();
                self.memory.write_byte(self.registers.a, hl);
                self.registers.set_hl(hl.wrapping_sub(1));
                2
            }
            0x33 => {}
            0x34 => {}
            0x35 => {}
            0x36 => {

            }
            0x37 => {}
            0x38 => {}
            0x39 => {}
            0x3A => {}
            0x3B => {}
            0x3C => {}
            0x3D => {}
            0x3E => {}
            0x3F => {}
            0x40 => {}
            0x41 => {}
            0x42 => {}
            0x43 => {}
            0x44 => {}
            0x45 => {}
            0x46 => {}
            0x47 => {}
            0x48 => {}
            0x49 => {}
            0x4A => {}
            0x4B => {}
            0x4C => {}
            0x4D => {}
            0x4E => {}
            0x4F => {}
            0x50 => {}
            0x51 => {}
            0x52 => {}
            0x53 => {}
            0x54 => {}
            0x55 => {}
            0x56 => {}
            0x57 => {}
            0x58 => {}
            0x59 => {}
            0x5A => {}
            0x5B => {}
            0x5C => {}
            0x5D => {}
            0x5E => {}
            0x5F => {}
            0x60 => {}
            0x61 => {}
            0x62 => {}
            0x63 => {}
            0x64 => {}
            0x65 => {}
            0x66 => {}
            0x67 => {}
            0x68 => {}
            0x69 => {}
            0x6A => {}
            0x6B => {}
            0x6C => {}
            0x6D => {}
            0x6E => {}
            0x6F => {}
            0x70 => {}
            0x71 => {}
            0x72 => {}
            0x73 => {}
            0x74 => {}
            0x75 => {}
            0x76 => {}
            0x77 => {}
            0x78 => {}
            0x79 => {}
            0x7A => {}
            0x7B => {}
            0x7C => {}
            0x7D => {}
            0x7E => {}
            0x7F => {}
            0x80 => {}
            0x81 => {}
            0x82 => {}
            0x83 => {}
            0x84 => {}
            0x85 => {}
            0x86 => {}
            0x87 => {}
            0x88 => {}
            0x89 => {}
            0x8A => {}
            0x8B => {}
            0x8C => {}
            0x8D => {}
            0x8E => {}
            0x8F => {}
            0x90 => {}
            0x91 => {}
            0x92 => {}
            0x93 => {}
            0x94 => {}
            0x95 => {}
            0x96 => {}
            0x97 => {}
            0x98 => {}
            0x99 => {}
            0x9A => {}
            0x9B => {}
            0x9C => {}
            0x9D => {}
            0x9E => {}
            0x9F => {}
            0xA0 => {}
            0xA1 => {}
            0xA2 => {}
            0xA3 => {}
            0xA4 => {}
            0xA5 => {}
            0xA6 => {}
            0xA7 => {}
            0xA8 => {}
            0xA9 => {}
            0xAA => {}
            0xAB => {}
            0xAC => {}
            0xAD => {}
            0xAE => {}
            0xAF => {}
            0xB0 => {}
            0xB1 => {}
            0xB2 => {}
            0xB3 => {}
            0xB4 => {}
            0xB5 => {}
            0xB6 => {}
            0xB7 => {}
            0xB8 => {}
            0xB9 => {}
            0xBA => {}
            0xBB => {}
            0xBC => {}
            0xBD => {}
            0xBE => {}
            0xBF => {}
            0xC0 => {}
            0xC1 => {}
            0xC2 => {}
            0xC3 => {}
            0xC4 => {}
            0xC5 => {}
            0xC6 => {}
            0xC7 => {}
            0xC8 => {}
            0xC9 => {}
            0xCA => {}
            0xCB => {}
            0xCC => {}
            0xCD => {}
            0xCE => {}
            0xCF => {}
            0xD0 => {}
            0xD1 => {}
            0xD2 => {}
            0xD3 => {}
            0xD4 => {}
            0xD5 => {}
            0xD6 => {}
            0xD7 => {}
            0xD8 => {}
            0xD9 => {}
            0xDA => {}
            0xDB => {}
            0xDC => {}
            0xDD => {}
            0xDE => {}
            0xDF => {}
            0xE0 => {}
            0xE1 => {}
            0xE2 => {}
            0xE3 => {}
            0xE4 => {}
            0xE5 => {}
            0xE6 => {}
            0xE7 => {}
            0xE8 => {}
            0xE9 => {}
            0xEA => {}
            0xEB => {}
            0xEC => {}
            0xED => {}
            0xEE => {}
            0xEF => {}
            0xF0 => {}
            0xF1 => {}
            0xF2 => {}
            0xF3 => {}
            0xF4 => {}
            0xF5 => {}
            0xF6 => {}
            0xF7 => {}
            0xF8 => {}
            0xF9 => {}
            0xFA => {}
            0xFB => {}
            0xFC => {}
            0xFD => {}
            0xFE => {}
            0xFF => {}
            _ => panic!("not implemented!")
        }
    }
}