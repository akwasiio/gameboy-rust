use crate::arch::memory::Memory;
use crate::arch::register::Register;

type ClockCycle = u16;

pub struct Cpu {
    // accumulator and flags register
    registers: Register,
    memory: Memory,
    is_halted: bool,
}

impl Cpu {
    pub fn new(boot_rom: Vec<u8>) -> Self {
        let mut memory = Memory::new();
        memory.load_boot_rom(boot_rom);

        Cpu {
            registers: Register::new(),
            memory,
            is_halted: false,
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
        // https://robdor.com/2016/08/10/gameboy-emulator-half-carry-flag/
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

    fn add8(&mut self, lhs: u8, rhs: u8, apply_carry: bool) -> u8 {
        let c = if apply_carry && self.registers.get_carry_flag() {
            1
        } else {
            0
        };

        let res = lhs.wrapping_add(rhs).wrapping_add(c);
        self.registers.set_zero_flag(res == 0x00);
        self.registers
            .set_half_carry_flag((lhs & 0x0F) + (rhs & 0x0F) + (c & 0x0F) > 0x0F);
        self.registers
            .set_carry_flag(u16::from(lhs) + u16::from(rhs) + u16::from(c) > 0xFF);
        self.registers.set_subtract_flag(false);
        res
    }

    fn add16(&mut self, lhs: u16, rhs: u16) -> u16 {
        let (res, carry) = lhs.overflowing_add(rhs);
        self.registers.set_zero_flag(false);
        self.registers.set_subtract_flag(false);
        self.registers.set_carry_flag(carry);
        self.set_inc_half_carry16(lhs, rhs);
        res
    }

    fn sub8(&mut self, lhs: u8, rhs: u8, apply_carry: bool) -> u8 {
        let c = if apply_carry && self.registers.get_carry_flag() {
            1
        } else {
            0
        };
        let res = lhs.wrapping_sub(rhs).wrapping_sub(c);
        self.registers.set_zero_flag(res == 0x00);
        self.registers
            .set_carry_flag(u16::from(c) < u16::from(lhs) + u16::from(c));
        self.registers
            .set_half_carry_flag((lhs & 0x0F) < (rhs & 0x0F) + c);
        self.registers.set_subtract_flag(true);
        res
    }

    fn and(&mut self, lhs: u8, rhs: u8) -> u8 {
        let res = lhs & rhs;
        self.registers.set_zero_flag(res == 0x00);
        self.registers.set_subtract_flag(false);
        self.registers.set_half_carry_flag(true);
        self.registers.set_carry_flag(false);
        res
    }

    fn or(&mut self, lhs: u8, rhs: u8) -> u8 {
        let res = lhs | rhs;
        self.registers.set_zero_flag(res == 0x00);
        self.registers.set_subtract_flag(false);
        self.registers.set_carry_flag(false);
        self.registers.set_half_carry_flag(false);
        res
    }

    fn xor(&mut self, lhs: u8, rhs: u8) -> u8 {
        let res = lhs ^ rhs;
        self.registers.set_zero_flag(res == 0x00);
        self.registers.set_subtract_flag(false);
        self.registers.set_carry_flag(false);
        self.registers.set_half_carry_flag(false);
        res
    }

    fn cp(&mut self, lhs: u8, rhs: u8) {
        self.sub8(lhs, rhs, false);
    }

    fn pop_stack(&mut self) -> u16 {
        let res = self.memory.read_word(self.registers.stack_pointer);
        self.registers.stack_pointer += 2;
        res
    }

    fn push_to_stack(&mut self, value: u16) {
        self.registers.stack_pointer -= 2;
        self.memory.write_word(value, self.registers.stack_pointer);
    }

    fn handle_op_codes(&mut self, opcode: u8) -> ClockCycle {
        match opcode {
            0x00 => 1,
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
                self.memory
                    .write_byte(self.registers.a, self.registers.get_bc());
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
                self.registers
                    .set_bc(self.registers.get_bc().wrapping_sub(1));
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
                self.memory
                    .write_byte(self.registers.a, self.registers.get_de());
                2
            }
            0x13 => {
                // INC DE
                self.registers
                    .set_de(self.registers.get_de().wrapping_add(1));
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
                let res = self.registers.a << 1
                    | if self.registers.get_carry_flag() {
                        0x01
                    } else {
                        0
                    };
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
                self.registers
                    .set_de(self.registers.get_de().wrapping_sub(1));
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
                self.registers.a = self.registers.a >> 1
                    | if self.registers.get_carry_flag() {
                        0x80
                    } else {
                        0
                    };
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
                self.registers
                    .set_hl(self.registers.get_hl().wrapping_add(1));
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
                todo!("Yet to understand how this works");
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
                self.registers
                    .set_hl(self.registers.get_hl().wrapping_sub(1));

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
            0x33 => {
                // INC SP
                self.registers.stack_pointer += 1;
                2
            }
            0x34 => {
                // INC (HL)
                let value = self.increment(self.memory.read_byte(self.registers.get_hl()));
                self.memory.write_byte(value, self.registers.get_hl());
                3
            }
            0x35 => {
                // DEC (HL)
                let hl = self.registers.get_hl();
                let value = self.decrement(self.memory.read_byte(hl));
                self.memory.write_byte(value, hl);
                3
            }
            0x36 => {
                // LD (HL), imm8
                self.memory
                    .write_byte(self.get_byte(), self.registers.get_hl());
                3
            }
            0x37 => {
                // SCF
                self.registers.set_carry_flag(true);
                self.registers.set_subtract_flag(false);
                self.registers.set_half_carry_flag(false);
                1
            }
            0x38 => {
                // JR C, imm8
                if self.registers.get_carry_flag() {
                    let imm8 = self.get_byte() as u16;
                    self.registers.program_counter =
                        self.registers.program_counter.wrapping_add(imm8);
                    3
                } else {
                    self.registers.program_counter += 2;
                    2
                }
            }
            0x39 => {
                // ADD HL, SP
                let hl = self.registers.get_hl();
                let res = self.add16(self.registers.stack_pointer, hl);
                self.registers.set_hl(res);
                2
            }
            0x3A => {
                // LD A, (HL-)
                let hl = self.registers.get_hl();
                self.registers.a = self.memory.read_byte(hl);
                self.registers.set_hl(hl.wrapping_sub(1));
                2
            }
            0x3B => {
                // DEC SP
                self.registers.stack_pointer = self.registers.stack_pointer.wrapping_sub(1);
                2
            }
            0x3C => {
                // INC A
                self.increment(self.registers.a);
                1
            }
            0x3D => {
                // DEC A
                self.decrement(self.registers.a);
                1
            }
            0x3E => {
                // LD A, d8
                self.registers.a = self.get_byte();
                2
            }
            0x3F => {
                // CCF
                self.registers
                    .set_carry_flag(!self.registers.get_carry_flag());
                self.registers.set_half_carry_flag(false);
                self.registers.set_subtract_flag(false);

                1
            }
            0x40 => {
                // LD B, B - what's the point?
                self.registers.b = self.registers.b;
                1
            }
            0x41 => {
                // LD B, C
                self.registers.b = self.registers.c;
                1
            }
            0x42 => {
                // LD B, D
                self.registers.b = self.registers.d;
                1
            }
            0x43 => {
                // LD B, E
                self.registers.b = self.registers.e;
                1
            }
            0x44 => {
                // LD B, H
                self.registers.b = self.registers.h;
                1
            }
            0x45 => {
                // LD B, L
                self.registers.b = self.registers.l;
                1
            }
            0x46 => {
                // LD B, (HL)
                self.registers.b = self.memory.read_byte(self.registers.get_hl());
                2
            }
            0x47 => {
                // LD B, A
                self.registers.b = self.registers.a;
                1
            }
            0x48 => {
                // LD C, B
                self.registers.c = self.registers.b;
                1
            }
            0x49 => {
                // LD C, C
                self.registers.c = self.registers.c;
                1
            }
            0x4A => {
                // LD C, D
                self.registers.c = self.registers.d;
                1
            }
            0x4B => {
                // LD C, E
                self.registers.c = self.registers.e;
                1
            }
            0x4C => {
                // LD C, H
                self.registers.c = self.registers.h;
                1
            }
            0x4D => {
                // LD C, L
                self.registers.c = self.registers.l;
                1
            }
            0x4E => {
                // LD C, (HL)
                self.registers.c = self.memory.read_byte(self.registers.get_hl());
                2
            }
            0x4F => {
                // LD C, A
                self.registers.c = self.registers.a;
                1
            }
            0x50 => {
                // LD D, B
                self.registers.d = self.registers.b;
                1
            }
            0x51 => {
                // LD D, C
                self.registers.d = self.registers.c;
                1
            }
            0x52 => {
                // LD D, D
                self.registers.d = self.registers.d;
                1
            }
            0x53 => {
                // LD D, E
                self.registers.d = self.registers.e;
                1
            }
            0x54 => {
                // LD D, H
                self.registers.d = self.registers.h;
                1
            }
            0x55 => {
                // LD D, L
                self.registers.d = self.registers.l;
                1
            }
            0x56 => {
                // LD D, (HL)
                self.registers.d = self.memory.read_byte(self.registers.get_hl());
                2
            }
            0x57 => {
                // LD D, A
                self.registers.d = self.registers.a;
                1
            }
            0x58 => {
                // LD E, B
                self.registers.e = self.registers.b;
                1
            }
            0x59 => {
                // LD E, C
                self.registers.e = self.registers.c;
                1
            }
            0x5A => {
                // LD E, D
                self.registers.e = self.registers.d;
                1
            }
            0x5B => {
                // LD E, E
                self.registers.e = self.registers.e;
                1
            }
            0x5C => {
                // LD E, H
                self.registers.e = self.registers.h;
                1
            }
            0x5D => {
                // LD E, L
                self.registers.e = self.registers.l;
                1
            }
            0x5E => {
                // LD E, (HL)
                self.registers.e = self.memory.read_byte(self.registers.get_hl());
                2
            }
            0x5F => {
                // LD E, A
                self.registers.e = self.registers.a;
                1
            }
            0x60 => {
                // LD H, B
                self.registers.h = self.registers.b;
                1
            }
            0x61 => {
                // LD H, C
                self.registers.h = self.registers.c;
                1
            }
            0x62 => {
                // LD H, D
                self.registers.h = self.registers.d;
                1
            }
            0x63 => {
                // LD H, E
                self.registers.h = self.registers.e;
                1
            }
            0x64 => {
                // LD H, H
                self.registers.h = self.registers.h;
                1
            }
            0x65 => {
                // LD H, L
                self.registers.h = self.registers.l;
                1
            }
            0x66 => {
                // LD H, (HL)
                self.registers.h = self.memory.read_byte(self.registers.get_hl());
                2
            }
            0x67 => {
                // LD H, A
                self.registers.h = self.registers.a;
                1
            }
            0x68 => {
                // LD L, B
                self.registers.l = self.registers.b;
                1
            }
            0x69 => {
                // LD L, C
                self.registers.l = self.registers.c;
                1
            }
            0x6A => {
                // LD L, D
                self.registers.l = self.registers.d;
                1
            }
            0x6B => {
                // LD L, E
                self.registers.l = self.registers.e;
                1
            }
            0x6C => {
                // LD L, H
                self.registers.l = self.registers.h;
                1
            }
            0x6D => {
                // LD L, L
                self.registers.l = self.registers.l;
                1
            }
            0x6E => {
                // LD L, (HL)
                self.registers.l = self.memory.read_byte(self.registers.get_hl());
                2
            }
            0x6F => {
                // LD L, A
                self.registers.l = self.registers.a;
                1
            }
            0x70 => {
                // LD (HL), B
                self.memory
                    .write_byte(self.registers.b, self.registers.get_hl());
                2
            }
            0x71 => {
                // LD (HL), C
                self.memory
                    .write_byte(self.registers.c, self.registers.get_hl());
                2
            }
            0x72 => {
                // LD (HL), D
                self.memory
                    .write_byte(self.registers.d, self.registers.get_hl());
                2
            }
            0x73 => {
                // LD (HL), E
                self.memory
                    .write_byte(self.registers.e, self.registers.get_hl());
                2
            }
            0x74 => {
                // LD (HL), H
                self.memory
                    .write_byte(self.registers.h, self.registers.get_hl());
                2
            }
            0x75 => {
                // LD (HL), L
                self.memory
                    .write_byte(self.registers.l, self.registers.get_hl());
                2
            }
            0x76 => {
                // HALT
                self.is_halted = true;
                1
            }
            0x77 => {
                // LD (HL), A
                self.memory
                    .write_byte(self.registers.a, self.registers.get_hl());
                2
            }
            0x78 => {
                // LD A, B
                self.registers.a = self.registers.b;
                1
            }
            0x79 => {
                // LD A, C
                self.registers.a = self.registers.c;
                1
            }
            0x7A => {
                // LD A, D
                self.registers.a = self.registers.d;
                1
            }
            0x7B => {
                // LD A, E
                self.registers.a = self.registers.e;
                1
            }
            0x7C => {
                // LD A, H
                self.registers.a = self.registers.h;
                1
            }
            0x7D => {
                // LD A, L
                self.registers.a = self.registers.l;
                1
            }
            0x7E => {
                // LD A, (HL)
                self.registers.a = self.memory.read_byte(self.registers.get_hl());
                2
            }
            0x7F => {
                // LD A, A
                self.registers.a = self.registers.a;
                1
            }
            0x80 => {
                // ADD A, B
                self.registers.a = self.add8(self.registers.a, self.registers.b, false);
                1
            }
            0x81 => {
                // ADD A, C
                self.registers.a = self.add8(self.registers.a, self.registers.b, false);
                1
            }
            0x82 => {
                // ADD A, D
                self.registers.a = self.add8(self.registers.a, self.registers.d, false);
                1
            }
            0x83 => {
                // ADD A, E
                self.registers.a = self.add8(self.registers.a, self.registers.e, false);
                1
            }
            0x84 => {
                // ADD A, H
                self.registers.a = self.add8(self.registers.a, self.registers.h, false);
                1
            }
            0x85 => {
                // ADD A, L
                self.registers.a = self.add8(self.registers.a, self.registers.l, false);
                1
            }
            0x86 => {
                // ADD A, (HL)
                self.registers.a = self.add8(
                    self.registers.a,
                    self.memory.read_byte(self.registers.get_hl()),
                    false,
                );
                2
            }
            0x87 => {
                // ADD A, A
                self.registers.a = self.add8(self.registers.a, self.registers.a, false);
                1
            }
            0x88 => {
                // ADC A, B
                self.registers.a = self.add8(self.registers.a, self.registers.b, true);
                1
            }
            0x89 => {
                // ADC A, C
                self.registers.a = self.add8(self.registers.a, self.registers.b, true);
                1
            }
            0x8A => {
                // ADC A, D
                self.registers.a = self.add8(self.registers.a, self.registers.d, true);
                1
            }
            0x8B => {
                // ADC A, E
                self.registers.a = self.add8(self.registers.a, self.registers.e, true);
                1
            }
            0x8C => {
                // ADC A, H
                self.registers.a = self.add8(self.registers.a, self.registers.h, true);
                1
            }
            0x8D => {
                // ADC A, L
                self.registers.a = self.add8(self.registers.a, self.registers.l, true);
                1
            }
            0x8E => {
                // ADC A, (HL)
                let byte = self.memory.read_byte(self.registers.get_hl());
                self.registers.a = self.add8(self.registers.a, byte, true);
                2
            }
            0x8F => {
                // ADC A, A
                self.registers.a = self.add8(self.registers.a, self.registers.a, true);
                1
            }
            0x90 => {
                // SUB A, B
                self.registers.a = self.sub8(self.registers.a, self.registers.b, false);
                1
            }
            0x91 => {
                // SUB A, C
                self.registers.a = self.sub8(self.registers.a, self.registers.c, false);
                1
            }
            0x92 => {
                // SUB A, D
                self.registers.a = self.sub8(self.registers.a, self.registers.d, false);
                1
            }
            0x93 => {
                // SUB A, E
                self.registers.a = self.sub8(self.registers.a, self.registers.e, false);
                1
            }
            0x94 => {
                // SUB A, H
                self.registers.a = self.sub8(self.registers.a, self.registers.h, false);
                1
            }
            0x95 => {
                // SUB A, L
                self.registers.a = self.sub8(self.registers.a, self.registers.l, false);
                1
            }
            0x96 => {
                // SUB A, (HL)
                self.registers.a = self.sub8(
                    self.registers.a,
                    self.memory.read_byte(self.registers.get_hl()),
                    false,
                );
                2
            }
            0x97 => {
                // SUB A, A
                self.registers.a = self.sub8(self.registers.a, self.registers.a, false);
                1
            }
            0x98 => {
                // SBC A, B
                self.registers.a = self.sub8(self.registers.a, self.registers.b, true);
                1
            }
            0x99 => {
                // SBC A, C
                self.registers.a = self.sub8(self.registers.a, self.registers.c, true);
                1
            }
            0x9A => {
                // SBC A, D
                self.registers.a = self.sub8(self.registers.a, self.registers.d, true);
                1
            }
            0x9B => {
                // SBC A, E
                self.registers.a = self.sub8(self.registers.a, self.registers.e, true);
                1
            }
            0x9C => {
                // SBC A, H
                self.registers.a = self.sub8(self.registers.a, self.registers.h, true);
                1
            }
            0x9D => {
                // SBC A, L
                self.registers.a = self.sub8(self.registers.a, self.registers.l, true);
                1
            }
            0x9E => {
                // SBC A, (HL)
                let byte = self.memory.read_byte(self.registers.get_hl());
                self.registers.a = self.sub8(self.registers.a, byte, true);
                1
            }
            0x9F => {
                // SBC A, A
                self.registers.a = self.sub8(self.registers.a, self.registers.a, true);
                1
            }
            0xA0 => {
                // AND A, B
                self.registers.a = self.and(self.registers.a, self.registers.b);
                1
            }
            0xA1 => {
                // AND A, C
                self.registers.a = self.and(self.registers.a, self.registers.c);
                1
            }
            0xA2 => {
                // AND A, D
                self.registers.a = self.and(self.registers.a, self.registers.d);
                1
            }
            0xA3 => {
                // AND A, E
                self.registers.a = self.and(self.registers.a, self.registers.e);
                1
            }
            0xA4 => {
                // AND A, H
                self.registers.a = self.and(self.registers.a, self.registers.h);
                1
            }
            0xA5 => {
                // AND A, L
                self.registers.a = self.and(self.registers.a, self.registers.l);
                1
            }
            0xA6 => {
                // AND A, (HL)
                self.registers.a = self.and(
                    self.registers.a,
                    self.memory.read_byte(self.registers.get_hl()),
                );
                2
            }
            0xA7 => {
                // AND A, A
                self.registers.a = self.and(self.registers.a, self.registers.a);
                1
            }
            0xA8 => {
                // XOR A, B
                self.registers.a = self.xor(self.registers.a, self.registers.b);
                1
            }
            0xA9 => {
                // XOR A, C
                self.registers.a = self.xor(self.registers.a, self.registers.c);
                1
            }
            0xAA => {
                // XOR A, D
                self.registers.a = self.xor(self.registers.a, self.registers.d);
                1
            }
            0xAB => {
                // XOR A, E
                self.registers.a = self.xor(self.registers.a, self.registers.e);
                1
            }
            0xAC => {
                // XOR A, H
                self.registers.a = self.xor(self.registers.a, self.registers.h);
                1
            }
            0xAD => {
                // XOR A, L
                self.registers.a = self.xor(self.registers.a, self.registers.l);
                1
            }
            0xAE => {
                // XOR A, (HL)
                self.registers.a = self.xor(
                    self.registers.a,
                    self.memory.read_byte(self.registers.get_hl()),
                );
                2
            }
            0xAF => {
                // XOR A, A
                self.registers.a = self.xor(self.registers.a, self.registers.a);
                1
            }
            0xB0 => {
                // OR A, B
                self.registers.a = self.or(self.registers.a, self.registers.b);
                1
            }
            0xB1 => {
                // OR A, C
                self.registers.a = self.or(self.registers.a, self.registers.c);
                1
            }
            0xB2 => {
                // OR A, D
                self.registers.a = self.or(self.registers.a, self.registers.d);
                1
            }
            0xB3 => {
                // OR A, E
                self.registers.a = self.or(self.registers.a, self.registers.e);
                1
            }
            0xB4 => {
                // OR A, H
                self.registers.a = self.or(self.registers.a, self.registers.h);
                1
            }
            0xB5 => {
                // OR A, L
                self.registers.a = self.or(self.registers.a, self.registers.l);
                1
            }
            0xB6 => {
                // OR A, (HL)
                self.registers.a = self.or(
                    self.registers.a,
                    self.memory.read_byte(self.registers.get_hl()),
                );
                2
            }
            0xB7 => {
                // OR A, A
                self.registers.a = self.or(self.registers.a, self.registers.a);
                1
            }
            0xB8 => {
                // CP A, B
                self.cp(self.registers.a, self.registers.b);
                1
            }
            0xB9 => {
                // CP A, C
                self.cp(self.registers.a, self.registers.c);
                1
            }
            0xBA => {
                // CP A, D
                self.cp(self.registers.a, self.registers.d);
                1
            }
            0xBB => {
                // CP A, E
                self.cp(self.registers.a, self.registers.e);
                1
            }
            0xBC => {
                // CP A, H
                self.cp(self.registers.a, self.registers.h);
                1
            }
            0xBD => {
                // CP A, L
                self.cp(self.registers.a, self.registers.l);
                1
            }
            0xBE => {
                // CP A, (HL)
                self.cp(
                    self.registers.a,
                    self.memory.read_byte(self.registers.get_hl()),
                );
                2
            }
            0xBF => {
                // CP A, (HL)
                self.cp(self.registers.a, self.registers.a);
                1
            }
            0xC0 => {
                // RET NZ
                if !self.registers.get_zero_flag() {
                    self.registers.program_counter = self.pop_stack();
                    5
                } else {
                    2
                }
            }
            0xC1 => {
                // POP BC
                self.registers.set_bc(self.pop_stack());
                3
            }
            0xC2 => {
                // JP NZ, imm16
                if !self.registers.get_zero_flag() {
                    self.registers.program_counter = self.get_word();
                    4
                } else {
                    self.registers.program_counter += 2;
                    3
                }
            }
            0xC3 => {
                // JP imm16
                self.registers.program_counter = self.get_word();
                4
            }
            0xC4 => {
                if !self.registers.get_zero_flag() {
                    self.push_to_stack(self.registers.program_counter + 2);
                    self.registers.program_counter = self.get_word();
                    6
                } else {
                    self.registers.program_counter += 2;
                    3
                }
            }
            0xC5 => {
                // PUSH BC
                self.push_to_stack(self.registers.get_bc());
                4
            }
            0xC6 => {
                // ADD A, imm8
                self.registers.a = self.add8(self.registers.a, self.get_byte(), false);
                2
            }
            0xC7 => {
                // RST 0
                self.push_to_stack(self.registers.program_counter);
                self.registers.program_counter = 0x0000;
                4
            }
            0xC8 => {
                // RET Z
                if self.registers.get_zero_flag() {
                    self.registers.program_counter = self.pop_stack();
                    5
                } else {
                    2
                }
            }
            0xC9 => {
                // RET
                self.registers.program_counter = self.pop_stack();
                4
            }
            0xCA => {
                // JP Z, imm16
                if self.registers.get_zero_flag() {
                    self.registers.program_counter = self.get_word();
                    4
                } else {
                    self.registers.program_counter += 2;
                    3
                }
            }
            0xCB => {
                // big if-else
                let inner_opcode = self.get_byte();
                todo!("Implement secondary opcode table")
            }
            0xCC => {
                // CALL Z, imm16
                if self.registers.get_zero_flag() {
                    self.push_to_stack(self.registers.program_counter + 2);
                    self.registers.program_counter = self.get_word();
                    6
                } else {
                    self.registers.program_counter += 2;
                    3
                }
            }
            0xCD => {
                // CALL imm16
                self.push_to_stack(self.registers.program_counter + 2);
                self.registers.program_counter = self.get_word();
                6
            }
            0xCE => {
                // ADC A, imm8
                self.registers.a = self.add8(self.registers.a, self.get_byte(), true);
                2
            }
            0xCF => {
                // RST 1
                self.push_to_stack(self.registers.program_counter);
                self.registers.program_counter = 0x08;
                4
            }
            0xD0 => {
                // RET NC
                if !self.registers.get_carry_flag() {
                    self.registers.program_counter = self.pop_stack();
                    5
                } else {
                  2
                }
            }
            0xD1 => {
                // POP DE
                self.registers.set_de(self.pop_stack());
                3
            }
            0xD2 => {
                // JP NC, imm16
                if !self.registers.get_carry_flag() {
                    self.registers.program_counter = self.get_word();
                    4
                } else {
                    self.registers.program_counter += 2;
                    3
                }
            }
            0xD4 => {
                // CALL NC, imm16
                if !self.registers.get_carry_flag() {
                    self.push_to_stack(self.registers.program_counter + 2);
                    self.registers.program_counter = self.get_word();
                    6
                } else {
                    self.registers.program_counter += 2;
                    3
                }
            }
            0xD5 => {
                // PUSH DE
                self.push_to_stack(self.registers.get_de());
                4
            }
            0xD6 => {
                // SUB A, imm8
                self.registers.a = self.sub8(self.registers.a, self.get_byte(), false);
                2
            }
            0xD7 => {
                // RST 2
                self.push_to_stack(self.registers.program_counter);
                self.registers.program_counter = 0x10;
                4
            }
            0xD8 => {
                // RST C
                if self.registers.get_carry_flag() {
                    self.registers.program_counter = self.pop_stack();
                    5
                } else {
                    2
                }
            }
            0xD9 => {
                // RETI
                self.registers.program_counter = self.pop_stack();
                todo!("feature to enable master interrupt");
                4
            }
            0xDA => {
                // JP C, imm16
                if self.registers.get_carry_flag() {
                    self.registers.program_counter = self.get_word();
                    4
                } else {
                    self.registers.program_counter += 2;
                    3
                }
            }
            0xDC => {
                // CALL C, imm16
                if self.registers.get_carry_flag() {
                    self.push_to_stack(self.registers.program_counter + 2);
                    self.registers.program_counter = self.get_word();
                    6
                } else {
                    3
                }
            }
            0xDE => {
                // SBC A, imm8
                self.registers.a = self.sub8(self.registers.a, self.get_byte(), true);
                2
            }
            0xDF => {
                // RST 3
                self.push_to_stack(self.registers.program_counter);
                self.registers.program_counter = 0x18;
                4
            }
            0xE0 => {
                // LD (imm8), A
                self.memory
                    .write_byte(self.registers.a, 0xFF00 | u16::from(self.get_byte()));
                3
            }
            0xE1 => {
                // POP HL
                self.registers.set_hl(self.pop_stack());
                3
            }
            0xE2 => {
                // LD (C), A
                self.memory
                    .write_byte(self.registers.a, 0xFF00 | u16::from(self.registers.c()));
                2
            }
            0xE5 => {
                // PUSH HL
                self.push_to_stack(self.registers.get_hl());
                4
            }
            0xE6 => {
                // AND A, imm8
                self.registers.a = self.and(self.registers.a, self.get_byte());
                2
            }
            0xE7 => {
                // RST 4
                self.push_to_stack(self.registers.program_counter);
                self.registers.program_counter = 0x20;
                4
            }
            0xE8 => {
                // ADD SP, imm8
                let imm8 = u16::from(self.get_byte());
                self.registers.stack_pointer = self.add16(self.registers.stack_pointer, imm8);
                4
            }
            0xE9 => {
                // JP HL
                self.registers.program_counter = self.registers.get_hl();
                1
            }
            0xEA => {
                // LD (imm16), A
                self.memory.write_byte(self.registers.a, self.get_word());
                4
            }

            0xEE => {
                // XOR A, imm8
                self.registers.a = self.xor(self.registers.a, self.get_byte());
                2
            }
            0xEF => {
                // RST 5
                self.push_to_stack(self.registers.program_counter);
                self.registers.program_counter = 0x28;
                4
            }
            0xF0 => {
                // LD A, (imm8)
                self.registers.a = self.memory.read_byte(0xFF00 | u16::from(self.get_byte()));
                3
            }
            0xF1 => {
                // POP AF
                self.registers.set_af(self.pop_stack());
                3
            }
            0xF2 => {
                // LD A, (C)
                self.registers.a = self.memory.read_byte(0xFF00 | u16::from(self.registers.a));
                2
            }
            0xF3 => {}
            0xF5 => {
                // PUSH AF
                self.push_to_stack(self.registers.get_af());
                4
            }
            0xF6 => {
                // OR A, imm8
                self.registers.a = self.or(self.registers.a, self.get_byte());
                2
            }
            0xF7 => {
                // RST 6
                self.push_to_stack(self.registers.program_counter);
                self.registers.program_counter = 0x30;
                4
            }
            0xF8 => {
                // LD HL, SP + imm8
                let imm8 = u16::from(self.get_byte());
                let r = self.add16(self.registers.stack_pointer, imm8);
                self.registers.set_hl(r);
                3
            }
            0xF9 => {
                // LD SP, HL
                self.registers.stack_pointer = self.registers.get_hl();
                2
            }
            0xFA => {
                // LD A, (imm16)
                self.registers.a = self.memory.read_byte(self.get_word());
                4
            }
            0xFB => {}
            0xFE => {
                // CP A, imm8
                self.cp(self.registers.a, self.get_byte());
                2
            }
            0xFF => {
                // RST 7
                self.push_to_stack(self.registers.program_counter);
                self.registers.program_counter = 0x38;
                4
            }
            // blank instructions
            0xD3 | 0xDB | 0xDD | 0xEB | 0xEC | 0xED | 0xF4 | 0xFC | 0xFD => {
                panic!("Opcode {:#X} is not supposed to be called", opcode);
            }
            _ => panic!("not implemented!"),
        }
    }
}
