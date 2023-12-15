pub struct Memory {
    ram: [u8; 0xFFFF],

}

impl Memory {
    pub fn new() -> Self {
        Self {
            ram: [0; 0xFFFF]
        }
    }

    pub fn read_byte(&self, address: u16) -> u8 {
        self.ram[address as usize]
    }

    pub fn write_byte(&mut self, value: u8, address: u16) {
        self.ram[address as usize] = value
    }

    pub fn read_word(&self, address: u16) -> u16 {
        u16::from(self.read_byte(address)) | (u16::from(self.read_byte(address + 1)) << 8)
    }

    pub fn write_word(&mut self, value: u16, address: u16) {
        self.write_byte((value & 0x00FF) as u8, address);
        self.write_byte(((value & 0xFF00) >> 8) as u8, address + 1);
    }
}

#[cfg(test)]
mod memory_tests{
    use crate::arch::memory::Memory;

    #[test]
    fn test_read_write_word() {
        let word = 0x3F;
        let mut mem = Memory::new();
        mem.write_word(word, 0x00);
        assert_eq!(mem.read_word(0x00), word)
    }
}