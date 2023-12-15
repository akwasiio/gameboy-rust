pub struct Memory {
    ram: [u8; 0xFFFF],

}

impl Memory {
    pub fn new() -> Self {
        Self {
            ram: [0; 0xFFFF]
        }
    }

    pub fn read_byte() -> u8 {
        unimplemented!("yet to implement this")
    }

    pub fn write_byte(value: u8) {
        unimplemented!("yet to implement this")
    }

    pub fn read_word() -> u16 {
        unimplemented!("yet to implement this")
    }

    pub fn write_word(word: u16) {
        unimplemented!("yet to implement this")
    }


}