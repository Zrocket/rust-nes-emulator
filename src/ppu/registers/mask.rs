use bitflags::bitflags;

bitflags! {
    pub struct MaskRegister: u8 {
        const GREYSCALE                 = 0b00000001;
        const LEFTMOST_BACKGROUND       = 0b00000010;
        const LEFTMOST_SPRITES          = 0b00000100;
        const BACKGROUND                = 0b00001000;
        const SPRITES                   = 0b00010000;
        const RED                       = 0b00100000;
        const GREEN                     = 0b01000000;
        const BLUE                      = 0b10000000;
    }
}

impl MaskRegister {
    pub fn new() -> Self {
        MaskRegister::from_bits_truncate(0b00000000)
    }

    pub fn update(&mut self, data: u8) {
        self.bits = data;
    }
}
