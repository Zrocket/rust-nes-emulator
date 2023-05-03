use bitflags::bitflags;

bitflags! {
    pub struct Attributes: u8 {
        const PALETTE1 =            0b00000001;
        const PALETTE2 =            0b00000010;
        const UNIMPLEMENTED1 =      0b00000100;
        const UNIMPLEMENTED2 =      0b00001000;
        const UNIMPLEMENTED3 =      0b00010000;
        const PRIORITY =            0b00100000;
        const FLIP_HORIZONTALLY =   0b01000000;
        const FLIP_VERTICALLY =     0b10000000;
    }
}

pub struct Sprite {
    // byte 0
    pub y_position: u8,
    // byte 1
    pub tile_index: u8,
    // byte 2
    pub attributes: u8,
    // byte 3
    pub x_position: u8,
}

impl Sprite {
    pub fn new() -> Self {
        Self {
            y_position: 0,
            tile_index: 0,
            attributes: 0,
            x_position: 0
        }
    }
}
