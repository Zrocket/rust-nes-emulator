use bitflags::bitflags;

bitflags! {
    /// PPU operation flags
    ///
    /// Example
    /// ```
    /// ```
    ///
    pub struct ControlRegister: u8 {
        /// Base nametable address 1
        const NAMETABLE1                = 0b00000001;
        /// Base nametable address 2
        const NAMETABLE2                = 0b00000010;
        /// VRAM address increment per CPU read/write of PPUDATA
        /// (0: add 1, going access; 1: add 32, going down)
        const VRAM_ADD_INCREMENT        = 0b00000100;
        /// Sprite pattern table address for 8x8 sprites
        /// (0: $0000; 1: $1000; ignored in 8x16 mode)
        const SPRITE_PATTERN_ADDR       = 0b00001000;
        /// Background pattern table address
        /// (0: $0000; 1: $1000)
        const BACKGROUND_PATTERN_ADDR   = 0b00010000;
        /// Sprite size
        /// (0: 8x8 pixels; 1: 8x16 pixels)
        const SPRITE_SIZE               = 0b00100000;
        /// PPU master/slave select
        /// (0: read backdrop from EXT pins; 1: output color on EXT pins)
        const MASTER_SLAVE_SELECT       = 0b01000000;
        /// Generate an NMI at the start of the vertical background interval
        /// (0: off; 1: on)
        const GENERATE_NMI              = 0b10000000;
    }
}

impl ControlRegister {
    pub fn new() -> Self {
        ControlRegister::from_bits_truncate(0b00000000)
    }

    pub fn nametable_addr(&self) -> u16 {
        match self.bits & 0b11 {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2c00,
            _ => panic!("not possible"),
        }
    }

    pub fn vram_addr_increment(&self) -> u8 {
        if !self.contains(ControlRegister::VRAM_ADD_INCREMENT) {
            1
        } else {
            32
        }
    }

    pub fn sprt_pattern_addr(&self) -> u16 {
        if !self.contains(ControlRegister::SPRITE_PATTERN_ADDR) {
            0
        } else {
            0x1000
        }
    }

    pub fn bknd_pattern_addr(&self) -> u16 {
        if !self.contains(ControlRegister::BACKGROUND_PATTERN_ADDR) {
            0
        } else {
            0x1000
        }
    }

    pub fn sprite_size(&self) -> u8 {
        if !self.contains(ControlRegister::SPRITE_SIZE) {
            8
        } else {
            16
        }
    }

    pub fn master_slave_select(&self) -> u8 {
        if !self.contains(ControlRegister::MASTER_SLAVE_SELECT) {
            0
        } else {
            1
        }
    }

    pub fn generate_vblank_nmi(&self) -> bool {
        return self.contains(ControlRegister::GENERATE_NMI);
    }

    pub fn update(&mut self, data: u8) {
        self.bits = data;
    }
}
