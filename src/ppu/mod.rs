use super::cartridge::Mirroring;
use registers::control::ControlRegister;
use registers::address::AddrRegister;
use registers::status::StatusRegister;
use registers::mask::MaskRegister;
use registers::scroll::ScrollRegister;

pub mod registers;

pub struct NesPPU {
    /// Game visuals stored on cartridge
    pub chr_rom: Vec<u8>,
    pub mirroring: Mirroring,
    pub ctrl: ControlRegister,
    pub mask: MaskRegister,
    pub status: StatusRegister,
    pub scroll: ScrollRegister,
    pub addr: AddrRegister,
    /// 2 KiB of memory to hold background information
    pub vram: [u8; 2048],

    /// Sprite state memory
    pub oam_addr: u8,
    pub oam_data: [u8; 256],
    /// Internal memory to keep pallete tables used by a screen
    pub palette_table: [u8; 32],

    internal_data_buf: u8,

    scanline: u16,
    cycles: usize,
    pub nmi_interrupt: Option<u8>,
}

impl NesPPU {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        NesPPU {
            chr_rom,
            mirroring,
            ctrl: ControlRegister::new(),
            mask: MaskRegister::new(),
            status: StatusRegister::new(),
            oam_addr: 0,
            scroll: ScrollRegister::new(),
            addr: AddrRegister::new(),
            vram: [0; 2048],
            oam_data: [0; 256],
            palette_table: [0; 32],
            internal_data_buf: 0,

            cycles: 0,
            scanline: 0,
            nmi_interrupt: None,
        }
    }

    pub fn tick(&mut self, cycles: u8) -> bool {
        self.cycles += cycles as usize;
        if self.cycles >= 341 {
            self.cycles = self.cycles - 341;
            self.scanline += 1;

            if self.scanline == 241 {
                if self.ctrl.generate_vblank_nmi() {
                    self.status.set_vblank_status(true);
                    todo!("Should trigger NMI interrupt")
                }
            }

            if self.scanline >= 262 {
                self.scanline = 0;
                self.status.reset_vblank_status();
                return true;
            }
        }
        return false;
    }

    pub fn write_to_ppu_addr(&mut self, value: u8) {
        self.addr.update(value);
    }

    pub fn write_to_ctrl(&mut self, value: u8) {
        let before_nmi_status = self.ctrl.generate_vblank_nmi();
        self.ctrl.update(value);
        if !before_nmi_status && self.ctrl.generate_vblank_nmi() && self.status.is_in_vblank() {
            self.nmi_interrupt = Some(1);
        }
    }

    fn increment_vram_addr(&mut self) {
        self.addr.increment(self.ctrl.vram_addr_increment());
    }

    pub fn read_data(&mut self) -> u8 {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0..=0x1fff => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.chr_rom[addr as usize];
                result
            }
            0x2000..=0x2fff => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.vram[self.mirror_vram_addr(addr) as usize];
                result
            }
            0x3000..=0x3eff => panic!("addr space 0x3000..0x3eff is not expected to be used, requested = {} ", addr),
            0x3f00..=0x3fff => {
                self.palette_table[(addr - 0x3f00) as usize]
            },
            _ => panic!("unexpected access to mirrored space {}", addr),
        }
    }

    pub fn write_to_data(&mut self, value: u8) {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0..=0x1fff => println!("attempt to write to chr chr rom space {}", addr),
            0x2000..=0x2fff => {
                self.vram[self.mirror_vram_addr(addr) as usize] = value;
            },
            0x3000..=0x3eff => panic!("addr space 0x3000..0x3eff is not expected to be used, requested = {}", addr),
            0x3f00..=0x3fff => {
                self.palette_table[(addr - 0x3f00) as usize] = value;
            },
            _ => panic!("unexpected access to mirrored space {}", addr),
        }
    }

    /// [short explanation of what the item does]
    ///
    /// Example
    /// ```
    /// ```
    ///
    /// Horizontal:
    ///     [ A ] [ a ]
    ///     [ B ] [ b ]
    ///
    /// Vertical:
    ///     [ A ] [ B ]
    ///     [ a ] [ b ]
    ///
    pub fn mirror_vram_addr(&self, addr: u16) -> u16 {
        let mirrored_vram = addr & 0b11111111111111; // mirror down 0x3000-0x3eff to 0x2000-0x2eff
        let vram_index = mirrored_vram - 0x2000; // to vram vector
        let name_table = vram_index / 0x400; // to the name table index
        match (&self.mirroring, name_table) {
            (Mirroring::VERTICAL, 2) | (Mirroring::VERTICAL, 3) => vram_index - 0x800,
            (Mirroring::HORIZONTAL, 2) => vram_index - 0x400,
            (Mirroring::HORIZONTAL, 1) => vram_index - 0x400,
            (Mirroring::HORIZONTAL, 3) => vram_index - 0x800,
            _ => vram_index
        }
    }

}
