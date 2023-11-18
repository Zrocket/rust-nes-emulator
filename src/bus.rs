use super::Mem;
use super::Rom;
use super::NesPPU;
use super::PPU;
use super::Joypad;
use super::APU;

//  _______________ $10000  _______________
// | PRG-ROM       |       |               |
// | Upper Bank    |       |               |
// |_ _ _ _ _ _ _ _| $C000 | PRG-ROM       |
// | PRG-ROM       |       |               |
// | Lower Bank    |       |               |
// |_______________| $8000 |_______________|
// | SRAM          |       | SRAM          |
// |_______________| $6000 |_______________|
// | Expansion ROM |       | Expansion ROM |
// |_______________| $4020 |_______________|
// | I/O Registers |       |               |
// |_ _ _ _ _ _ _ _| $4000 |               |
// | Mirrors       |       | I/O Registers |
// | $2000-$2007   |       |               |
// |_ _ _ _ _ _ _ _| $2008 |               |
// | I/O Registers |       |               |
// |_______________| $2000 |_______________|
// | Mirrors       |       |               |
// | $0000-$07FF   |       |               |
// |_ _ _ _ _ _ _ _| $0800 |               |
// | RAM           |       | RAM           |
// |_ _ _ _ _ _ _ _| $0200 |               |
// | Stack         |       |               |
// |_ _ _ _ _ _ _ _| $0100 |               |
// | Zero Page     |       |               |
// |_______________| $0000 |_______________|
const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1fff;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3fff;

/// cpu bus object
///
/// Example
/// ```
/// ```
///
pub struct Bus<'call> {
    /// vram memory
    cpu_vram: [u8; 2048],
    /// Stored program rom
    prg_rom: Vec<u8>,
    /// PPU
    ppu: NesPPU,
    /// Cycles
    cycles: usize,
    /// Callback function
    gameloop_callback: Box<dyn FnMut(&NesPPU, &mut Joypad) + 'call>,
    /// Joypad 1
    joypad: Joypad,
}

impl<'a> Bus<'a> {
    /// Create and return a new bus object
    ///
    /// Example
    /// ```
    /// ```
    ///
    pub fn new<'call, F>(rom: Rom, gameloop_callback: F) -> Bus<'call>
    where
        F: FnMut(&NesPPU, &mut Joypad) + 'call
    {
        let ppu = NesPPU::new(rom.chr_rom, rom.screen_mirroring);

        Bus {
            cpu_vram: [0; 2048],
            prg_rom: rom.prg_rom,
            ppu,
            cycles: 0,
            gameloop_callback: Box::from(gameloop_callback),
            joypad: Joypad::new(),
        }
    }

    /// Read the program rom at addr
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            //mirror if needed
            addr = addr % 0x4000;
        }
        self.prg_rom[addr as usize]
    }

    /// Execute a designated ammount of clock cycles
    ///
    /// Example
    /// ```
    /// ```
    ///
    pub fn tick(&mut self, cycles: u8) {
        // add to current cycles
        self.cycles += cycles as usize;

        let nmi_before = self.ppu.nmi_interrupt.is_some();
        // execute ppu clock cycles
        self.ppu.tick(cycles * 3);
        let nmi_after = self.ppu.nmi_interrupt.is_some();

        if !nmi_before && nmi_after {
            (self.gameloop_callback)(&self.ppu, &mut self.joypad);
        }
    }

    /// Return the current status of ppu nmi interrupt
    ///
    /// Example
    /// ```
    /// ```
    ///
    pub fn poll_nmi_status(&mut self) -> Option<u8> {
        self.ppu.poll_nmi_interrupt()
    }
}

impl Mem for Bus<'_> {
    /// Read data from memory location addr
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            RAM..= RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b00000111_11111111;
                self.cpu_vram[mirror_down_addr as usize]
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                //panic!("Attempt to read from write-only PPU address {:X}", addr);
                0
            }
            0x2002 => self.ppu.read_status(),
            0x2004 => self.ppu.read_oam_data(),
            0x2007 => self.ppu.read_data(),

            0x4000..=0x4003 => {
                // Ignore APU
                0
            }

            0x4004..=0x4007 => {
                // Ignore APU
                0
            }

            0x4008..=0x400B => {
                // Ignore APU
                0
            }

            0x400C..=0x400F => {
                // Ignore APU
                0
            }
            
            0x4010..=0x4013 => {
                // Ignore APU
                0
            }

            0x4015 => {
                // Ignore APU
                0
            }

            0x4016 => {
                self.joypad.read()
            }

            0x4017 => {
                // Ignore joypad 2
                0
            }
            0x2008..= PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.mem_read(mirror_down_addr)
            }
            0x8000..=0xFFFF => self.read_prg_rom(addr),

            _ => {
                //println!("Ignoring mem access at {:X}", addr);
                0
            }
        }
    }

    /// Write data to the memory location addr
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b11111111111;
                self.cpu_vram[mirror_down_addr as usize] = data;
            }
            0x2000 => {
                self.ppu.write_to_ctrl(data);
            }
            0x2001 => {
                self.ppu.write_to_mask(data);
            }

            0x2002 => panic!("attempt to write to PPU status register"),

            0x2003 => {
                self.ppu.write_to_oam_addr(data);
            }
            0x2004 => {
                self.ppu.write_to_oam_data(data);
            }
            0x2005 => {
                self.ppu.write_to_scroll(data);
            }

            0x2006 => {
                self.ppu.write_to_ppu_addr(data);
            }
            0x2007 => {
                self.ppu.write_to_data(data);
            }
            0x4000..=0x4013 | 0x4015 => {
                // Ignore APU
            }

            0x4016 => {
                self.joypad.write(data);
            }

            0x4017 => {
                // Ignore joypad 2
            }

            0x4014 => {
                let mut buffer: [u8; 256] = [0; 256];
                let hi: u16 = (data as u16) << 8;
                for i in 0..256u16 {
                    buffer[i as usize] = self.mem_read(hi + i);
                }

                self.ppu.write_oam_dma(&buffer);
            }

            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.mem_write(mirror_down_addr, data);
            }
            0x8000..=0xffff => panic!("Attempt to write to Cartridge ROM space: {:x}", addr),

            _ => {
                println!("Ignoring mem write-access at {:x}", addr);
            }
        }
    }
}
