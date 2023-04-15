use std::collections::HashMap;
use bitflags::bitflags;
use super::opcodes;
use super::Bus;
use super::Rom;
use super::NES_TAG;


bitflags! {
    /// Convenient constants for common bit flags
    pub struct CpuFlags: u8 {
        /// Carry bit
        const CARRY             = 0b00000001;
        /// Zero bit
        const ZERO              = 0b00000010;
        const INTERRUPT_DISABLE = 0b00000100;
        /// Decimal bit
        const DECIMAL           = 0b00001000;
        const BREAK             = 0b00010000;
        const BREAK2            = 0b00100000;
        /// Overflow bit
        const OVERFLOW          = 0b01000000;
        /// Negative bit
        const NEGATIVE          = 0b10000000;
    }
}

const STACK: u16 = 0x100;
const STACK_RESET: u8 = 0xfd;

#[derive(Debug)]
#[allow(non_camel_case_types)]
/// The different addressing modes an OpCode can perform.
/// Each addressing type determins the operand of the instruction.
pub enum AddressingMode {
    /// The value is provided as the operand
    Immediate,
    /// Address relative to the first page of memory plus a provided byte
    ZeroPage,
    /// Same as ZeroPage addressing, but also adds the value of the x register
    ZeroPage_x,
    /// Same as ZeroPage addressing, but also adds the value of the y register
    ZeroPage_Y,
    /// The value at the provided full u16 address in the operand
    Absolute,
    /// Same as Absolute addressing, but also adds the value of the x register
    Absolute_X,
    /// Same as Absolute addressing, but also adds the value of the y register
    Absolute_Y,
    Absolute_Indirect,
    /// Same as ZeroPage_x, but the value at the address is used as the final address
    Indirect_X,
    /// Same as ZeroPage_y, but the value at the address is used as the final address
    Indirect_Y,
    Relative,
    /// The OpCode does not need or perform memory addressing
    NoneAddressing,
}

/// Defines required memory operations
///
/// Example
/// ```
/// ```
///
pub trait Mem {
    /// Read the u8 contents of a u16 memory address
    ///
    /// Example
    /// ```
    /// let example = self.mem_read(addr);
    /// ```
    ///
    fn mem_read(&mut self, addr: u16) -> u8;

    /// Write u8 data to a u16 memory address
    ///
    /// Example
    /// ```
    /// self.mem_write(addr, data);
    /// ```
    ///
    /// # [OPTIONAL: more explanations and code examples in case some specific
    /// # cases have to be explained in details]
    fn mem_write(&mut self, addr: u16, data: u8);

    /// Read the u16 data of a u16 memory address
    ///
    /// Example
    /// ```
    /// let example = self.mem_read_u16(addr);
    /// ```
    ///
    /// # [OPTIONAL: more explanations and code examples in case some specific
    /// # cases have to be explained in details]
    fn mem_read_u16(&mut self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    /// Write u16 data to a u16 memory address
    ///
    /// Example
    /// ```
    /// self.mem_write_u16(addr, data);
    /// ```
    ///
    /// # [OPTIONAL: more explanations and code examples in case some specific
    /// # cases have to be explained in details]
    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos, hi);
    }
}

/// Contains all the needed components of the CPU:
///     registers
///     memory
///     bus
///     instruction decoding
///
/// Example
/// ```
/// ```
///
/// # [OPTIONAL: more explanations and code examples in case some specific
/// # cases have to be explained in details]
pub struct CPU<'a> {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: CpuFlags,
    pub program_counter: u16,
    pub stack_pointer: u8,
    pub bus: Bus<'a>,
}

impl Mem for CPU<'_> {
    fn mem_read(&mut self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data);
    }

    fn mem_read_u16(&mut self, pos: u16) -> u16 {
        self.bus.mem_read_u16(pos)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        self.bus.mem_write_u16(pos, data);
    }
}
fn page_cross(addr1: u16, addr2: u16) -> bool {
    addr1 & 0xFF00 != addr2 & 0xFF00
}

impl<'a> CPU<'a> {
    /// Creates a new CPU object with default parameters
    ///
    /// Example
    /// ```
    /// ```
    ///
    pub fn new<'b>(bus: Bus<'b>) -> CPU<'b> {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: CpuFlags::from_bits_truncate(0b100100),
            program_counter: 0,
            stack_pointer: STACK_RESET,
            bus,
        }
    }

    /// Pops and returns the u8 value at the stack pointer address
    ///
    /// Example
    /// ```
    /// let example = self.stack_pop();
    /// ```
    ///
    fn stack_pop(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.mem_read((STACK as u16) + self.stack_pointer as u16)
    }

    /// Pushes the provided u8 data to the stack pointer address
    ///
    /// Example
    /// ```
    /// self.stack_push(data);
    /// ```
    ///
    fn stack_push(&mut self, data: u8) {
        self.mem_write((STACK as u16) + self.stack_pointer as u16, data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    /// Pops and returns the u16 value at the stack pointer address
    ///
    /// Example
    /// ```
    /// let example = self.stack_pop_u16();
    /// ```
    ///
    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop() as u16;
        let hi = self.stack_pop() as u16;

        hi << 8 | lo
    }

    /// Pushes the provided u16 data to the stack pointer address
    ///
    /// Example
    /// ```
    /// self.stack_push_u16();
    /// ```
    ///
    fn stack_push_u16(&mut self, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;

        self.stack_push(hi);
        self.stack_push(lo);
    }

    /// Return the operand address of the given addressing mode.
    /// See AddressingMode enum for more details on addressing modes.
    ///
    /// Example
    /// ```
    /// let example = self.get_operand_address(mode);
    /// ```
    ///
    fn get_operand_address(&mut self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,

            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,

            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),

            AddressingMode::ZeroPage_x => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }

            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }

            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }

            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }

            AddressingMode::Indirect_X => {
                // get the base address
                let base = self.mem_read(self.program_counter);

                // add register x to the base address
                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                // combine low and high bytes into a u16
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                // return resulting value
                (hi as u16) << 8 | (lo as u16)
            }

            AddressingMode::Indirect_Y => {
                // get the base address
                let base = self.mem_read(self.program_counter);

                // read the low and high bits at the base address
                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
                // combine low and high values into the new dereferenced base address
                let deref_base = (hi as u16) << 8 | (lo as u16);
                // add the value at register y to the dereferenced base address
                let deref = deref_base.wrapping_add(self.register_y as u16);
                // return the resulting value
                deref
            }

            AddressingMode::NoneAddressing | AddressingMode::Relative | AddressingMode::Absolute_Indirect => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }


    /// Load and run the provided program as a u8 vec reference
    ///
    /// Example
    /// ```
    /// cpu.load_and_run(program);
    /// ```
    ///
    pub fn load_and_run(&mut self, program: &Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    /// Load the provided program as a u8 vec reference.
    /// Does not run the program.
    ///
    /// Example
    /// ```
    /// ```
    ///
    pub fn load(&mut self, program: &Vec<u8>) {
        for i in 0..(program.len() as u16) {
            self.mem_write(0x0600 + i, program[i as usize]);
        }
        //self.mem_write_u16(0xFFFC, 0x0600);
    }

    /// Run the program, if one is loaded
    ///
    /// Example
    /// ```
    /// ```
    ///
    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        // get opcode mapping
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;

        loop {
            if let Some(_nmi) = self.bus.poll_nmi_status() {
                self.interrupt_nmi();
            }

            callback(self);

            // get next OpCode
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let program_counter_state = self.program_counter;

            let opcode = opcodes.get(&code).expect(&format!("OpCode {:x} is not recognized", code));

            // Match byte with instruction
            match code {
                /* ADC */
                0x6d | 0x7d | 0x79 | 0x69 | 0x65 | 0x61 | 0x75 | 0x71 => {
                    self.adc(&opcode.mode);
                }

                /* AND */
                0x2d | 0x3d | 0x39 | 0x29 | 0x25 | 0x21 | 0x35 | 0x31 => {
                    self.and(&opcode.mode);
                }

                /* ASL */
                0x0a | 0x06 | 0x16 | 0x0e | 0x1e => {
                    self.asl(&opcode.mode);
                }

                /* BIT */
                0x2c /*| 0x89*/ | 0x24 => {
                    self.bit(&opcode.mode);
                }

                /* BPL */
                0x10 => {
                    self.branch(!self.status.contains(CpuFlags::NEGATIVE));
                }

                /* BMI */
                0x30 => {
                    self.branch(self.status.contains(CpuFlags::NEGATIVE));
                }

                /* BVC */
                0x50 => {
                    self.branch(!self.status.contains(CpuFlags::OVERFLOW));
                }

                /* BVS */
                0x70 => {
                    self.branch(self.status.contains(CpuFlags::OVERFLOW));
                }

                /* BCC */
                0x90 => {
                    self.branch(!self.status.contains(CpuFlags::CARRY));
                }

                /* BCS */
                0xb0 =>  {
                    self.branch(self.status.contains(CpuFlags::CARRY));
                }

                /* BNE */
                0xd0 => {
                    self.branch(!self.status.contains(CpuFlags::ZERO));
                }

                /* BEQ */
                0xf0 => {
                    self.branch(self.status.contains(CpuFlags::ZERO));
                }

                /* CMP */
                0xcd | 0xdd | 0xd9 | 0xc9 | 0xc5 | 0xc1 | 0xd5 | 0xd1 => {
                    self.compare(&opcode.mode, self.register_a);
                }

                /* CPX */
                0xec | 0xe0 | 0xe4 => {
                    self.compare(&opcode.mode, self.register_x);
                }

                /* CPY */
                0xcc | 0xc0 | 0xc4 => {
                    self.compare(&opcode.mode, self.register_y);
                }

                /* DEC */
                0xce | 0xde | 0xc6 | 0xd6 => {
                    self.dec(&opcode.mode);
                }

                /* EOR */
                0x4d | 0x5d | 0x59 | 0x49 | 0x45 | 0x41 | 0x55 | 0x51 => {
                    self.eor(&opcode.mode);
                }

                /* CLC */
                0x18 => {
                    self.clc();
                }

                /* SEC */
                0x38 => {
                    self.sec();
                }

                /* CLI */
                0x58 => {
                    self.status.remove(CpuFlags::INTERRUPT_DISABLE);
                }

                /* SEI */
                0x78 => {
                    self.status.insert(CpuFlags::INTERRUPT_DISABLE)
                }

                /* CLV */
                0xb8 => {
                    self.status.remove(CpuFlags::OVERFLOW);
                }

                /* CLD */
                0xd8 => {
                    self.cld();
                }

                /* SED */
                0xf8 => {
                    self.sed();
                }

                /* INC */
                0xee | 0xfe | 0xe6 | 0xf6 => {
                    self.inc(&opcode.mode);
                }

                /* JMP Absolute */
                0x4c => {
                    let target = self.mem_read_u16(self.program_counter);
                    self.program_counter = target;
                }
                /* JMP Indirect */
                0x6c => {
                    let addr = self.mem_read_u16(self.program_counter);

                    let target = if addr & 0x00ff == 0x00ff {
                        let lo = self.mem_read(addr);
                        let hi = self.mem_read(addr & 0xff00);
                        (hi as u16) << 8 | (lo as u16)
                    } else {
                        self.mem_read_u16(addr)
                    };

                    self.program_counter = target;
                }

                /* JSR */
                0x20 => {
                    self.stack_push_u16(self.program_counter + 2 - 1);
                    let target = self.mem_read_u16(self.program_counter);
                    self.program_counter = target
                }

                /* LDA */
                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(&opcode.mode);
                }

                /* LDX */
                0xae | 0xbe | 0xa2 | 0xa6 | 0xb6 => {
                    self.ldx(&opcode.mode);
                }

                /* LDY */
                0xac | 0xbc | 0xa0 | 0xa4 | 0xb4 => {
                    self.ldy(&opcode.mode);
                }

                /* LSR */
                0x4a | 0x46 | 0x56 | 0x4e | 0x5e => {
                    self.lsr(&opcode.mode);
                }

                /* STA */
                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                    self.sta(&opcode.mode);
                }

                /* STX */
                0x8e | 0x86 | 0x96 => {
                    self.stx(&opcode.mode);
                }

                /* STY */
                0x8c | 0x84 | 0x94 => {
                    self.sty(&opcode.mode);
                }

                /* SBC */
                0xed | 0xfd | 0xf9 | 0xe9 | 0xe5 | 0xe1 | 0xf5 | 0xf1 => {
                    self.sbc(&opcode.mode);
                }

                /* INX */
                0xe8 => self.inx(),

                /* INY */
                0xc8 => self.iny(),


                /* DEX */
                0xca => self.dex(),

                /* DEY */
                0x88 => self.dey(),


                /* ORA */
                0x0d | 0x1d | 0x19 | 0x09 | 0x05 | 0x01 | 0x15 | 0x11 => {
                    self.ora(&opcode.mode);
                }

                /* ROR Accumulaor */
                0x6a => {
                    self.ror_accumulator();
                }

                /* ROR */
                0x66 | 0x76 | 0x6e | 0x7e => {
                    self.ror(&opcode.mode);
                }

                /* ROL Accumulaor */
                0x2a => {
                    self.rol_accumulator();
                }

                /* ROL */
                0x26 | 0x36 | 0x2e | 0x3e => {
                    self.rol(&opcode.mode);
                }

                /* RTI */
                0x40 => {
                    self.status.bits = self.stack_pop();
                    self.status.remove(CpuFlags::BREAK);
                    self.status.insert(CpuFlags::BREAK2);

                    self.program_counter = self.stack_pop_u16();
                }

                /* RTS */
                0x60 => {
                    self.program_counter = self.stack_pop_u16() + 1;
                }

                /* TXS */
                0x9a => {
                    self.stack_pointer = self.register_x;
                }

                /* TSX */
                0xba => {
                    self.register_x = self.stack_pointer;
                    self.update_zero_and_negative_flags(self.register_x);
                }

                /* PHA */
                0x48 => {
                    self.stack_push(self.register_a);
                }

                /* PLA */
                0x68 => {
                    let value = self.stack_pop();
                    self.set_register_a(value);
                }

                /* PHP */
                0x08 => {
                    let mut flags = self.status.clone();
                    flags.insert(CpuFlags::BREAK);
                    flags.insert(CpuFlags::BREAK2);
                    self.stack_push(flags.bits());
                }

                /* PLP */
                0x28 => {
                    self.status.bits = self.stack_pop();
                    self.status.remove(CpuFlags::BREAK);
                    self.status.insert(CpuFlags::BREAK2);
                }

                /* TAX */
                0xaa => self.tax(),

                /* TXA */
                0x8a => self.txa(),

                /* TAY */
                0xa8 => self.tay(),

                /* TYA */
                0x98 => self.tya(),

                /* BRK */
                0x00 => return,

                /* ILLEGAL OPCODES */

                /* AAC */
                0x0b | 0x2b => {
                    self.and(&opcode.mode);
                    if self.status.contains(CpuFlags::NEGATIVE) {
                        self.status.insert(CpuFlags::CARRY);
                    }
                },

                /* AAX */
                0x87 | 0x97 | 0x83 | 0x8f => {
                    let addr = self.get_operand_address(&opcode.mode);
                    let value = self.register_a & self.register_x;
                    self.mem_write(addr, value);

                    //self.update_zero_and_negative_flags(value);
                },

                /* ARR */
                0x6b => {
                    self.and(&opcode.mode);
                    self.register_a = self.register_a >> 1;
                    if self.register_a & 0b00010000 > 0 {
                        if self.register_a & 0b00100000 > 0 {
                            self.status.insert(CpuFlags::CARRY);
                            self.status.remove(CpuFlags::OVERFLOW);
                        } else {
                            self.status.insert(CpuFlags::OVERFLOW);
                            self.status.remove(CpuFlags::CARRY);
                        }
                    } else if self.register_a & 0b00100000 > 0 {
                        self.status.insert(CpuFlags::CARRY);
                        self.status.insert(CpuFlags::OVERFLOW);
                    } else {
                        self.status.remove(CpuFlags::CARRY);
                        self.status.remove(CpuFlags::OVERFLOW);
                    }

                    self.update_zero_and_negative_flags(self.register_a);
                },

                /* ASR */
                0x4b => {
                    self.and(&opcode.mode);
                    self.ror_accumulator();
                },

                /* ATX */
                0xab => {
                    self.and(&opcode.mode);
                    self.register_x = self.register_a;
                    self.update_zero_and_negative_flags(self.register_x);
                },

                /* AXA */
                0x9f | 0x93 => {
                    let addr = self.get_operand_address(&opcode.mode);
                    let mut value = self.register_x & self.register_a;
                    value = value & 7;
                    self.mem_write(addr, value);
                },

                /* AXS */
                0xcb => {
                    let addr = self.get_operand_address(&opcode.mode);
                    self.mem_write(addr, self.register_x & self.register_a);
                },

                /* DCP */
                0xc7 | 0xd7 | 0xcf | 0xdf | 0xdb | 0xc3 | 0xd3 => {
                    let addr = self.get_operand_address(&opcode.mode);
                    self.dec(&opcode.mode);
                    let value = self.mem_read(addr);
                    self.update_zero_and_negative_flags(self.register_a.wrapping_sub(value));
                },

                /* DOP */
                0x04 | 0x14 | 0x34 | 0x44 | 0x54 | 0x64 | 0x74 | 0x80 | 0x82 | 0x89 | 0xc2 | 0xd4 | 0xe2 | 0xf4 => {},

                /* ISC */
                0xe7 | 0xf7 | 0xef | 0xff | 0xfb | 0xe3 | 0xf3 => {
                    self.inc(&opcode.mode);
                    self.sbc(&opcode.mode);
                },

                /* KIL */
                0x02 | 0x12 | 0x22 | 0x32 | 0x42 | 0x52 | 0x62 | 0x72 | 0x92 | 0xb2 | 0xd2 | 0xf2 => return,

                /* LAR */
                0xbb => {
                },

                /* LAX */
                0xa7 | 0xb7 | 0xaf | 0xbf | 0xa3 | 0xb3 => {
                    self.lda(&opcode.mode);
                    self.register_x = self.register_a;
                    self.update_zero_and_negative_flags(self.register_x);
                },

                /* NOP */
                0xea | 0x1a | 0x3a | 0x5a | 0x7a | 0xda | 0xfa => {},

                /* RLA */
                0x27 | 0x37 | 0x2f | 0x3f | 0x3b | 0x23 | 0x33 => {
                    self.rol(&opcode.mode);
                    self.and(&opcode.mode);
                },

                /* RRA */
                0x67 | 0x77 | 0x6f | 0x7f | 0x7b | 0x63 | 0x73 => {
                    self.ror(&opcode.mode);
                    self.adc(&opcode.mode);
                },

                /* SBC */
                0xeb => {
                    self.sbc(&opcode.mode);
                },

                /* SLO */
                0x07 | 0x17 | 0x0f | 0x1f | 0x1b | 0x03 | 0x13 => {
                    let addr = self.get_operand_address(&opcode.mode);
                    self.asl(&opcode.mode);
                    let value = self.mem_read(addr);
                    self.set_register_a(self.register_a | value);
                },

                /* SRE */
                0x47 | 0x57 | 0x4f | 0x5f | 0x5b | 0x43 | 0x53 => {
                    let addr = self.get_operand_address(&opcode.mode);
                    self.lsr(&opcode.mode);
                    let value = self.mem_read(addr);
                    self.set_register_a(self.register_a ^ value);
                },

                /* SXA */
                0x9e => {},

                /* SYA */
                0x9c => {},

                /* TOP */
                0x0c | 0x1c | 0x3c | 0x5c | 0x7c | 0xdc | 0xfc => {},

                /* XAA */
                0x8b => {},

                /* XAS */
                0x9b => {},

                _ => todo!()
            }

            self.bus.tick(opcode.cycles);

            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.len - 1) as u16;
            }
        }
    }

    /// Reset CPU state
    ///
    /// Example
    /// ```
    /// ```
    ///
    pub fn reset(&mut self) {
        self.set_register_a(0);
        self.register_x = 0;
        self.status = CpuFlags::from_bits_truncate(0b100100);

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    /// Load accumulator with memory
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_register_a(value);
    }

    /// Load register x with memory
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// Rotate right one bit
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn ror(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        let old_carry = self.status.contains(CpuFlags::CARRY);

        if data & 1 == 1 {
            self.status.insert(CpuFlags::CARRY);
        }
        else {
            self.status.remove(CpuFlags::CARRY);
        }

        data = data >> 1;

        if old_carry {
            data = data | 0b10000000;
        }

        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
    }

    fn ror_accumulator(&mut self) {
        let mut data = self.register_a;
        let old_carry = self.status.contains(CpuFlags::CARRY);

        if data & 1 == 1 {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }
        data = data >> 1;
        if old_carry {
            data = data | 0b10000000;
        }
        self.set_register_a(data);
    }

    /// Rotate left one bit
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn rol(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        let old_carry = self.status.contains(CpuFlags::CARRY);

        if data >> 7 == 1 {
            self.status.insert(CpuFlags::CARRY);
        }
        else {
            self.status.remove(CpuFlags::CARRY);
        }

        data = data << 1;

        if old_carry {
            data = data | 0b00000001;
        }

        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
    }

    fn rol_accumulator(&mut self) {
        let mut data = self.register_a;
        let old_carry = self.status.contains(CpuFlags::CARRY);

        if data >> 7 == 1 {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }
        data = data << 1;
        if old_carry {
            data = data | 1;
        }
        self.set_register_a(data);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn sta(&mut self, mode: &AddressingMode) {
       let addr = self.get_operand_address(mode);
       self.mem_write(addr, self.register_a);
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.add_to_register_a(value);

        self.update_zero_and_negative_flags(self.register_a);

    }

    /// Sets accumulator to the specified value and updates cpu flags
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn set_register_a(&mut self, value: u8) {
        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn add_to_register_a(&mut self, value: u8) {
        let sum = self.register_a as u16
            + value as u16
            + (if self.status.contains(CpuFlags::CARRY) {
                1
            } else {
                0
            }) as u16;

        let carry = sum > 0xff;

        if carry {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }

        let result = sum as u8;

        if (value ^ result) & (result ^ self.register_a) & 0x80 != 0 {
            self.status.insert(CpuFlags::OVERFLOW);
        } else {
            self.status.remove(CpuFlags::OVERFLOW);
        }

        self.set_register_a(result);

    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.add_to_register_a(((value as i8).wrapping_neg().wrapping_sub(1)) as u8);
    }

    /// Increments the memory value by one.
    /// If the value is 0xff, it will overflow back into 0.
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut value = self.mem_read(addr);

        if value == 0xff {
            value = 0;
            self.mem_write(addr, value);
        } else {
            value = value + 1;
            self.mem_write(addr, value);
        }

        self.update_zero_and_negative_flags(value);
    }

    /// Increments the value of the x register.
    /// If the value if 0xff, it will overflow back into 0.
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn inx(&mut self) {
        if self.register_x == 0xff {
            self.register_x = 0;
        } else {
            self.register_x += 1;
        }
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// Increments the value of the y register.
    /// If the value if 0xff, it will overflow back into 0.
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn iny(&mut self) {
        if self.register_y == 0xff {
            self.register_y = 0;
        } else {
            self.register_y += 1;
        }
        self.update_zero_and_negative_flags(self.register_y);
    }

    /// Decrements the memory value.
    /// If the value is 0, it will underflow into 0xff.
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut value = self.mem_read(addr);

        if value == 0 {
            value = 0xff;
            self.mem_write(addr, value);
        } else {
            value = value - 1;
            self.mem_write(addr, value);
        }

        self.update_zero_and_negative_flags(value);
    }

    /// Decrements the value of the x register.
    /// If the value is 0, it will underflow into 0xff.
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn dex(&mut self) {
        if self.register_x == 0 {
            self.register_x = 0xff;
        } else {
            self.register_x -= 1;
        }
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// Decrements the value of the y register.
    /// If the value is 0, it will underflow into 0xff.
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn dey(&mut self) {
        if self.register_y == 0 {
            self.register_y = 0xff;
        } else {
            self.register_y -= 1;
        }
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn asl(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::NoneAddressing => {
                if self.register_a & 0b10000000 > 0 {
                    self.status.insert(CpuFlags::CARRY);
                } else {
                    self.status.remove(CpuFlags::CARRY);
                }

                self.set_register_a(self.register_a << 1);
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let mut value = self.mem_read(addr);

                if value & 0b10000000 > 0 {
                    self.status.insert(CpuFlags::CARRY);
                } else {
                    self.status.remove(CpuFlags::CARRY);
                }

                value = value << 1;
                self.mem_write(addr, value);
                self.update_zero_and_negative_flags(value);
            }
        }
    }
    
    fn lsr(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::NoneAddressing => {
                if self.register_a & 0b00000001 > 0 {
                    self.status.insert(CpuFlags::CARRY);
                } else {
                    self.status.remove(CpuFlags::CARRY);
                }

                self.set_register_a(self.register_a >> 1);
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let mut value = self.mem_read(addr);

                if value & 0b00000001 > 0 {
                    self.status.insert(CpuFlags::CARRY);
                } else {
                    self.status.remove(CpuFlags::CARRY);
                }

                value = value >> 1;
                self.mem_write(addr, value);
                self.update_zero_and_negative_flags(value);
            }
        }
    }

    fn and(&mut self, mode:  &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_register_a(self.register_a & value);

    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_register_a(self.register_a | value);

    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_register_a(self.register_a ^ value);

    }

    fn compare(&mut self, mode: &AddressingMode, compare_with: u8) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        if compare_with >= value {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }

        self.update_zero_and_negative_flags(compare_with.wrapping_sub(value));
    }

    fn branch(&mut self, condition: bool) {
        if condition {
            let jump: i8 = self.mem_read(self.program_counter) as i8;
            let jump_addr = self.program_counter
                .wrapping_add(1)
                .wrapping_add(jump as u16);

            self.program_counter = jump_addr;
        }
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        if value & self.register_a == 0 {
            self.status.insert(CpuFlags::ZERO);
        } else {
            self.status.remove(CpuFlags::ZERO);
        }

        self.status.set(CpuFlags::NEGATIVE, value & 0b10000000 > 0);
        self.status.set(CpuFlags::OVERFLOW, value & 0b01000000 > 0);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn txa(&mut self) {
        self.set_register_a(self.register_x);
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn tya(&mut self) {
        self.set_register_a(self.register_y);
    }

    fn clc(&mut self) {
        self.status.remove(CpuFlags::CARRY);
    }

    fn sec(&mut self) {
        self.status.insert(CpuFlags::CARRY);
    }

    fn cld(&mut self) {
        self.status.remove(CpuFlags::DECIMAL);
    }

    fn sed(&mut self) {
        self.status.insert(CpuFlags::DECIMAL);
    }

    /// Updates the zero and negative CPU flags based on the provided result
    ///
    /// Example
    /// ```
    /// ```
    ///
    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status.insert(CpuFlags::ZERO);
        } else {
            self.status.remove(CpuFlags::ZERO);
        }

        if result >> 7 == 1 {
            self.status.insert(CpuFlags::NEGATIVE);
        } else {
            self.status.remove(CpuFlags::NEGATIVE);
        }
    }

    fn interrupt_nmi(&mut self) {
        self.stack_push_u16(self.program_counter);
        let mut flag = self.status.clone();
        flag.insert(CpuFlags::BREAK);
        flag.insert(CpuFlags::BREAK2);

        self.stack_push(flag.bits);
        self.status.insert(CpuFlags::INTERRUPT_DISABLE);

        self.bus.tick(2);
        self.program_counter = self.mem_read_u16(0xfffa);
    }
}
