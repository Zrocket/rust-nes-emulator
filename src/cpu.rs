use std::collections::HashMap;
use bitflags::bitflags;
use super::opcodes;
use super::Bus;

bitflags! {
    pub struct CpuFlags: u8 {
        const CARRY             = 0b00000001;
        const ZERO              = 0b00000010;
        const INTERRUPT_DISABLE = 0b00000100;
        const DECIMAL           = 0b00001000;
        const BREAK             = 0b00010000;
        const UNUSED            = 0b00100000;
        const OVERFLOW          = 0b01000000;
        const NEGATIVE          = 0b10000000;
    }
}

const STACK: u16 = 0x100;
const STACK_RESET: u8 = 0xfd;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_x,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

pub trait Mem {
    fn mem_read(&self, addr: u16) -> u8;

    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(& self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos, hi);
    }
}

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: CpuFlags,
    pub program_counter: u16,
    pub stack_pointer: u8,
    pub bus: Bus,
}

impl Mem for CPU {
    fn mem_read(&self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data);
    }

    fn mem_read_u16(& self, pos: u16) -> u16 {
        self.bus.mem_read_u16(pos)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        self.bus.mem_write_u16(pos, data);
    }
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: CpuFlags::from_bits_truncate(0b100100),
            program_counter: 0,
            stack_pointer: STACK_RESET,
            bus: Bus::new(),
        }
    }

    fn stack_pop(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.mem_read((STACK as u16) + self.stack_pointer as u16)
    }

    fn stack_push(&mut self, data: u8) {
        self.mem_write((STACK as u16) + self.stack_pointer as u16, data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop() as u16;
        let hi = self.stack_pop() as u16;

        hi << 8 | lo
    }

    fn stack_push_u16(&mut self, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;

        self.stack_push(hi);
        self.stack_push(lo);
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
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
                let base = self.mem_read(self.program_counter);

                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }

            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }

            AddressingMode::NoneAddressing => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }


    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x0600 .. (0x0600 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x0600);
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;

        loop {
            callback(self);

            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let program_counter_state = self.program_counter;

            let opcode = opcodes.get(&code).expect(&format!("OpCode {:x} is not recognized", code));

            let val = self.mem_read((STACK as u16) + self.stack_pointer as u16);
            println!("a: {:x}", self.register_a);
            println!("x: {:x}", self.register_x);
            println!("y: {:x}", self.register_y);
            println!("pc: {:x}", self.program_counter);
            println!("sp: {:x}", val);
            println!("{:x}", code);

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
                0x2c | 0x89 | 0x24 => {
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

                /* ROR */
                0x6a | 0x66 | 0x76 | 0x6e | 0x7e => {
                    self.ror(&opcode.mode);
                }

                /* ROL */
                0x2a | 0x26 | 0x36 | 0x2e | 0x3e => {
                    self.rol(&opcode.mode);
                }

                /* RTI */
                0x40 => {
                    self.status.bits = self.stack_pop();
                    self.status.remove(CpuFlags::BREAK);
                    self.status.insert(CpuFlags::UNUSED);

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
                    self.stack_push(self.status.bits());
                    self.status.insert(CpuFlags::BREAK);
                    self.status.insert(CpuFlags::UNUSED);
                }

                /* PLP */
                0x28 => {
                    self.status.bits = self.stack_pop();
                    self.status.remove(CpuFlags::BREAK);
                    self.status.remove(CpuFlags::UNUSED);
                }

                /* TAX */
                0xaa => self.tax(),

                /* TXA */
                0x8a => self.txa(),

                /* TAY */
                0xa8 => self.tay(),

                /* TYA */
                0x98 => self.tya(),

                /* NOP */
                0xea => {}

                /* BRK */
                0x00 => return,
                _ => todo!()
            }

            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.len - 1) as u16;
            }
        }
    }

    pub fn reset(&mut self) {
        self.set_register_a(0);
        self.register_x = 0;
        self.status = CpuFlags::from_bits_truncate(0b100100);

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_register_a(value);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }

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
            data = data & 0b10000000;
        }

        match mode {
            AddressingMode::Absolute => {
                self.set_register_a(data);
            }
            _ => {
                self.mem_write(addr, data);
                self.update_zero_and_negative_flags(data);
            }
        }
    }

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
            data = data & 0b00000001;
        }

        match mode {
            AddressingMode::Absolute => {
                self.set_register_a(data);
            }
            _ => {
                self.mem_write(addr, data);
                self.update_zero_and_negative_flags(data);
            }
        }
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

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        if value == 0xff {
            self.mem_write(addr, 0);
        } else {
            self.mem_write(addr, value + 1);
        }
    }

    fn inx(&mut self) {
        if self.register_x == 0xff {
            self.register_x = 0;
        } else {
            self.register_x += 1;
        }
    }

    fn iny(&mut self) {
        if self.register_y == 0xff {
            self.register_y = 0;
        } else {
            self.register_y += 1;
        }
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        if value == 0 {
            self.mem_write(addr, 0xff);
        } else {
            self.mem_write(addr, value - 1);
        }
    }

    fn dex(&mut self) {
        if self.register_x == 0 {
            self.register_x = 0xff;
        } else {
            self.register_x -= 1;
        }
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn dey(&mut self) {
        if self.register_y == 0 {
            self.register_y = 0xff;
        } else {
            self.register_y -= 1;
        }
    }

    fn asl(&mut self, mode: &AddressingMode) {
        if self.register_a & 0b1000000 > 0 {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }

        self.set_register_a(self.register_a << 1);

    }
    
    fn lsr(&mut self, mode: &AddressingMode) {
        if self.register_a & 0b0000001 > 0 {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }

        self.set_register_a(self.register_a >> 1);

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

        self.status.set(CpuFlags::NEGATIVE, value & 0b1000000 > 0);
        self.status.set(CpuFlags::OVERFLOW, value & 0b0100000 > 0);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn txa(&mut self) {
        self.set_register_a(self.register_x);
    }

    fn tay(&mut self) {
        self.set_register_a(self.register_y);
    }

    fn tya(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
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
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immidiate_load_data() {
        let mut cpu = CPU::new();
        let program = vec![0xa9, 0x05, 0x00];
        cpu.load_and_run(program);
        assert_eq!(cpu.register_a, 0x05);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b00);
        assert!(cpu.status.bits()  & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        let program = vec![0xa9, 0x00, 0x00];
        cpu.load_and_run(program);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_c() {
        let mut cpu = CPU::new();
        let program = vec![0xa9, 0x0a, 0xaa, 0x00];
        cpu.load_and_run(program);

        assert_eq!(cpu.register_x, 10);
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        let program = vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00];
        cpu.load_and_run(program);

        assert_eq!(cpu.register_x, 0xc1);
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        let program = vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00];
        cpu.load_and_run(program);

        println!("{}", cpu.register_x);

        assert_eq!(cpu.register_x, 1);
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
    }
}
