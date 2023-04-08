use std::collections::HashMap;
use std::fs;

use super::{CPU, Mem, AddressingMode, opcodes, Rom, Bus};

pub fn trace(cpu: &mut CPU) -> String {
    let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;
    let code = cpu.mem_read(cpu.program_counter);
    let opcode = opcodes.get(&code).expect(&format!("OpCode {:x} is not recognized", code));

    let mut asm_line: String = "".to_string();
    for i in 0..opcode.len {
        asm_line.push_str(&format!("{:02X} ", cpu.mem_read(cpu.program_counter + i as u16)));
    }

    let op_line = match &opcode.mode {
            AddressingMode::Immediate => {
                format!("{} #${:02X}",
                        opcode.menomic,
                        cpu.mem_read(cpu.program_counter + 1))
            }
            AddressingMode::ZeroPage => {
                let value: u16 = cpu.mem_read(cpu.program_counter + 1) as u16;
                format!("{} ${:02X} = {:02X}",
                        opcode.menomic,
                        cpu.mem_read(cpu.program_counter + 1),
                        cpu.mem_read(value))
            }
            AddressingMode::ZeroPage_x => {
                let operand = cpu.mem_read(cpu.program_counter + 1);
                let val = cpu.register_x.wrapping_add(operand);
                let deref = cpu.mem_read(val as u16);
                format!("{} ${:02X},X @ {:02X} = {:02X}",
                        opcode.menomic,
                        operand,
                        val,
                        deref)
            }
            AddressingMode::ZeroPage_Y => {
                let lo = cpu.mem_read(cpu.program_counter + 1);
                let deref_base = cpu.register_y.wrapping_add(lo);

                format!("{} ${:02X},Y @ {:02X} = {:02X}",
                        opcode.menomic,
                        lo,
                        deref_base,
                        cpu.mem_read(deref_base as u16))
            }
            AddressingMode::Absolute => {
                let lo = cpu.mem_read(cpu.program_counter + 1);
                let hi = cpu.mem_read(cpu.program_counter + 2);
                let deref_base = (hi as u16) << 8 | (lo as u16);

                if opcode.menomic == "JMP" {
                    format!("{} ${:04X}",
                            opcode.menomic,
                            deref_base)
                } else {
                    format!("{} ${:04X} = {:02X}",
                            opcode.menomic,
                            deref_base,
                            cpu.mem_read(deref_base))
                }

            }
            AddressingMode::Absolute_X => {
                let lo = cpu.mem_read(cpu.program_counter + 1);
                let hi = cpu.mem_read(cpu.program_counter + 2);
                let addr = (hi as u16) << 8 | (lo as u16);
                let deref = addr.wrapping_add(cpu.register_x as u16);
                format!("{} ${:04X},X @ {:04X} = {:02X}",
                        opcode.menomic,
                        addr,
                        deref,
                        cpu.mem_read(deref))
            }
            AddressingMode::Absolute_Y => {
                let lo = cpu.mem_read(cpu.program_counter + 1);
                let hi = cpu.mem_read(cpu.program_counter + 2);
                let val = (hi as u16) << 8 | (lo as u16);
                let deref = val.wrapping_add(cpu.register_y as u16);
                format!("{} ${:04X},Y @ {:04X} = {:02X}",
                        opcode.menomic,
                        val,
                        deref,
                        cpu.mem_read(deref))
            }
            AddressingMode::Indirect_X => {
                let base = cpu.mem_read(cpu.program_counter + 1);
                let indirect_base = base.wrapping_add(cpu.register_x);
                let lo = cpu.mem_read(indirect_base as u16);
                let hi = cpu.mem_read((indirect_base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let real_value = cpu.mem_read(deref_base);

                format!("{} (${:02X},X) @ {:02X} = {:04X} = {:02X}",
                opcode.menomic,
                base,
                indirect_base,
                deref_base,
                real_value)
            }
            AddressingMode::Indirect_Y => {
                let base = cpu.mem_read(cpu.program_counter + 1);
                let lo = cpu.mem_read(base as u16);
                let hi = cpu.mem_read((base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(cpu.register_y as u16);
                let real_value = cpu.mem_read(deref);
                
                format!("{} (${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                opcode.menomic,
                base,
                deref_base,
                deref,
                real_value)
            }
            AddressingMode::Relative => {
                let jump: i8 = cpu.mem_read(cpu.program_counter + 1) as i8;
                let jump_addr = cpu.program_counter
                    .wrapping_add(1)
                    .wrapping_add(jump as u16);
                format!("{} ${:04X}",
                        opcode.menomic,
                        jump_addr + 1)
            }
            AddressingMode::NoneAddressing => {
                match &opcode.len {
                    1 => {
                        if &opcode.menomic == &"LSR" || &opcode.menomic == &"ASL" || &opcode.menomic == &"ROL" || &opcode.menomic == &"ROR" {
                            format!("{} A",
                                    opcode.menomic)
                        } else {
                            format!("{}",
                                    opcode.menomic)
                        }
                    }
                    2 => {
                        let offset = cpu.mem_read(cpu.program_counter + 1) as u16;
                        let val = cpu.program_counter + opcode.len as u16 + offset;
                        format!("{} ${:04X}",
                                opcode.menomic,
                                val)
                    }
                    _ => {
                        let lo = cpu.mem_read(cpu.program_counter + 1);
                        let hi = cpu.mem_read(cpu.program_counter + 2);
                        let val = (hi as u16) << 8 | (lo as u16);
                        format!("{} ${:04X}",
                                opcode.menomic,
                                val,)
                    }
                }
            }
            AddressingMode::Absolute_Indirect => {
                let addr = cpu.mem_read_u16(cpu.program_counter + 1);

                let target = if addr & 0x00ff == 0x00ff {
                    let lo = cpu.mem_read(addr);
                    let hi = cpu.mem_read(addr & 0xff00);
                    (hi as u16) << 8 | (lo as u16)
                } else {
                    cpu.mem_read_u16(addr)
                };
                format!("{} (${:04X}) = {:04X}",
                        opcode.menomic,
                        addr,
                        target)

            }
        };

    match opcode.wrap {
        true => {
            let line = format!("{:04X}  {:09}*{:31} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
                                   cpu.program_counter,
                                   asm_line,
                                   op_line,
                                   cpu.register_a,
                                   cpu.register_x,
                                   cpu.register_y,
                                   cpu.status.bits(),
                                   cpu.stack_pointer,
                                   );
            line

        },
        false => {
            let line = format!("{:04X}  {:09} {:31} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
                                   cpu.program_counter,
                                   asm_line,
                                   op_line,
                                   cpu.register_a,
                                   cpu.register_x,
                                   cpu.register_y,
                                   cpu.status.bits(),
                                   cpu.stack_pointer,
                                   );
            line
        }
    }
}

fn test_rom() -> Rom {
    let game_file = fs::read(&"nestest.nes").expect("Cannot open test rom file");
    Rom::new(&game_file).unwrap()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_format_trace() {
        let mut bus = Bus::new(test_rom());
        bus.mem_write(100, 0xa2);
        bus.mem_write(101, 0x01);
        bus.mem_write(102, 0xca);
        bus.mem_write(103, 0x88);
        bus.mem_write(104, 0x00);

        let mut cpu = CPU::new(bus);
        cpu.program_counter = 0x64;
        cpu.register_a = 1;
        cpu.register_x = 2;
        cpu.register_y = 3;
        let mut result: Vec<String> = vec![];
        cpu.run_with_callback(|cpu| {
            result.push(trace(cpu));
        });
        assert_eq!(
            "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD",
            result[0]
        );
        assert_eq!(
            "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD",
            result[1]
        );
        assert_eq!(
            "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
            result[2]
        );
    }

    #[test]
    fn test_foramt_mem_access() {
        let mut bus = Bus::new(test_rom());
        // ORA ($33), Y
        bus.mem_write(100, 0x11);
        bus.mem_write(101, 0x33);

        //data
        bus.mem_write(0x33, 00);
        bus.mem_write(0x34, 04);

        //target cell
       bus.mem_write(0x400, 0xAA);

       let mut cpu = CPU::new(bus);
       cpu.program_counter = 0x64;
       cpu.register_y = 0;
       let mut result: Vec<String> = vec![];
       cpu.run_with_callback(|cpu| {
           result.push(trace(cpu));
       });
       assert_eq!(
           "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD",
           result[0]
       );
    }
}
