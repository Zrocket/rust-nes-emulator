use std::collections::HashMap;
use std::fs;

use super::{CPU, Mem, AddressingMode, opcodes, Rom, Bus};

fn trace(cpu: &mut CPU) -> String {
    let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;
    let code = cpu.mem_read(cpu.program_counter);
    let opcode = opcodes.get(&code).expect(&format!("OpCode {:x} is not recognized", code));

    let asm_line = format!("{:02X} {:02X}",
                      cpu.mem_read(cpu.program_counter),
                      cpu.mem_read(cpu.program_counter + 1),
                      );

    match &opcode.mode {
        Immediate => {}
        ZeroPage => {}
        ZeroPage_x => {}
        ZeroPage_Y => {}
        Absolute => {}
        Absolute_X => {}
        Absolute_Y => {}
        Indirect_X => {}
        Indirect_Y => {}
        NoneAddressing=> {}
    }

    let op_line = format!("{}",
                          opcode.menomic,
                          );

    let line = format!("{:04X}  {}   {} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
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
