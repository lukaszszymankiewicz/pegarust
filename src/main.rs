// use raylib::prelude::*;
use std::ops::Index;

enum PREG_FLAGS {
    Carry = 0,
    Zero = 1,
    InterruptDisable = 2,
    Decimal = 3,
    Overflow = 6,
    Negative = 7,
}

enum REGISTER {
    A = 0,
    X = 1,
    Y = 2,
    P = 3,
    PCH = 4,
    PCL = 5,
    S = 6,
    ALL = 7
}

type InstructionFunction = fn(u8, u8) -> u8;



#[derive(Clone, Copy)]
struct Instruction {
    op: u8,
    mnemonic: &'static str,
    register: u8,
    arg: u8,
    fun: InstructionFunction,
    // Cfun: InstructionFunction,
}

struct InstructionSet {
    instructions: [Instruction; 25],
}

impl Index<u8> for InstructionSet {
    type Output = Instruction;

    fn index(&self, index: u8) -> &Self::Output {
        return &self.instructions[index as usize]
    }
}

const InstructionMap : InstructionSet = InstructionSet{
    instructions : [
        Instruction{ op: 0x00, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x01, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x02, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x03, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x04, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x05, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x06, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x07, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x08, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x09, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x0a, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x0b, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x0c, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x0d, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x0e, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x0f, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x10, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x11, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x12, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x13, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x14, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x15, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x16, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{ op: 0x17, mnemonic: "", register: 0, arg: 0, fun: clear_bit },
        Instruction{
            op: 0x18,
            mnemonic: "CLC",
            register: REGISTER::P as u8,
            arg: PREG_FLAGS::Carry as u8,
            fun: clear_bit
        }
    ]
};

struct CPU {
    memory: [u8; 0xffff],
    registers: [u8; REGISTER::ALL as usize],
}

impl CPU {
    fn origin() -> CPU {
        CPU {
            memory : [0; 0xffff],
            registers : [0; REGISTER::ALL as usize]
        }
    }

    fn run_instruction(&mut self, ins: u8) {
        let ins = InstructionMap[ins];

        self.registers[ins.register as usize] = (ins.fun)(
            self.registers[ins.register as usize], ins.arg
        );
    }
}

fn clear_bit(val: u8, n: u8) -> u8 {
     return val & !(1 << n);
}

fn main() {
    let mut cpu = CPU::origin(); 
    println!("Hello! \n");
    cpu.registers[REGISTER::P as usize] = 0b1111_1111;
    println!("value of P register: {}", cpu.registers[REGISTER::P as usize]);
    cpu.run_instruction(0x18);
    println!("value of P register after: {}", cpu.registers[REGISTER::P as usize]);

    let op = 0x18;
    // let (mut rl, thread) = raylib::init()
    //     .size(640, 480)
    //     .title("Hello, World")
    //     .build();

    // while !rl.window_should_close() {
    //     let mut d = rl.begin_drawing(&thread);

    //     d.clear_background(Color::WHITE);
    //     d.draw_text("Hello, world!", 12, 12, 20, Color::BLACK);
    // }
}
