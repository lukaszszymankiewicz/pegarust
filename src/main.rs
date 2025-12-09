// use raylib::prelude::*;
use std::ops::Index;


enum CRD_MEMORY_MAP {
    HEADER_START = 0x0000,
    ROM_START = 0x0010,
}

const HEADER_SIZE: usize = CRD_MEMORY_MAP::ROM_START as usize - CRD_MEMORY_MAP::HEADER_START as usize;

enum CPU_MEMORY_MAP {
    PROM_START = 0x8000
}

enum PREG_FLAGS {
    Carry = 0,
    Zero = 1,
    InterruptDisable = 2,
    Decimal = 3,
    Overflow = 6,
    Negative = 7,
}

#[repr(u8)]
enum DATA_SOURCE {
    REGISTER,
    MEMORY,
    LITERAL,
    NEXT_BYTE,
}

enum R {
    A = 0,
    X = 1,
    Y = 2,
    P = 3,
    S = 4,
    PCH = 5,
    PCL = 6,
    SP = 7,
    ALL = 8,
}

struct CPU {
    memory: [u8; 0xffff],
    regs: [u8; R::ALL as usize],
}

fn clear_bit(val: u8, n: u8) -> u8 {
     return val & !(1 << n);
}

fn set_bit(val: u8, n: u8) -> u8 {
     return val | (1 << n);
}

impl CPU {
    fn origin() -> CPU {
        CPU {
            memory : [0; 0xffff],
            regs : [0, 0, 0, 0, 0, 0x80, 0x00, 0x00],
        }
    }
    
    // ONLY u8 here
    fn set_flag(&mut self, reg: usize, flag: usize) {
        self.regs[reg] = set_bit(self.regs[reg] as u8, flag as u8);
    }

    fn unset_flag(&mut self, reg: usize, flag: usize) {
        self.regs[reg] = clear_bit(self.regs[reg] as u8, flag as u8);
    }

    fn set_reg(&mut self, reg: usize, val: usize) {
        self.regs[reg] = val as u8
    }

    fn inc_reg(&mut self, reg: usize, val: usize) {
        self.regs[reg] += val as u8;
    }

    fn read_PC_byte(&mut self, delta: usize) -> u8 {
        let pc: usize = ((self.regs[R::PCH as usize] as usize) << 8) + (self.regs[R::PCL as usize]) as usize + delta;
        return self.memory[pc] as u8;
    }

    // to Emulator struct
    fn read_cardridge(&mut self, path: &'static str) {
        let source = std::fs::read(path).unwrap();
        
        // Define the range where you want to copy into the destination array
        let program_len = source.len() - HEADER_SIZE;

        let crd_rom_st: usize = CRD_MEMORY_MAP::ROM_START as usize;

        let cpu_rom_st: usize = CPU_MEMORY_MAP::PROM_START as usize;
        let cpu_rom_end: usize = CPU_MEMORY_MAP::PROM_START as usize + program_len;

        self.memory[cpu_rom_st..cpu_rom_end].copy_from_slice(&(source[crd_rom_st..]));
    }

    // here the translation from usize to u8
    fn run_instruction(&mut self, idx: usize) {
        let i = &INSTRUCTION_SET[idx];

        let old = self.regs[i.reg].clone();
        let val: usize;

        let val: usize = match i.source {
            DATA_SOURCE::REGISTER => self.regs[i.val as usize] as usize,
            DATA_SOURCE::MEMORY => self.memory[i.val as usize] as usize,
            DATA_SOURCE::LITERAL => i.val.clone(),
            DATA_SOURCE::NEXT_BYTE => self.read_PC_byte(1) as usize,
        };

        (i.fun)(self, i.reg, val);

        let new = self.regs[i.reg].clone();
        self.regs[R::P as usize] &= 0b0111_1110;

        // set flags
        self.regs[R::P as usize] += (i.z_fun)(old as u8, new as u8);
        self.regs[R::P as usize] += (i.n_fun)(old as u8, new as u8);

    }
}

fn check_zero_value(old: u8, new: u8) -> u8 {
    return (new == 0) as u8;
}

fn check_negative(old: u8, new: u8) -> u8 {
    return ((new >> 7) == 1) as u8;
}

fn dummy(old: u8, _: u8) -> u8 {
    return old as u8;
}

struct Instruction {
    op: usize,
    source: DATA_SOURCE,
    val: usize,
    reg: usize,
    bytes: usize,
    cycles: usize,
    mnemonic: &'static str,
    fun: fn(&mut CPU, usize, usize),
    z_fun: fn(u8, u8) -> u8,
    n_fun: fn(u8, u8) -> u8,
}

const INSTRUCTION_SET: [Instruction; 5] = [
    Instruction{
        op: 0xe8,
        source: DATA_SOURCE::LITERAL,
        val: 1,
        reg: R::X as usize,
        bytes: 1,
        cycles: 2,
        mnemonic: "INX",
        fun: CPU::inc_reg,
        z_fun: check_zero_value,
        n_fun: check_negative,
    },
    Instruction{
        op: 0x9a,
        source: DATA_SOURCE::REGISTER,
        val: R::X as usize,
        reg: R::SP as usize,
        bytes: 1,
        cycles: 2,
        mnemonic: "TXS",
        fun: CPU::set_reg,
        z_fun: dummy,
        n_fun: dummy,
    },
    Instruction{
        op: 0xa2,
        source: DATA_SOURCE::NEXT_BYTE,
        val: 0,
        reg: R::X as usize,
        bytes: 2,
        cycles: 2,
        mnemonic: "LDX",
        fun: CPU::set_reg,
        z_fun: check_zero_value,
        n_fun: check_negative,
    },
    Instruction{
        op: 0x78,
        source: DATA_SOURCE::LITERAL,
        val: PREG_FLAGS::InterruptDisable as usize,
        reg: R::P as usize,
        bytes: 1,
        cycles: 2,
        mnemonic: "SEI",
        fun: CPU::set_flag,
        z_fun: dummy,
        n_fun: dummy,
    },
    Instruction{
        op: 0xd8,
        source: DATA_SOURCE::LITERAL,
        val: PREG_FLAGS::InterruptDisable as usize,
        reg: R::P as usize,
        bytes: 1,
        cycles: 2,
        mnemonic: "CLD",
        fun: CPU::unset_flag,
        z_fun: dummy,
        n_fun: dummy,
    },
];


fn main() {
    let mut cpu = CPU::origin(); 
    println!("Hello! \n");
    cpu.read_cardridge("src/rom.nes");
    cpu.regs[R::X as usize] = 69;

    println!("value of X register: {}", cpu.regs[R::X as usize]);
    cpu.run_instruction(0);
    println!("value of X register after: {}", cpu.regs[R::X as usize]);
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
