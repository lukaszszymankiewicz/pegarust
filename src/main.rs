// use raylib::prelude::*;

const PRG_HEADER_START: usize = 0x0000;
const PRG_ROM_START: usize = 0x0010;

const HEADER_SIZE: usize = PRG_ROM_START - PRG_HEADER_START;

const CPU_RAM_START: usize = 0x0100;
const CPU_RAM_END: usize = 0x01FF;
const CPU_PRG_START: usize = 0x8000;

const CF: usize = 0;
const ZF: usize = 1;
const IDF: usize = 2;
const DF: usize = 3;
const OF: usize = 6;
const NF: usize = 7;

#[derive(Copy, Clone)]
enum MemoryType {
    memory,
    literal,
    acc,
}

#[derive(Copy, Clone)]
struct Address {
    val: u16,
    mode: MemoryType,
}

impl std::ops::Add<u8> for Address {
    type Output = Address;

    fn add(mut self, _val: u8) -> Address {
        self.val = self.val + _val as u16;
        return self;
    }
}

#[derive(Copy, Clone)]
enum AddressingMode {
    imm   , // Immediate - Uses the 8-bit operand itself as the value for the operation
    zp   , // Zero page - Fetches the value from an 8-bit address on the zero page.
    abs   , // Absolute - Fetches the value from a 16-bit address anywhere in memory.
    zpx  , // Zero page indexed - val = PEEK((arg + X) % 256)
    zpy  , // Zero page indexed - val = PEEK((arg + Y) % 256)
    absx  , // Absolute indexed - val = PEEK(arg + X)
    absy  , // Absolute indexed - val = PEEK(arg + Y)
    indx	, // Indexed indirect - val = PEEK(PEEK((arg + X) % 256) + PEEK((arg + X + 1) % 256) * 256)
    indy	, // Indirect indexed - val = PEEK(PEEK(arg) + PEEK((arg + 1) % 256) * 256 + Y)
    acc   , // Accumulator,
    relative, 	// Relative	Branch instructions (e.g. BEQ, BCS) have a relative addressing mode that specifies an 8-bit signed offset relative to the current PC.
    implied, // JMP instruction has a special indirect addressing mode that can jump to the address stored in a 16-bit pointer anywhere in memory.
}

struct Instruction {
    op: usize,
    mode: AddressingMode,
    bytes: usize,
    cycles: usize,
    mnemonic: &'static str,
    fun: fn(&mut CPU, Address),
}

struct CPU {
    memory: [u8; 0xffff],
    reg_a : u8,
    reg_x : u8,
    reg_y : u8,
    reg_p : u8,
    reg_s : u8,
    reg_n : u8,
    reg_sp: u8,
    reg_pc: u16,

}

// bit operations
fn clear_bit(val: usize, n: usize) -> usize {
     return val & !(1 << n);
}

fn set_bit(val: usize, n: usize) -> usize {
     return val | (1 << n);
}

fn get_bit(val: usize, n: usize) -> usize {
    return (val >> n) & 1;
}

fn copy_bit(src: u8, dest: u8, n: usize) -> u8 {
    let mask: u8 = 1u8 << n;
    return ((dest & !mask) | (src & mask)) as u8; 
}

// flag operations
fn check_zero(val: u8) -> bool {
    return (val == 0)
}

fn check_negative(val: u8) -> bool {
    return ((val >> 7) == 1);
}

fn check_overflow(res: usize, a: usize, mem: usize) -> bool {
    return ((res ^ a) & (res ^ mem) & 0x80) == 1;
}

fn check_carry(value: usize, bound: usize) -> bool {
    return (value >= bound);
}

// util
fn wrap_to_u8(val: usize) -> usize {
    return val & 0b1111_1111;
}


impl CPU {
    fn origin() -> CPU {
        CPU {
            memory : [0; 0xffff],
            reg_a  : 0 ,
            reg_x  : 0 ,
            reg_y  : 0 ,
            reg_p  : 0 ,
            reg_s  : 0 ,
            reg_n  : 0 ,
            reg_sp : 0xff ,
            reg_pc : 0x80,
        }
    }

    fn unset_flag(&mut self, flag: usize) -> usize {
        return clear_bit(self.reg_p as usize, flag);
    }
    
    fn set_flag(&mut self, flag: usize) -> usize {
        return set_bit(self.reg_p as usize, flag);
    }

    fn get_flag(&mut self, flag: usize) -> usize {
        return get_bit(self.reg_p as usize, flag);
    }
    
    fn get_address_val(&self, add: Address) -> u16 {

        let _val: u16 = match add.mode {
             MemoryType::memory => self.memory[add.val as usize] as u16,
            _ => add.val as u16,
        };

        return _val;

    }

    fn set_flag_var(&mut self, flag: usize, var: bool) {
        if var == true { 
            self.set_flag(flag);
        } else {
            self.unset_flag(flag);
        }
    }

    fn copy_flag(&mut self, flag: usize, val: u8) {
        self.reg_p = copy_bit(val, self.reg_p, flag);
    }

    fn push_to_stack(&mut self, val: u8) {
        self.memory[CPU_RAM_END - self.reg_sp as usize] = val;
        self.reg_sp -= 1;
    }

    fn pop_from_stack(&mut self) -> u8 {
        let res: u8 = self.memory[CPU_RAM_END - self.reg_sp as usize];
        self.reg_sp += 1;
        return res;
    }

    // ins
    fn ins_ADC(&mut self, add: Address) {
        let val = self.get_address_val(add) as u8;

        let new_a: usize = self.reg_a as usize + val as usize + self.get_flag(CF);
        assert!(new_a <= 255 + 255 + 1);

        self.set_flag_var(ZF, new_a == 0);
        self.set_flag_var(CF, check_carry(new_a, 0xff + 1));
        self.set_flag_var(OF, check_overflow(new_a, self.reg_a as usize, val as usize));
        self.set_flag_var(NF, check_negative(new_a as u8));

        self.reg_a = (new_a % 256) as u8;
    }

    fn ins_AND(&mut self, add: Address) {
        let val = self.get_address_val(add) as u8;

        self.reg_a = self.reg_a & val;

        self.set_flag_var(ZF, check_zero(self.reg_a));
        self.set_flag_var(NF, check_negative(self.reg_a));
    }

    fn ins_ASL(&mut self, add: Address) {
        let val = self.get_address_val(add) as u16;
        let res: u8 = self.reg_a << 1;

        self.set_flag_var(CF, check_carry(self.reg_a as usize, 0b1000_0000));
        self.set_flag_var(ZF, check_zero(res));
        self.set_flag_var(NF, check_negative(res));

        match add.mode {
            MemoryType::memory => self.memory[val as usize] = res,
            MemoryType::acc => self.reg_a = res,
            _ => (),
        }
    }

    fn ins_BCC(&mut self, add: Address) {
        let offset: i8 = self.get_address_val(add) as i8;
        let new_pc: i32 = (self.reg_pc as i32 + offset as i32);

        if self.get_flag(CF) == 0 {
            assert!(new_pc <= 255 * 255);
            assert!(new_pc as u16 >= 0);
            self.reg_pc = new_pc as u16;
        }
    }

    fn ins_BCS(&mut self, add: Address) {
        let offset: i8 = self.get_address_val(add) as i8;
        let new_pc: i32 = (self.reg_pc as i32 + offset as i32);

        if self.get_flag(CF) == 1 {
            assert!(new_pc <= 255 * 255);
            assert!(new_pc as u16 >= 0);
            self.reg_pc = new_pc as u16;
        }
    }

    fn ins_BEQ(&mut self, add: Address) {
        let offset: i8 = self.get_address_val(add) as i8;
        let new_pc: i32 = (self.reg_pc as i32 + offset as i32);

        if self.get_flag(ZF) == 1 {
            assert!(new_pc <= 255 * 255);
            assert!(new_pc as u16 >= 0);
            self.reg_pc = new_pc as u16;
        }
    }
    
    fn ins_BIT(&mut self, add: Address) {
        let val: u8 = self.get_address_val(add) as u8;
        let res: u8 = self.reg_a & val;

        self.set_flag_var(ZF, check_zero(res));
        self.copy_flag(OF, val);
        self.copy_flag(NF, val);
    }

    fn ins_BMI(&mut self, add: Address) {
        let offset: i8 = self.get_address_val(add) as i8;
        let new_pc: i32 = (self.reg_pc as i32 + offset as i32);

        if self.get_flag(NF) == 1 {
            assert!(new_pc <= 255 * 255);
            assert!(new_pc as u16 >= 0);
            self.reg_pc = new_pc as u16;
        }
    }

    fn ins_BNE(&mut self, add: Address) {
        let offset: i8 = self.get_address_val(add) as i8;
        let new_pc: i32 = (self.reg_pc as i32 + offset as i32);

        if self.get_flag(ZF) == 1 {
            assert!(new_pc <= 255 * 255);
            assert!(new_pc as u16 >= 0);
            self.reg_pc = new_pc as u16;
        }
    }

    fn ins_BPL(&mut self, add: Address) {
        let offset: i8 = self.get_address_val(add) as i8;
        let new_pc: i32 = (self.reg_pc as i32 + offset as i32);

        if self.get_flag(NF) == 0 {
            assert!(new_pc <= 255 * 255);
            assert!(new_pc as u16 >= 0);
            self.reg_pc = new_pc as u16;
        }
    }

     fn ins_BRK(&mut self, add: Address) {
         // TODO
     }

    fn ins_BVC(&mut self, add: Address) {
        let offset: i8 = self.get_address_val(add) as i8;
        let new_pc: i32 = (self.reg_pc as i32 + offset as i32);

        if self.get_flag(OF) == 1 {
            assert!(new_pc <= 255 * 255);
            assert!(new_pc as u16 >= 0);
            self.reg_pc = new_pc as u16;
        }
    }

    fn ins_BVS(&mut self, add: Address) {
        let offset: i8 = self.get_address_val(add) as i8;
        let new_pc: i32 = (self.reg_pc as i32 + offset as i32);

        if self.get_flag(OF) == 0 {
            assert!(new_pc <= 255 * 255);
            assert!(new_pc as u16 >= 0);
            self.reg_pc = new_pc as u16;
        }
    }

    fn ins_CLC(&mut self, _: Address) {
        self.unset_flag(CF);
    }

    fn ins_CLD(&mut self, _: Address) {
        self.unset_flag(DF);
    }

    fn ins_CLI(&mut self, _: Address) {
        self.unset_flag(IDF);
    }

    fn ins_CLV(&mut self, _: Address) {
        self.unset_flag(OF);
    }

    fn ins_CMP(&mut self, add: Address) {
        let val: u8 = self.get_address_val(add) as u8;

        self.set_flag_var(CF, self.reg_a >= val);
        self.set_flag_var(ZF, self.reg_a == val);
        self.set_flag_var(NF, val > self.reg_a);
    }

    fn ins_CPX(&mut self, add: Address) {
        let val: u8 = self.get_address_val(add) as u8;

        self.set_flag_var(CF, self.reg_x >= val);
        self.set_flag_var(ZF, self.reg_x == 0);
        self.set_flag_var(NF, val > self.reg_x);
    }

    fn ins_CPY(&mut self, add: Address) {
        let val: u8 = self.get_address_val(add) as u8;

        self.set_flag_var(CF, self.reg_y >= val);
        self.set_flag_var(ZF, self.reg_y == 0);
        self.set_flag_var(NF, val > self.reg_y);
    }

    fn ins_DEC(&mut self, add: Address) {
        let val: u8 = self.get_address_val(add) as u8;
        let mut res: u8 = 0xff;
        if val != 0 { 
            res = val - 1;
        }

        self.set_flag_var(ZF, res == 0);
        self.set_flag_var(NF, val == 0);

        self.memory[add.val as usize] = res;
    }

    fn ins_DEX(&mut self, _: Address) {
        let mut res: u8 = self.reg_x;

        let mut res: u8 = 0xff;
        if self.reg_x != 0 { 
            res = self.reg_x - 1;
        }

        self.set_flag_var(ZF, res == 0);
        self.set_flag_var(NF, self.reg_x == 0);

        self.reg_x = res;
    }

    fn ins_DEY(&mut self, _: Address) {
        let mut res: u8 = self.reg_y;

        let mut res: u8 = 0xff;
        if self.reg_y != 0 { 
            res = self.reg_y - 1;
        }

        self.set_flag_var(ZF, res == 0);
        self.set_flag_var(NF, self.reg_y == 0);

        self.reg_y = res;
    }

    fn ins_EOR(&mut self, add: Address) {
        let val: u8 = self.get_address_val(add) as u8;
        let res: u8 = self.reg_a & val;

        self.set_flag_var(ZF, res == 0);
        self.set_flag_var(NF, check_negative(res));

        self.reg_a = res;
    }

    fn ins_INC(&mut self, add: Address) {
        let val: u8 = self.get_address_val(add) as u8;
        
        let mut res: u8 = 0;
        if val != 0xff {
            res = val + 1;
        }

        self.set_flag_var(ZF, res==0);
        self.set_flag_var(NF, check_negative(res));

        self.memory[add.val as usize] = res;
    }

    fn ins_INX(&mut self, add: Address) {
        let val: u8 = self.reg_x;
        
        let mut res: u8 = 0;
        if self.reg_x != 0xff {
            res = self.reg_x + 1;
        }

        self.set_flag_var(ZF, res == 0);
        self.set_flag_var(NF, check_negative(res));

        self.reg_x = res;
    }

    fn ins_INY(&mut self, add: Address) {
        let val: u8 = self.reg_y;
        
        let mut res: u8 = 0;
        if self.reg_y != 0xff {
            res = self.reg_y + 1;
        }

        self.set_flag_var(ZF, res == 0);
        self.set_flag_var(NF, check_negative(res));

        self.reg_y = res;
    }


    fn ins_JMP(&mut self, add: Address) {
        let val: u16 = self.get_address_val(add);
        self.reg_pc = val;
    }

    fn ins_JSR(&mut self, add: Address) {
        let val: u16 = self.get_address_val(add);

        self.push_to_stack((self.reg_pc >> 8) as u8);
        self.push_to_stack(self.reg_pc as u8);

        self.reg_pc = val;
    }

    fn ins_LDA(&mut self, add: Address) {
        self.reg_a = self.get_address_val(add) as u8;

        self.set_flag_var(ZF, self.reg_a == 0);
        self.set_flag_var(NF, check_negative(self.reg_a));
    }

    fn ins_LDX(&mut self, add: Address) {
        self.reg_x = self.get_address_val(add) as u8;

        self.set_flag_var(ZF, self.reg_x == 0);
        self.set_flag_var(NF, check_negative(self.reg_x));
    }

    fn ins_LDY(&mut self, add: Address) {
        self.reg_y = self.get_address_val(add) as u8;

        self.set_flag_var(ZF, self.reg_y == 0);
        self.set_flag_var(NF, check_negative(self.reg_y));
    }

    fn ins_LSR(&mut self, add: Address) {
        let val = self.get_address_val(add) as u8;
        let res: u8 = val >> 1;

        self.set_flag_var(CF, (val & 1) == 1);
        self.set_flag_var(ZF, res == 0);
        self.unset_flag(NF);

        match add.mode {
            MemoryType::memory => self.memory[add.val as usize] = res,
            MemoryType::acc => self.reg_a = res,
            _ => (),
        }
    }

    fn ins_NOP(&mut self, _: Address) { }

    fn ins_ORA(&mut self, add: Address) {
        let val = self.get_address_val(add) as u8;
        let res: u8 = self.reg_a | val;

        self.set_flag_var(ZF, res == 0);
        self.set_flag_var(NF, check_negative(res));

        self.reg_a = val;
    }

    fn ins_PHA(&mut self, _: Address) {
        self.push_to_stack(self.reg_a);
    }

    fn ins_PHP(&mut self, _: Address) {
        self.push_to_stack(self.reg_p + 0b0011_0000);
    }

    fn ins_PLA(&mut self, _: Address) {
       let res: u8 = self.pop_from_stack();

        self.set_flag_var(ZF, res == 0);
        self.set_flag_var(NF, check_negative(res));

        self.reg_a = res;
    }

    fn ins_PLP(&mut self, _: Address) {
       let val: u8 = self.pop_from_stack();

        self.copy_flag(CF, val);
        self.copy_flag(ZF, val);
        self.copy_flag(IDF, val);
        self.copy_flag(DF, val);
        self.copy_flag(OF, val);
        self.copy_flag(NF, val);
    }

    fn ins_ROL(&mut self, add: Address) {
        let val = self.get_address_val(add) as u8;
        let res: u8 = val.rotate_left(1);

        self.set_flag_var(CF, (val & 1) == 1);
        self.set_flag_var(ZF, res == 0);
        self.set_flag_var(NF, (val & 7) == 1);

        match add.mode {
            MemoryType::memory => self.memory[add.val as usize] = res,
            MemoryType::acc => self.reg_a = res,
            _ => (),
        }
    }

    fn ins_ROR(&mut self, add: Address) {
        let val = self.get_address_val(add) as u8;
        let res: u8 = val.rotate_right(1);

        self.set_flag_var(CF, (val & 1) == 1);
        self.set_flag_var(ZF, res == 0);
        self.set_flag_var(NF, (val & 7) == 1);

        match add.mode {
            MemoryType::memory => self.memory[add.val as usize] = res,
            MemoryType::acc => self.reg_a = res,
            _ => (),
        }
    }

    fn ins_RTI(&mut self, _: Address) {
        let flags: u8 = self.pop_from_stack();
        let pcl: u8 = self.pop_from_stack();
        let pch: u8 = self.pop_from_stack();

        self.copy_flag(CF, flags);
        self.copy_flag(ZF, flags);
        self.copy_flag(IDF, flags);
        self.copy_flag(DF, flags);
        self.copy_flag(OF, flags);
        self.copy_flag(NF, flags);
        self.reg_pc = ((pch as u16) << 8) + (pcl as u16);
    }

    fn ins_RTS(&mut self, _: Address) {
        let pcl: u8 = self.pop_from_stack();
        let pch: u8 = self.pop_from_stack();

        self.reg_pc = ((pch as u16) << 8) + (pcl as u16) + 1;
    }

    fn ins_SDC(&mut self, add: Address) {
        let val = self.get_address_val(add) as u8;

        let new_a: isize = self.reg_a as isize - val as isize - !self.get_flag(CF) as isize;

        self.set_flag_var(CF, !(new_a < 0));
        self.set_flag_var(ZF, new_a == 0);
        self.set_flag_var(OF, ((new_a as u8 ^ self.reg_a) & (new_a as u8 ^ !val) & 0x80) == 1);
        self.set_flag_var(NF, check_negative(new_a as u8));

        self.reg_a = (new_a % 256) as u8;
    }

    fn ins_SEC(&mut self, _: Address) {
        self.set_flag(CF);
    }

    fn ins_SED(&mut self, _: Address) {
        self.set_flag(DF);
    }

    fn ins_SEI(&mut self, _: Address) {
        self.set_flag(IDF);
    }

    fn ins_STA(&mut self, add: Address) {
        self.memory[add.val as usize] = self.reg_a;
    }

    fn ins_STX(&mut self, add: Address) {
        self.memory[add.val as usize] = self.reg_x;
    }

    fn ins_STY(&mut self, add: Address) {
        self.memory[add.val as usize] = self.reg_y;
    }

    fn ins_TAX(&mut self, _: Address) {
        self.reg_x = self.reg_a;
        self.set_flag_var(ZF, self.reg_x == 0);
        self.set_flag_var(NF, check_negative(self.reg_x));
    }

    fn ins_TAY(&mut self, _: Address) {
        self.reg_y = self.reg_a;
        self.set_flag_var(ZF, self.reg_y == 0);
        self.set_flag_var(NF, check_negative(self.reg_y));
    }

    fn ins_TSX(&mut self, _: Address) {
        self.reg_x = self.reg_sp;
        self.set_flag_var(ZF, self.reg_x == 0);
        self.set_flag_var(NF, check_negative(self.reg_x));
    }

    fn ins_TXA(&mut self, _: Address) {
        self.reg_a = self.reg_x;
        self.set_flag_var(ZF, self.reg_a == 0);
        self.set_flag_var(NF, check_negative(self.reg_a));
    }

    fn ins_TXS(&mut self, _: Address) {
        self.reg_sp = self.reg_a;
    }

    fn ins_TYA(&mut self, _: Address) {
        self.reg_a = self.reg_y;
        self.set_flag_var(ZF, self.reg_a == 0);
        self.set_flag_var(NF, check_negative(self.reg_a));
    }

    fn get_address(&mut self, mode: AddressingMode) -> Address {
        
        let _mode: MemoryType = match mode {
            AddressingMode::acc => MemoryType::acc,
            AddressingMode::imm | AddressingMode::relative | AddressingMode::implied => MemoryType::literal,
            _ => MemoryType::memory,
        };

        let _val: u16 = match mode {
            AddressingMode::acc => self.reg_a as u16,
            // Uses the 8-bit operand itself as the value for the operation, rather than fetching a value from a memory address.
            AddressingMode::imm => self.pop_pc(),
            // Fetches the value from an 8-bit address on the zero page.
            AddressingMode::zp => self.memory[self.pop_pc() as usize] as u16,
            // val = PEEK((arg + X) % 256)
            AddressingMode::zpx => (self.pop_pc() + self.reg_x as u16) % 0xff,
            // val = PEEK((arg + Y) % 256)
            AddressingMode::zpy => (self.pop_pc() + self.reg_y as u16) % 0xff,
            // Fetches the value from a 16-bit address anywhere in memory.
            AddressingMode::abs => self.pop_pc() << 8 + self.pop_pc() as u16,
            // val = PEEK(arg + X)
            AddressingMode::absx => self.pop_pc() << 8 + self.pop_pc() + self.reg_x as u16,
            // val = PEEK(arg + Y)
            AddressingMode::absy => self.pop_pc() << 8 + self.pop_pc() + self.reg_y as u16,
            //  val = PEEK(PEEK((arg + X) % 256) + PEEK((arg + X + 1) % 256) * 256)
            AddressingMode::indx =>
                (
                    self.memory[((self.pop_pc() + self.reg_x as u16) % 0xff) as usize] as u16
                    +
                    (self.memory[((self.pop_pc() + self.reg_x as u16) % 0xff) as usize] as u16) << 8
                ),
            // val = PEEK(PEEK(arg) + PEEK((arg + 1) % 256) * 256 + Y)	
            AddressingMode::indy =>
                (
                    self.memory[self.pop_pc() as usize] as u16
                    +
                    (self.memory[self.pop_pc() as usize % 256] as u16) << 8
                ) + self.reg_y as u16,

            // Relative	Branch instructions (e.g. BEQ, BCS) have a relative addressing mode that specifies an 8-bit signed offset relative to the current PC.
            AddressingMode::relative => self.memory[self.pop_pc() as usize] as u16, // handled in instruction
            AddressingMode::implied => 0 as u16, //dummy
        };

        return Address { val: _val, mode: _mode };
    }

    // to Emulator struct
    fn read_cardridge(&mut self, path: &'static str) {
        let source = std::fs::read(path).unwrap();
        
        // Define the range where you want to copy into the destination array
        let program_len = source.len() - HEADER_SIZE;

        let crd_rom_st: usize = PRG_ROM_START;

        let cpu_rom_st: usize = CPU_PRG_START;
        let cpu_rom_end: usize = CPU_PRG_START + program_len;

        self.memory[cpu_rom_st..cpu_rom_end].copy_from_slice(&(source[crd_rom_st..]));
    }

    fn pop_pc(&mut self) -> u16 {
        let mut pc: u16 = self.reg_pc;
        self.reg_pc += 1;
        return pc;
    }

    fn run_instruction(&mut self) {
        // let op: usize = self.pop_pc() as usize;
        let op: usize = 0;
        let i = &INSTRUCTION_SET[op];
        let add: Address = self.get_address(i.mode);
        (i.fun)(self, add);
    }
}

const INSTRUCTION_SET: [Instruction; 151] = [
    Instruction{op: 0x65, mode: AddressingMode::imm, bytes: 2, cycles: 2, mnemonic: "ADC", fun: CPU::ins_ADC},
    Instruction{op: 0x69, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "ADC", fun: CPU::ins_ADC},
    Instruction{op: 0x75, mode: AddressingMode::zpx, bytes: 2, cycles: 4, mnemonic: "ADC", fun: CPU::ins_ADC},
    Instruction{op: 0x6D, mode: AddressingMode::abs, bytes: 3, cycles: 4, mnemonic: "ADC", fun: CPU::ins_ADC},
    Instruction{op: 0x7D, mode: AddressingMode::absx, bytes: 3, cycles: 4, mnemonic: "ADC", fun: CPU::ins_ADC},
    Instruction{op: 0x79, mode: AddressingMode::absy, bytes: 3, cycles: 4, mnemonic: "ADC", fun: CPU::ins_ADC},
    Instruction{op: 0x61, mode: AddressingMode::indx, bytes: 3, cycles: 6, mnemonic: "ADC", fun: CPU::ins_ADC},
    Instruction{op: 0x71, mode: AddressingMode::indy, bytes: 3, cycles: 6, mnemonic: "ADC", fun: CPU::ins_ADC},
    Instruction{op: 0x29, mode: AddressingMode::imm, bytes: 2, cycles: 2, mnemonic: "AND", fun: CPU::ins_AND},
    Instruction{op: 0x25, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "AND", fun: CPU::ins_AND},
    Instruction{op: 0x35, mode: AddressingMode::zpx, bytes: 2, cycles: 4, mnemonic: "AND", fun: CPU::ins_AND},
    Instruction{op: 0x2d, mode: AddressingMode::abs, bytes: 2, cycles: 4, mnemonic: "AND", fun: CPU::ins_AND},
    Instruction{op: 0x3d, mode: AddressingMode::absx, bytes: 2, cycles: 4, mnemonic: "AND", fun: CPU::ins_AND},
    Instruction{op: 0x39, mode: AddressingMode::absy, bytes: 2, cycles: 4, mnemonic: "AND", fun: CPU::ins_AND},
    Instruction{op: 0x21, mode: AddressingMode::indx, bytes: 2, cycles: 6, mnemonic: "AND", fun: CPU::ins_AND},
    Instruction{op: 0x31, mode: AddressingMode::indy, bytes: 2, cycles: 6, mnemonic: "AND", fun: CPU::ins_AND},
    Instruction{op: 0x0A, mode: AddressingMode::acc, bytes: 1, cycles: 2, mnemonic: "ASL", fun: CPU::ins_ASL},
    Instruction{op: 0x06, mode: AddressingMode::zp, bytes: 2, cycles: 5, mnemonic: "ASL", fun: CPU::ins_ASL},
    Instruction{op: 0x16, mode: AddressingMode::zpx, bytes: 2, cycles: 6, mnemonic: "ASL", fun: CPU::ins_ASL},
    Instruction{op: 0x0E, mode: AddressingMode::abs, bytes: 3, cycles: 6, mnemonic: "ASL", fun: CPU::ins_ASL},
    Instruction{op: 0x1E, mode: AddressingMode::absx, bytes: 3, cycles: 7, mnemonic: "ASL", fun: CPU::ins_ASL},
    Instruction{op: 0x90, mode: AddressingMode::relative, bytes: 2, cycles: 2, mnemonic: "BCC", fun: CPU::ins_BCC},
    Instruction{op: 0xB0, mode: AddressingMode::relative, bytes: 2, cycles: 2, mnemonic: "BCS", fun: CPU::ins_BCS},
    Instruction{op: 0xF0, mode: AddressingMode::relative, bytes: 2, cycles: 2, mnemonic: "BEQ", fun: CPU::ins_BEQ},
    Instruction{op: 0x24, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "BIT", fun: CPU::ins_BIT},
    Instruction{op: 0x2C, mode: AddressingMode::abs, bytes: 2, cycles: 4, mnemonic: "BIT", fun: CPU::ins_BIT},
    Instruction{op: 0x30, mode: AddressingMode::relative, bytes: 2, cycles: 2, mnemonic: "BMI", fun: CPU::ins_BMI},
    Instruction{op: 0xD0, mode: AddressingMode::relative, bytes: 2, cycles: 2, mnemonic: "BNE", fun: CPU::ins_BNE},
    Instruction{op: 0x10, mode: AddressingMode::relative, bytes: 2, cycles: 2, mnemonic: "BPL", fun: CPU::ins_BPL},
    Instruction{op: 0x00, mode: AddressingMode::imm, bytes: 2, cycles: 7, mnemonic: "BRK", fun: CPU::ins_BRK},
    Instruction{op: 0x50, mode: AddressingMode::relative, bytes: 2, cycles: 2, mnemonic: "BVC", fun: CPU::ins_BVC},
    Instruction{op: 0x80, mode: AddressingMode::relative, bytes: 2, cycles: 2, mnemonic: "BVS", fun: CPU::ins_BVS},
    Instruction{op: 0x18, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "CLC", fun: CPU::ins_CLC},
    Instruction{op: 0xD8, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "CLD", fun: CPU::ins_CLD},
    Instruction{op: 0x58, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "CLI", fun: CPU::ins_CLI},
    Instruction{op: 0xB8, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "CLV", fun: CPU::ins_CLV},
    Instruction{op: 0xC9, mode: AddressingMode::imm, bytes: 2, cycles: 2, mnemonic: "CLV", fun: CPU::ins_CLV},
    Instruction{op: 0xC5, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "CLV", fun: CPU::ins_CLV},
    Instruction{op: 0xD5, mode: AddressingMode::zpx, bytes: 2, cycles: 4, mnemonic: "CLV", fun: CPU::ins_CLV},
    Instruction{op: 0xCD, mode: AddressingMode::abs, bytes: 2, cycles: 4, mnemonic: "CLV", fun: CPU::ins_CLV},
    Instruction{op: 0xDD, mode: AddressingMode::absx, bytes: 3, cycles: 4, mnemonic: "CLV", fun: CPU::ins_CLV},
    Instruction{op: 0xD9, mode: AddressingMode::absy, bytes: 3, cycles: 4, mnemonic: "CLV", fun: CPU::ins_CLV},
    Instruction{op: 0xC1, mode: AddressingMode::indx, bytes: 2, cycles: 5, mnemonic: "CLV", fun: CPU::ins_CLV},
    Instruction{op: 0xD1, mode: AddressingMode::indy, bytes: 2, cycles: 6, mnemonic: "CLV", fun: CPU::ins_CLV},
    Instruction{op: 0xE0, mode: AddressingMode::imm, bytes: 2, cycles: 2, mnemonic: "CPX", fun: CPU::ins_CPX},
    Instruction{op: 0xE4, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "CPX", fun: CPU::ins_CPX},
    Instruction{op: 0xEC, mode: AddressingMode::abs, bytes: 3, cycles: 4, mnemonic: "CPX", fun: CPU::ins_CPX},
    Instruction{op: 0xC0, mode: AddressingMode::imm, bytes: 2, cycles: 2, mnemonic: "CPY", fun: CPU::ins_CPY},
    Instruction{op: 0xC4, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "CPY", fun: CPU::ins_CPY},
    Instruction{op: 0xCC, mode: AddressingMode::abs, bytes: 3, cycles: 4, mnemonic: "CPY", fun: CPU::ins_CPY},
    Instruction{op: 0xC6, mode: AddressingMode::zp, bytes: 2, cycles: 5, mnemonic: "DEC", fun: CPU::ins_DEC},
    Instruction{op: 0xD6, mode: AddressingMode::zpx, bytes: 2, cycles: 6, mnemonic: "DEC", fun: CPU::ins_DEC},
    Instruction{op: 0xCE, mode: AddressingMode::abs, bytes: 3, cycles: 6, mnemonic: "DEC", fun: CPU::ins_DEC},
    Instruction{op: 0xDE, mode: AddressingMode::absx, bytes: 3, cycles: 7, mnemonic: "DEC", fun: CPU::ins_DEC},
    Instruction{op: 0xCA, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "DEX", fun: CPU::ins_DEX},
    Instruction{op: 0x88, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "DEY", fun: CPU::ins_DEY},
    Instruction{op: 0x49, mode: AddressingMode::imm, bytes: 2, cycles: 2, mnemonic: "EOR", fun: CPU::ins_EOR},
    Instruction{op: 0x45, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "EOR", fun: CPU::ins_EOR},
    Instruction{op: 0x55, mode: AddressingMode::zpx, bytes: 2, cycles: 4, mnemonic: "EOR", fun: CPU::ins_EOR},
    Instruction{op: 0x4D, mode: AddressingMode::abs, bytes: 3, cycles: 4, mnemonic: "EOR", fun: CPU::ins_EOR},
    Instruction{op: 0x5D, mode: AddressingMode::absx, bytes: 3, cycles: 4, mnemonic: "EOR", fun: CPU::ins_EOR},
    Instruction{op: 0x59, mode: AddressingMode::absy, bytes: 3, cycles: 4, mnemonic: "EOR", fun: CPU::ins_EOR},
    Instruction{op: 0x41, mode: AddressingMode::indx, bytes: 2, cycles: 6, mnemonic: "EOR", fun: CPU::ins_EOR},
    Instruction{op: 0x51, mode: AddressingMode::indy, bytes: 2, cycles: 5, mnemonic: "EOR", fun: CPU::ins_EOR},
    Instruction{op: 0xE6, mode: AddressingMode::zp, bytes: 2, cycles: 5, mnemonic: "INC", fun: CPU::ins_INC},
    Instruction{op: 0xF6, mode: AddressingMode::zpx, bytes: 2, cycles: 6, mnemonic: "INC", fun: CPU::ins_INC},
    Instruction{op: 0xEE, mode: AddressingMode::abs, bytes: 3, cycles: 6, mnemonic: "INC", fun: CPU::ins_INC},
    Instruction{op: 0xFE, mode: AddressingMode::absx, bytes: 3, cycles: 7, mnemonic: "INC", fun: CPU::ins_INC},
    Instruction{op: 0xE8, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "INX", fun: CPU::ins_INX},
    Instruction{op: 0xC8, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "INY", fun: CPU::ins_INY},
    Instruction{op: 0x4C, mode: AddressingMode::abs, bytes: 3, cycles: 3, mnemonic: "JMP", fun: CPU::ins_JMP},
    Instruction{op: 0x6C, mode: AddressingMode::implied, bytes: 3, cycles: 5, mnemonic: "JMP", fun: CPU::ins_JMP},
    Instruction{op: 0x20, mode: AddressingMode::abs, bytes: 3, cycles: 6, mnemonic: "JSR", fun: CPU::ins_JSR},
    Instruction{op: 0xA9, mode: AddressingMode::imm, bytes: 2, cycles: 2, mnemonic: "LDA", fun: CPU::ins_LDA},
    Instruction{op: 0xA5, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "LDA", fun: CPU::ins_LDA},
    Instruction{op: 0xB5, mode: AddressingMode::zpx, bytes: 2, cycles: 4, mnemonic: "LDA", fun: CPU::ins_LDA},
    Instruction{op: 0xAD, mode: AddressingMode::abs, bytes: 3, cycles: 4, mnemonic: "LDA", fun: CPU::ins_LDA},
    Instruction{op: 0xBD, mode: AddressingMode::absx, bytes: 3, cycles: 4, mnemonic: "LDA", fun: CPU::ins_LDA},
    Instruction{op: 0xB9, mode: AddressingMode::absy, bytes: 3, cycles: 4, mnemonic: "LDA", fun: CPU::ins_LDA},
    Instruction{op: 0xA1, mode: AddressingMode::indx, bytes: 2, cycles: 6, mnemonic: "LDA", fun: CPU::ins_LDA},
    Instruction{op: 0xB1, mode: AddressingMode::indy, bytes: 2, cycles: 5, mnemonic: "LDA", fun: CPU::ins_LDA},
    Instruction{op: 0xA2, mode: AddressingMode::imm, bytes: 2, cycles: 2, mnemonic: "LDX", fun: CPU::ins_LDX},
    Instruction{op: 0xA6, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "LDX", fun: CPU::ins_LDX},
    Instruction{op: 0xB6, mode: AddressingMode::zpy, bytes: 2, cycles: 4, mnemonic: "LDX", fun: CPU::ins_LDX},
    Instruction{op: 0xAE, mode: AddressingMode::abs, bytes: 3, cycles: 4, mnemonic: "LDX", fun: CPU::ins_LDX},
    Instruction{op: 0xBE, mode: AddressingMode::absy, bytes: 3, cycles: 4, mnemonic: "LDX", fun: CPU::ins_LDX},
    Instruction{op: 0xA0, mode: AddressingMode::imm, bytes: 2, cycles: 2, mnemonic: "LDY", fun: CPU::ins_LDY},
    Instruction{op: 0xA4, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "LDY", fun: CPU::ins_LDY},
    Instruction{op: 0xB4, mode: AddressingMode::zpx, bytes: 2, cycles: 4, mnemonic: "LDY", fun: CPU::ins_LDY},
    Instruction{op: 0xAC, mode: AddressingMode::abs, bytes: 3, cycles: 4, mnemonic: "LDY", fun: CPU::ins_LDY},
    Instruction{op: 0xBC, mode: AddressingMode::absx, bytes: 3, cycles: 4, mnemonic: "LDY", fun: CPU::ins_LDY},
    Instruction{op: 0x4A, mode: AddressingMode::acc, bytes: 1, cycles: 2, mnemonic: "LSR", fun: CPU::ins_LSR},
    Instruction{op: 0x46, mode: AddressingMode::zp, bytes: 2, cycles: 5, mnemonic: "LSR", fun: CPU::ins_LSR},
    Instruction{op: 0x56, mode: AddressingMode::zpx, bytes: 2, cycles: 6, mnemonic: "LSR", fun: CPU::ins_LSR},
    Instruction{op: 0x4E, mode: AddressingMode::abs, bytes: 3, cycles: 6, mnemonic: "LSR", fun: CPU::ins_LSR},
    Instruction{op: 0x5E, mode: AddressingMode::absx, bytes: 3, cycles: 7, mnemonic: "LSR", fun: CPU::ins_LSR},
    Instruction{op: 0xEA, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "NOP", fun: CPU::ins_NOP},
    Instruction{op: 0x09, mode: AddressingMode::imm, bytes: 2, cycles: 2, mnemonic: "ORA", fun: CPU::ins_ORA},
    Instruction{op: 0x05, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "ORA", fun: CPU::ins_ORA},
    Instruction{op: 0x15, mode: AddressingMode::zpx, bytes: 2, cycles: 4, mnemonic: "ORA", fun: CPU::ins_ORA},
    Instruction{op: 0x0D, mode: AddressingMode::abs, bytes: 3, cycles: 4, mnemonic: "ORA", fun: CPU::ins_ORA},
    Instruction{op: 0x1D, mode: AddressingMode::absx, bytes: 3, cycles: 4, mnemonic: "ORA", fun: CPU::ins_ORA},
    Instruction{op: 0x19, mode: AddressingMode::absy, bytes: 3, cycles: 4, mnemonic: "ORA", fun: CPU::ins_ORA},
    Instruction{op: 0x01, mode: AddressingMode::indx, bytes: 2, cycles: 6, mnemonic: "ORA", fun: CPU::ins_ORA},
    Instruction{op: 0x11, mode: AddressingMode::indy, bytes: 2, cycles: 5, mnemonic: "ORA", fun: CPU::ins_ORA},
    Instruction{op: 0x48, mode: AddressingMode::implied, bytes: 1, cycles: 3, mnemonic: "PHA", fun: CPU::ins_PHA},
    Instruction{op: 0x08, mode: AddressingMode::implied, bytes: 1, cycles: 3, mnemonic: "PHP", fun: CPU::ins_PHP},
    Instruction{op: 0x68, mode: AddressingMode::implied, bytes: 1, cycles: 4, mnemonic: "PLA", fun: CPU::ins_PLA},
    Instruction{op: 0x28, mode: AddressingMode::implied, bytes: 1, cycles: 4, mnemonic: "PLP", fun: CPU::ins_PLP},
    Instruction{op: 0x2A, mode: AddressingMode::acc, bytes: 1, cycles: 2, mnemonic: "ROL", fun: CPU::ins_ROL},
    Instruction{op: 0x26, mode: AddressingMode::zp, bytes: 2, cycles: 5, mnemonic: "ROL", fun: CPU::ins_ROL},
    Instruction{op: 0x36, mode: AddressingMode::zpx, bytes: 2, cycles: 6, mnemonic: "ROL", fun: CPU::ins_ROL},
    Instruction{op: 0x2E, mode: AddressingMode::abs, bytes: 3, cycles: 6, mnemonic: "ROL", fun: CPU::ins_ROL},
    Instruction{op: 0x3E, mode: AddressingMode::absx, bytes: 3, cycles: 7, mnemonic: "ROL", fun: CPU::ins_ROL},
    Instruction{op: 0x6A, mode: AddressingMode::acc, bytes: 1, cycles: 2, mnemonic: "ROR", fun: CPU::ins_ROR},
    Instruction{op: 0x66, mode: AddressingMode::zp, bytes: 2, cycles: 5, mnemonic: "ROR", fun: CPU::ins_ROR},
    Instruction{op: 0x76, mode: AddressingMode::zpx, bytes: 2, cycles: 6, mnemonic: "ROR", fun: CPU::ins_ROR},
    Instruction{op: 0x6E, mode: AddressingMode::abs, bytes: 3, cycles: 6, mnemonic: "ROR", fun: CPU::ins_ROR},
    Instruction{op: 0x7E, mode: AddressingMode::absx, bytes: 3, cycles: 7, mnemonic: "ROR", fun: CPU::ins_ROR},
    Instruction{op: 0x40, mode: AddressingMode::implied, bytes: 1, cycles: 6, mnemonic: "RTI", fun: CPU::ins_RTI},
    Instruction{op: 0x60, mode: AddressingMode::implied, bytes: 1, cycles: 6, mnemonic: "RTS", fun: CPU::ins_RTS},
    Instruction{op: 0xE9, mode: AddressingMode::imm, bytes: 2, cycles: 2, mnemonic: "SDC", fun: CPU::ins_SDC},
    Instruction{op: 0xE5, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "SDC", fun: CPU::ins_SDC},
    Instruction{op: 0xF5, mode: AddressingMode::zpx, bytes: 2, cycles: 4, mnemonic: "SDC", fun: CPU::ins_SDC},
    Instruction{op: 0xED, mode: AddressingMode::abs, bytes: 3, cycles: 4, mnemonic: "SDC", fun: CPU::ins_SDC},
    Instruction{op: 0xFD, mode: AddressingMode::absx, bytes: 3, cycles: 4, mnemonic: "SDC", fun: CPU::ins_SDC},
    Instruction{op: 0xF9, mode: AddressingMode::absy, bytes: 3, cycles: 4, mnemonic: "SDC", fun: CPU::ins_SDC},
    Instruction{op: 0xE1, mode: AddressingMode::indx, bytes: 2, cycles: 6, mnemonic: "SDC", fun: CPU::ins_SDC},
    Instruction{op: 0xF1, mode: AddressingMode::indy, bytes: 2, cycles: 5, mnemonic: "SDC", fun: CPU::ins_SDC},
    Instruction{op: 0x38, mode: AddressingMode::indy, bytes: 1, cycles: 2, mnemonic: "SEC", fun: CPU::ins_SEC},
    Instruction{op: 0xF8, mode: AddressingMode::indy, bytes: 1, cycles: 2, mnemonic: "SED", fun: CPU::ins_SED},
    Instruction{op: 0x78, mode: AddressingMode::indy, bytes: 1, cycles: 2, mnemonic: "SEI", fun: CPU::ins_SEI},
    Instruction{op: 0x85, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "STA", fun: CPU::ins_STA},
    Instruction{op: 0x95, mode: AddressingMode::zpx, bytes: 2, cycles: 4, mnemonic: "STA", fun: CPU::ins_STA},
    Instruction{op: 0x8D, mode: AddressingMode::abs, bytes: 3, cycles: 4, mnemonic: "STA", fun: CPU::ins_STA},
    Instruction{op: 0x9D, mode: AddressingMode::absx, bytes: 3, cycles: 5, mnemonic: "STA", fun: CPU::ins_STA},
    Instruction{op: 0x99, mode: AddressingMode::absy, bytes: 2, cycles: 5, mnemonic: "STA", fun: CPU::ins_STA},
    Instruction{op: 0x81, mode: AddressingMode::indx, bytes: 2, cycles: 6, mnemonic: "STA", fun: CPU::ins_STA},
    Instruction{op: 0x91, mode: AddressingMode::indy, bytes: 2, cycles: 6, mnemonic: "STA", fun: CPU::ins_STA},
    Instruction{op: 0x86, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "STX", fun: CPU::ins_STX},
    Instruction{op: 0x96, mode: AddressingMode::zpy, bytes: 2, cycles: 4, mnemonic: "STX", fun: CPU::ins_STX},
    Instruction{op: 0x8E, mode: AddressingMode::abs, bytes: 3, cycles: 4, mnemonic: "STX", fun: CPU::ins_STX},
    Instruction{op: 0x84, mode: AddressingMode::zp, bytes: 2, cycles: 3, mnemonic: "STY", fun: CPU::ins_STY},
    Instruction{op: 0x94, mode: AddressingMode::zpx, bytes: 2, cycles: 4, mnemonic: "STY", fun: CPU::ins_STY},
    Instruction{op: 0x8C, mode: AddressingMode::abs, bytes: 3, cycles: 4, mnemonic: "STY", fun: CPU::ins_STY},
    Instruction{op: 0xAA, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "TAX", fun: CPU::ins_TAX},
    Instruction{op: 0xA8, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "TAY", fun: CPU::ins_TAY},
    Instruction{op: 0xBA, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "TSX", fun: CPU::ins_TSX},
    Instruction{op: 0x8A, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "TXA", fun: CPU::ins_TXA},
    Instruction{op: 0x9A, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "TXS", fun: CPU::ins_TXS},
    Instruction{op: 0x98, mode: AddressingMode::implied, bytes: 1, cycles: 2, mnemonic: "TXA", fun: CPU::ins_TYA},
];


fn main() {
    let mut cpu = CPU::origin(); 
    println!("Hello! \n");
    cpu.read_cardridge("src/rom.nes");

    println!("value of X register: {}", cpu.reg_x);
    cpu.run_instruction();
    // let (mut rl, thread) = raylib::init()
    //     .size(640, 480)
    //     .title("Hello, World")
    //     .build();

    println!("reg p: {:#b}", cpu.reg_p);
    // while !rl.window_should_close() {
    //     let mut d = rl.begin_drawing(&thread);

    //     d.clear_background(Color::WHITE);
    //     d.draw_text("Hello, world!", 12, 12, 20, Color::BLACK);
    // }
}
