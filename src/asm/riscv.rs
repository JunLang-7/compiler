use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg {
    Zero,
    Ra,
    Sp,
    Gp,
    Tp,
    T0,
    T1,
    T2,
    S0,
    S1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    T3,
    T4,
    T5,
    T6,
}

impl Reg {
    /// Get register from its index
    pub fn from_index(index: i32) -> Self {
        match index {
            0 => Reg::Zero,
            1 => Reg::Ra,
            2 => Reg::Sp,
            3 => Reg::Gp,
            4 => Reg::Tp,
            5 => Reg::T0,
            6 => Reg::T1,
            7 => Reg::T2,
            8 => Reg::S0,
            9 => Reg::S1,
            10 => Reg::A0,
            11 => Reg::A1,
            12 => Reg::A2,
            13 => Reg::A3,
            14 => Reg::A4,
            15 => Reg::A5,
            16 => Reg::A6,
            17 => Reg::A7,
            18 => Reg::S2,
            19 => Reg::S3,
            20 => Reg::S4,
            21 => Reg::S5,
            22 => Reg::S6,
            23 => Reg::S7,
            24 => Reg::S8,
            25 => Reg::S9,
            26 => Reg::S10,
            27 => Reg::S11,
            28 => Reg::T3,
            29 => Reg::T4,
            30 => Reg::T5,
            31 => Reg::T6,
            _ => panic!("Invalid register index: {}", index),
        }
    }

    /// Check if the register is callee-saved
    pub fn is_callee_saved(&self) -> bool {
        matches!(
            self,
            Reg::S0
                | Reg::S1
                | Reg::S2
                | Reg::S3
                | Reg::S4
                | Reg::S5
                | Reg::S6
                | Reg::S7
                | Reg::S8
                | Reg::S9
                | Reg::S10
                | Reg::S11
        )
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            Reg::Zero => "zero",
            Reg::Ra => "ra",
            Reg::Sp => "sp",
            Reg::Gp => "gp",
            Reg::Tp => "tp",
            Reg::T0 => "t0",
            Reg::T1 => "t1",
            Reg::T2 => "t2",
            Reg::T3 => "t3",
            Reg::T4 => "t4",
            Reg::T5 => "t5",
            Reg::T6 => "t6",
            Reg::S0 => "s0",
            Reg::S1 => "s1",
            Reg::S2 => "s2",
            Reg::S3 => "s3",
            Reg::S4 => "s4",
            Reg::S5 => "s5",
            Reg::S6 => "s6",
            Reg::S7 => "s7",
            Reg::S8 => "s8",
            Reg::S9 => "s9",
            Reg::S10 => "s10",
            Reg::S11 => "s11",
            Reg::A0 => "a0",
            Reg::A1 => "a1",
            Reg::A2 => "a2",
            Reg::A3 => "a3",
            Reg::A4 => "a4",
            Reg::A5 => "a5",
            Reg::A6 => "a6",
            Reg::A7 => "a7",
        };
        write!(f, "{}", name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    // 算术运算 (rd, rs1, rs2)
    Add(Reg, Reg, Reg),
    Sub(Reg, Reg, Reg),
    Mul(Reg, Reg, Reg),
    Div(Reg, Reg, Reg),
    Rem(Reg, Reg, Reg),
    And(Reg, Reg, Reg),
    Or(Reg, Reg, Reg),
    Xor(Reg, Reg, Reg),
    Sll(Reg, Reg, Reg),
    Srl(Reg, Reg, Reg),
    Sra(Reg, Reg, Reg),
    Slt(Reg, Reg, Reg),
    Sgt(Reg, Reg, Reg), // Pseudo: sgt rd, rs1, rs2 -> slt rd, rs2, rs1
    Seqz(Reg, Reg),     // Pseudo: seqz rd, rs
    Snez(Reg, Reg),     // Pseudo: snez rd, rs

    // 立即数运算
    Addi(Reg, Reg, i32),
    Slli(Reg, Reg, i32), // 用于数组地址计算优化

    // 伪指令 / 传输
    Li(Reg, i32),    // li rd, imm
    La(Reg, String), // la rd, symbol
    Mv(Reg, Reg),    // mv rd, rs

    // 内存访问 (rd/src, base, offset)
    Lw(Reg, Reg, i32),
    Sw(Reg, Reg, i32),

    // 控制流
    Bnez(Reg, String), // bnez cond, label
    J(String),         // j label
    Call(String),      // call symbol
    Ret,               // ret
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Inst::Add(rd, rs1, rs2) => write!(f, "\tadd {}, {}, {}", rd, rs1, rs2),
            Inst::Sub(rd, rs1, rs2) => write!(f, "\tsub {}, {}, {}", rd, rs1, rs2),
            Inst::Mul(rd, rs1, rs2) => write!(f, "\tmul {}, {}, {}", rd, rs1, rs2),
            Inst::Div(rd, rs1, rs2) => write!(f, "\tdiv {}, {}, {}", rd, rs1, rs2),
            Inst::Rem(rd, rs1, rs2) => write!(f, "\trem {}, {}, {}", rd, rs1, rs2),
            Inst::Slt(rd, rs1, rs2) => write!(f, "\tslt {}, {}, {}", rd, rs1, rs2),
            Inst::Sgt(rd, rs1, rs2) => write!(f, "\tsgt {}, {}, {}", rd, rs1, rs2),
            Inst::Seqz(rd, rs) => write!(f, "\tseqz {}, {}", rd, rs),
            Inst::Snez(rd, rs) => write!(f, "\tsnez {}, {}", rd, rs),
            Inst::And(rd, rs1, rs2) => write!(f, "\tand {}, {}, {}", rd, rs1, rs2),
            Inst::Or(rd, rs1, rs2) => write!(f, "\tor {}, {}, {}", rd, rs1, rs2),
            Inst::Xor(rd, rs1, rs2) => write!(f, "\txor {}, {}, {}", rd, rs1, rs2),
            Inst::Sll(rd, rs1, rs2) => write!(f, "\tsll {}, {}, {}", rd, rs1, rs2),
            Inst::Srl(rd, rs1, rs2) => write!(f, "\tsrl {}, {}, {}", rd, rs1, rs2),
            Inst::Sra(rd, rs1, rs2) => write!(f, "\tsra {}, {}, {}", rd, rs1, rs2),

            Inst::Addi(rd, rs, imm) => write!(f, "\taddi {}, {}, {}", rd, rs, imm),
            Inst::Slli(rd, rs, imm) => write!(f, "\tslli {}, {}, {}", rd, rs, imm),

            Inst::Li(rd, imm) => write!(f, "\tli {}, {}", rd, imm),
            Inst::La(rd, label) => write!(f, "\tla {}, {}", rd, label),
            Inst::Mv(rd, rs) => write!(f, "\tmv {}, {}", rd, rs),

            Inst::Lw(rd, base, off) => write!(f, "\tlw {}, {}({})", rd, off, base),
            Inst::Sw(src, base, off) => write!(f, "\tsw {}, {}({})", src, off, base),

            Inst::Bnez(cond, label) => write!(f, "\tbnez {}, {}", cond, label),
            Inst::J(label) => write!(f, "\tj {}", label),
            Inst::Call(label) => write!(f, "\tcall {}", label),
            Inst::Ret => write!(f, "\tret"),
        }
    }
}

#[derive(Debug)]
pub struct RiscvBlock {
    pub label: String,
    pub insts: Vec<Inst>,
}

#[derive(Debug)]
pub struct RiscvFunc {
    pub name: String,
    pub blocks: Vec<RiscvBlock>,
    #[allow(unused)]
    pub stack_size: i32,
}

#[derive(Debug)]
pub struct RiscvProg {
    pub data_sec: Vec<String>,
    pub text_sec: Vec<RiscvFunc>,
}

impl RiscvProg {
    pub fn new() -> Self {
        Self {
            data_sec: Vec::new(),
            text_sec: Vec::new(),
        }
    }
}

// 实现打印整个程序的 Display
impl fmt::Display for RiscvProg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // 打印 .data
        writeln!(f, "\t.data")?;
        for line in &self.data_sec {
            writeln!(f, "{}", line)?;
        }
        writeln!(f, "")?;

        // 打印 .text
        for func in &self.text_sec {
            writeln!(f, "\t.text")?;
            writeln!(f, "\t.globl {}", func.name)?;
            writeln!(f, "{}:", func.name)?;

            for block in &func.blocks {
                if !block.label.is_empty() {
                    writeln!(f, "{}:", block.label)?;
                }
                for inst in &block.insts {
                    writeln!(f, "{}", inst)?;
                }
            }
            writeln!(f, "")?;
        }
        Ok(())
    }
}
