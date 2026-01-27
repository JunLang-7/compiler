use core::panic;
use koopa::ir::{BasicBlock, BinaryOp, FunctionData, Program, Value, ValueKind};
use std::{
    collections::HashMap,
    io::{Result, Write},
};

/// A trait to generate asm file base on Koopa IR
pub trait GenerateAsm {
    fn generate(&self, writer: &mut dyn Write) -> Result<()>;
}

impl GenerateAsm for Program {
    fn generate(&self, writer: &mut dyn Write) -> Result<()> {
        writeln!(writer, "\t.text")?;
        for &func in self.func_layout() {
            self.func(func).generate(writer)?;
        }
        Ok(())
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self, writer: &mut dyn Write) -> Result<()> {
        let name = self.name().trim_start_matches('@');
        writeln!(writer, "\t.globl {}", name)?;
        writeln!(writer, "{}:", name)?;

        let mut asm_gen = AsmGen::new(writer, self);
        asm_gen.generate()?;

        Ok(())
    }
}

/// Asm generator
pub struct AsmGen<'a> {
    writer: &'a mut dyn Write,
    func_data: &'a FunctionData,
    // inst -> stack offset
    stack_map: HashMap<Value, i32>,
    stack_size: i32,
}

impl<'a> AsmGen<'a> {
    /// Create a new AsmGen instance
    pub fn new(writer: &'a mut dyn Write, func_data: &'a FunctionData) -> Self {
        let mut stack_map = HashMap::new();
        let mut offset = 0;
        for (&_bb, node) in func_data.layout().bbs() {
            for &inst in node.insts().keys() {
                let val_data = func_data.dfg().value(inst);
                // 只有返回值非空时才分配内存空间
                if !val_data.ty().is_unit() {
                    stack_map.insert(inst, offset);
                    offset += 4;
                }
            }
        }
        let stack_size = (offset + 15) & !15; // align to 16 bytes
        Self {
            writer,
            func_data,
            stack_map,
            stack_size,
        }
    }

    fn load_to_reg(&mut self, val: Value, reg: &str) -> Result<()> {
        let val_data = self.func_data.dfg().value(val);
        match val_data.kind() {
            ValueKind::Integer(int) => {
                writeln!(self.writer, "\tli {}, {}", reg, int.value())?;
            }
            _ => {
                if let Some(&offset) = self.stack_map.get(&val) {
                    if offset < -2048 || offset > 2047 {
                        writeln!(self.writer, "\tli {}, {}", reg, offset)?;
                        writeln!(self.writer, "\tadd {}, sp, {}", reg, reg)?;
                        writeln!(self.writer, "\tlw {}, 0({})", reg, reg)?;
                    } else {
                        writeln!(self.writer, "\tlw {}, {}(sp)", reg, offset)?;
                    }
                } else {
                    panic!("Value {:?} not found in stack map", val);
                }
            }
        }
        Ok(())
    }

    fn store_from_reg(&mut self, val: Value, reg: &str) -> Result<()> {
        if let Some(&offset) = self.stack_map.get(&val) {
            if offset < -2048 || offset > 2047 {
                writeln!(self.writer, "\tli t3, {}", offset)?;
                writeln!(self.writer, "\tadd t3, sp, t3")?;
                writeln!(self.writer, "\tsw {}, 0(t3)", reg)?;
            } else {
                writeln!(self.writer, "\tsw {}, {}(sp)", reg, offset)?;
            }
        }
        Ok(())
    }

    /// Generate asm code for the function
    pub fn generate(&mut self) -> Result<()> {
        // prologue
        if self.stack_size > 0 {
            writeln!(self.writer, "\taddi sp, sp, -{}", self.stack_size)?;
        }
        for (&bb, node) in self.func_data.layout().bbs() {
            let label = self.get_bb_label(bb);
            writeln!(self.writer, "{}:", label)?;
            for &inst in node.insts().keys() {
                let value_data = self.func_data.dfg().value(inst);
                match value_data.kind() {
                    ValueKind::Integer(_) => {}
                    ValueKind::Return(ret) => {
                        if let Some(ret_val) = ret.value() {
                            self.load_to_reg(ret_val, "a0")?;
                        }
                        // epilogue
                        if self.stack_size > 0 {
                            writeln!(self.writer, "\taddi sp, sp, {}", self.stack_size)?;
                        }
                        writeln!(self.writer, "\tret")?;
                    }
                    ValueKind::Binary(bin) => {
                        // 获取操作数的寄存器
                        self.load_to_reg(bin.lhs(), "t0")?;
                        self.load_to_reg(bin.rhs(), "t1")?;

                        match bin.op() {
                            BinaryOp::Add => writeln!(self.writer, "\tadd t0, t0, t1")?,
                            BinaryOp::Sub => writeln!(self.writer, "\tsub t0, t0, t1")?,
                            BinaryOp::Mul => writeln!(self.writer, "\tmul t0, t0, t1")?,
                            BinaryOp::Div => writeln!(self.writer, "\tdiv t0, t0, t1")?,
                            BinaryOp::Mod => writeln!(self.writer, "\trem t0, t0, t1")?,
                            BinaryOp::Lt => writeln!(self.writer, "\tslt t0, t0, t1")?,
                            BinaryOp::Gt => writeln!(self.writer, "\tsgt t0, t0, t1")?,
                            BinaryOp::Le => {
                                writeln!(self.writer, "\tsgt t0, t0, t1")?;
                                writeln!(self.writer, "\tseqz t0, t0")?;
                            }
                            BinaryOp::Ge => {
                                writeln!(self.writer, "\tslt t0, t0, t1")?;
                                writeln!(self.writer, "\tseqz t0, t0")?;
                            }
                            BinaryOp::And => writeln!(self.writer, "\tand t0, t0, t1")?,
                            BinaryOp::Or => writeln!(self.writer, "\tor t0, t0, t1")?,
                            BinaryOp::Eq => {
                                writeln!(self.writer, "\txor t0, t0, t1")?;
                                writeln!(self.writer, "\tseqz t0, t0")?;
                            }
                            BinaryOp::NotEq => {
                                writeln!(self.writer, "\txor t0, t0, t1")?;
                                writeln!(self.writer, "\tsnez t0, t0")?;
                            }
                            _ => unreachable!(),
                        }
                        self.store_from_reg(inst, "t0")?;
                    }
                    ValueKind::Alloc(_) => {}
                    ValueKind::Load(load) => {
                        self.load_to_reg(load.src(), "t0")?;
                        self.store_from_reg(inst, "t0")?;
                    }
                    ValueKind::Store(store) => {
                        self.load_to_reg(store.value(), "t0")?;
                        self.store_from_reg(store.dest(), "t0")?;
                    }
                    ValueKind::Branch(branch) => {
                        let cond = branch.cond();
                        self.load_to_reg(cond, "t0")?;
                        writeln!(
                            self.writer,
                            "\tbnez t0, {}",
                            self.get_bb_label(branch.true_bb())
                        )?;
                        writeln!(self.writer, "\tj {}", self.get_bb_label(branch.false_bb()))?;
                    }
                    ValueKind::Jump(jmp) => {
                        writeln!(self.writer, "\tj {}", self.get_bb_label(jmp.target()))?;
                    }
                    _ => unreachable!(),
                }
            }
        }
        Ok(())
    }

    /// Get the label of a basic block
    fn get_bb_label(&self, bb: BasicBlock) -> String {
        let bb_data = self.func_data.dfg().bb(bb);
        if let Some(name) = bb_data.name() {
            name.trim_start_matches("%").to_string()
        } else {
            "unknown".to_string()
        }
    }
}
