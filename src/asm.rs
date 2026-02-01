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
        writeln!(writer, "\t.data")?;
        for &val in self.inst_layout() {
            let data = self.borrow_value(val);
            if let ValueKind::GlobalAlloc(alloc) = data.kind() {
                let name = data.name().as_deref().unwrap().trim_start_matches('@');
                writeln!(writer, "\t.global {}", name)?;
                writeln!(writer, "{}:", name)?;
                // 处理初值
                let init = alloc.init();
                match self.borrow_value(init).kind() {
                    ValueKind::Integer(int) => {
                        writeln!(writer, "\t.word {}", int.value())?;
                    }
                    ValueKind::ZeroInit(_) => {
                        writeln!(writer, "\t.zero 4")?;
                    }
                    _ => {}
                }
            }
        }
        writeln!(writer, "")?;

        for &func in self.func_layout() {
            let func_data = self.func(func);
            if func_data.layout().entry_bb().is_none() {
                continue; // skip declarations
            }
            let name = func_data.name().trim_start_matches('@');
            writeln!(writer, "\t.text")?;
            writeln!(writer, "\t.globl {}", name)?;
            writeln!(writer, "{}:", name)?;

            let mut asm_gen = AsmGen::new(writer, self, func_data);
            asm_gen.generate()?;
        }
        Ok(())
    }
}

/// Asm generator
pub struct AsmGen<'a> {
    writer: &'a mut dyn Write,
    program: &'a Program,
    func_data: &'a FunctionData,
    // inst -> stack offset
    stack_map: HashMap<Value, i32>,
    stack_size: i32,
    has_call: bool,
}

impl<'a> AsmGen<'a> {
    /// Create a new AsmGen instance
    pub fn new(
        writer: &'a mut dyn Write,
        program: &'a Program,
        func_data: &'a FunctionData,
    ) -> Self {
        let mut stack_map = HashMap::new();
        let mut has_call = false;
        let mut max_call_args = 0;

        // 先扫描一遍，计算溢出参数空间
        for (&_bb, node) in func_data.layout().bbs() {
            for &inst in node.insts().keys() {
                let val_data = func_data.dfg().value(inst);
                if let ValueKind::Call(call) = val_data.kind() {
                    has_call = true;
                    max_call_args = max_call_args.max(call.args().len() as i32);
                }
            }
        }
        // 计算底部预留空间
        let overflow_size = if max_call_args > 8 {
            (max_call_args - 8) * 4
        } else {
            0
        };
        let mut offset = overflow_size;
        // 为函数参数分配栈空间
        for (i, &parm) in func_data.params().iter().enumerate() {
            if i < 8 {
                stack_map.insert(parm, offset);
                offset += 4;
            }
        }
        // 为指令分配栈空间
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
        // 如果函数中有调用，则需要为保存ra寄存器分配空间
        if has_call {
            offset += 4;
        }
        let stack_size = (offset + 15) & !15; // align to 16 bytes
        // 为溢出参数分配空间
        for (i, &parm) in func_data.params().iter().enumerate() {
            if i >= 8 {
                let parm_offset = stack_size + ((i as i32 - 8) * 4);
                stack_map.insert(parm, parm_offset);
            }
        }
        Self {
            writer,
            program,
            func_data,
            stack_map,
            stack_size,
            has_call,
        }
    }

    fn load_to_reg(&mut self, val: Value, reg: &str) -> Result<()> {
        if self.func_data.dfg().values().contains_key(&val) {
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
        } else {
            let global_data = self.program.borrow_value(val);
            match global_data.kind() {
                ValueKind::GlobalAlloc(_) => {
                    let name = global_data
                        .name()
                        .as_deref()
                        .unwrap()
                        .trim_start_matches('@');
                    writeln!(self.writer, "\tla {}, {}", reg, name)?;
                    writeln!(self.writer, "\tlw {}, 0({})", reg, reg)?;
                }
                _ => panic!("Unknown value source"),
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
        } else {
            let global_data = self.program.borrow_value(val);
            if let ValueKind::GlobalAlloc(_) = global_data.kind() {
                let name = global_data
                    .name()
                    .as_deref()
                    .unwrap()
                    .trim_start_matches('@');
                writeln!(self.writer, "\tla t3, {}", name)?;
                writeln!(self.writer, "\tsw {}, 0(t3)", reg)?;
            }
        }
        Ok(())
    }

    /// Generate asm code for the function
    pub fn generate(&mut self) -> Result<()> {
        // prologue
        if self.stack_size > 0 {
            writeln!(self.writer, "\taddi sp, sp, -{}", self.stack_size)?;
            if self.has_call {
                writeln!(self.writer, "\tsw ra, {}(sp)", self.stack_size - 4)?;
            }
        }

        // 保存函数参数到栈上
        for (i, &param) in self.func_data.params().iter().enumerate() {
            if i < 8 {
                if let Some(&offset) = self.stack_map.get(&param) {
                    writeln!(self.writer, "\tsw a{}, {}(sp)", i, offset)?;
                }
            }
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
                        if self.has_call {
                            writeln!(self.writer, "\tlw ra, {}(sp)", self.stack_size - 4)?;
                        }
                        if self.stack_size > 0 {
                            writeln!(self.writer, "\taddi sp, sp, {}", self.stack_size)?;
                        }
                        writeln!(self.writer, "\tret")?;
                        writeln!(self.writer, "")?;
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
                    ValueKind::Call(call) => {
                        // 传递参数
                        for (i, &arg) in call.args().iter().enumerate() {
                            if i < 8 {
                                self.load_to_reg(arg, &format!("a{}", i))?;
                            } else {
                                let offset = (i - 8) * 4;
                                self.load_to_reg(arg, "t0")?;
                                writeln!(self.writer, "\tsw t0, {}(sp)", offset)?;
                            }
                        }
                        // 写入调用指令
                        let func = self.program.func(call.callee());
                        let name = func.name().trim_start_matches('@');
                        writeln!(self.writer, "\tcall {}", name)?;
                        // 处理返回值
                        let inst_data = self.func_data.dfg().value(inst);
                        if !inst_data.ty().is_unit() {
                            self.store_from_reg(inst, "a0")?;
                        }
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
        if let Some(bb_name) = bb_data.name() {
            let func_name = self.func_data.name().trim_start_matches('@');
            format!("{}_{}", func_name, bb_name.trim_start_matches("%"))
        } else {
            "unknown".to_string()
        }
    }
}
