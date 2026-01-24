use std::io::{Result, Write};
use koopa::ir::{FunctionData, Program, Value, ValueKind};

/// A trait to generate asm file base on Koopa IR
pub trait GenerateAsm {
    fn generate(&self, writer: &mut dyn Write) -> Result<()>;
}

impl GenerateAsm for Program {
    fn generate(&self, writer: &mut dyn Write) -> Result<()> {
        writeln!(writer, "  .text")?;
        for &func in self.func_layout() {
            self.func(func).generate(writer)?;
        }
        Ok(())
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self, writer: &mut dyn Write) -> Result<()> {
        let name = self.name().trim_start_matches('@');
        writeln!(writer, "\t.global {}", name)?;
        writeln!(writer, "{}:", name)?;

        for (&_bb, node) in self.layout().bbs() {
            // 遍历指令列表
            for &inst in node.insts().keys() {
                generate_inst(self, inst, writer)?;
            }
        }
        writeln!(writer, "\tret")?;
        Ok(())
    }
}

/// generate asm based on each instruction
fn generate_inst(func_data: &FunctionData, inst: Value, writer: &mut dyn Write) -> Result<()> {
    let value_data = func_data.dfg().value(inst);
    match value_data.kind() {
        ValueKind::Integer(_int) => {}
        ValueKind::Return(ret) => {
            if let Some(ret_val) = ret.value() {
                let ret_val_data = func_data.dfg().value(ret_val);
                match ret_val_data.kind() {
                    ValueKind::Integer(int) => {
                        // generate li a0, int
                        writeln!(writer, " \tli a0, {}", int.value())?;
                    }
                    _ => unreachable!(),
                }
            }
        }
        _ => unreachable!(),
    }
    Ok(())
}