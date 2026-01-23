use std::io::{Result, Write};

use crate::ast;
use koopa::ir::{FunctionData, Program, Type};
use koopa::ir::{Value, ValueKind, builder_traits::*};

/// Generate Koopa IR from the AST
pub fn generate_koopa(ast: &ast::CompUnit) -> Program {
    // create the program and function
    let mut program = Program::new();
    let func_data = FunctionData::new(format!("@{}", ast.func_def.ident), vec![], Type::get_i32());
    let func = program.new_func(func_data);
    let func_data = program.func_mut(func);

    // 在函数中创建一个基本块(entry basic block)
    let entry = func_data
        .dfg_mut()
        .new_bb()
        .basic_block(Some("%entry".into()));
    // 将基本块添加到函数布局中
    func_data
        .layout_mut()
        .bbs_mut()
        .push_key_back(entry)
        .expect("fail to push entry bb");

    // 在基本块中添加返回语句
    let ret_val = ast.func_def.block.stmt.num;
    let val = func_data.dfg_mut().new_value().integer(ret_val);
    let ret = func_data.dfg_mut().new_value().ret(Some(val));
    // 将返回语句添加到基本块中
    func_data
        .layout_mut()
        .bb_mut(entry)
        .insts_mut()
        .push_key_back(ret)
        .expect("fail to push return value");
    program
}

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
