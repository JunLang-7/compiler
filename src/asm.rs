use core::panic;
use koopa::ir::{BinaryOp, FunctionData, Program, Value, ValueKind};
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
        writeln!(writer, "\t.globl {}", name)?;
        writeln!(writer, "{}:", name)?;

        let mut value_map: HashMap<Value, String> = HashMap::new();
        let mut reg_idx = 0;

        for (&_bb, node) in self.layout().bbs() {
            // 遍历指令列表
            for &inst in node.insts().keys() {
                generate_inst(self, inst, writer, &mut value_map, &mut reg_idx)?;
            }
        }
        Ok(())
    }
}

/// generate asm based on each instruction
fn generate_inst(
    func_data: &FunctionData,
    inst: Value,
    writer: &mut dyn Write,
    value_map: &mut HashMap<Value, String>,
    reg_idx: &mut usize,
) -> Result<()> {
    let value_data = func_data.dfg().value(inst);
    match value_data.kind() {
        ValueKind::Integer(_) => {}
        ValueKind::Return(ret) => {
            if let Some(ret_val) = ret.value() {
                load_to_reg(func_data, ret_val, "a0", writer, value_map)?;
            }
            writeln!(writer, "\tret")?;
        }
        ValueKind::Binary(bin) => {
            // 获取操作数的寄存器
            let rs = get_operand_reg(func_data, bin.lhs(), writer, value_map, reg_idx)?;
            let rd = get_operand_reg(func_data, bin.rhs(), writer, value_map, reg_idx)?;
            let rt = if rs != "x0" {
                rs.clone()
            } else if rd != "x0" {
                rd.clone()
            } else {
                let reg = get_reg(reg_idx);
                *reg_idx += 1;
                reg
            };

            match bin.op() {
                BinaryOp::Sub => {
                    writeln!(writer, "\tsub {}, {}, {}", rt, rs, rd)?;
                }
                BinaryOp::Eq => {
                    writeln!(writer, "\txor {}, {}, {}", rt, rs, rd)?;
                    writeln!(writer, "\tseqz {}, {}", rt, rt)?;
                }
                _ => unreachable!(),
            }
            value_map.insert(inst, rt);
        }
        _ => unreachable!(),
    }
    Ok(())
}

/// Load a value into a register
fn load_to_reg(
    func_data: &FunctionData,
    val: Value,
    dst_reg: &str,
    writer: &mut dyn Write,
    value_map: &HashMap<Value, String>,
) -> Result<()> {
    let val_data = func_data.dfg().value(val);
    match val_data.kind() {
        ValueKind::Integer(int) => {
            writeln!(writer, "\tli {}, {}", dst_reg, int.value())?;
        }
        _ => {
            if let Some(src_reg) = value_map.get(&val) {
                if src_reg != dst_reg {
                    writeln!(writer, "\tmv {}, {}", dst_reg, src_reg)?;
                }
            } else {
                panic!("Register not found!");
            }
        }
    }
    Ok(())
}

/// Get the register for an operand value
fn get_operand_reg(
    func_data: &FunctionData,
    val: Value,
    writer: &mut dyn Write,
    value_map: &HashMap<Value, String>,
    reg_idx: &mut usize,
) -> Result<String> {
    // Check if the value is already mapped to a register
    if let Some(reg) = value_map.get(&val) {
        return Ok(reg.clone());
    }

    let val_data = func_data.dfg().value(val);
    // If the value is an integer zero, return x0 directly
    if let ValueKind::Integer(int) = val_data.kind() {
        if int.value() == 0 {
            return Ok("x0".to_string());
        }
    }
    // Otherwise, allocate a new register and load the value into it
    let reg = get_reg(reg_idx);
    *reg_idx += 1;
    load_to_reg(func_data, val, &reg, writer, value_map)?;
    Ok(reg)
}

/// Get a register name based on the index
fn get_reg(reg_idx: &mut usize) -> String {
    if *reg_idx < 7 {
        let reg = format!("t{}", *reg_idx);
        reg
    } else if *reg_idx < 15 {
        let reg = format!("a{}", *reg_idx - 7);
        reg
    } else {
        panic!("Register overflow!");
    }
}
