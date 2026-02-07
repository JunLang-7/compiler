mod asm_gen;
mod interval;
mod liveness;
mod lsra;

use asm_gen::AsmGen;
use koopa::ir::{Program, Type, TypeKind, Value, ValueKind};
use std::io::{Result, Write};

/// Generate `RISC-V` assembly from `Koopa IR`
pub fn generate_asm(program: &Program, writer: &mut dyn Write) -> Result<()> {
    generate_data_section(program, writer)?;
    generate_text_section(program, writer)?;
    Ok(())
}

/// Generate `.data` section for global variables
fn generate_data_section(program: &Program, writer: &mut dyn Write) -> Result<()> {
    writeln!(writer, "\t.data")?;
    for &val in program.inst_layout() {
        let data = program.borrow_value(val);
        if let ValueKind::GlobalAlloc(alloc) = data.kind() {
            let name = data.name().as_deref().unwrap().trim_start_matches('@');
            writeln!(writer, "\t.global {}", name)?;
            writeln!(writer, "{}:", name)?;
            // 处理初值
            let init = alloc.init();
            generate_global_init(program, writer, init)?;
        }
    }
    writeln!(writer, "")?;
    Ok(())
}

/// Generate `.text` section for functions
fn generate_text_section(program: &Program, writer: &mut dyn Write) -> Result<()> {
    for &func in program.func_layout() {
        let func_data = program.func(func);
        if func_data.layout().entry_bb().is_none() {
            continue; // skip declarations
        }
        let name = func_data.name().trim_start_matches('@');
        writeln!(writer, "\t.text")?;
        writeln!(writer, "\t.globl {}", name)?;
        writeln!(writer, "{}:", name)?;

        let mut asm_gen = AsmGen::new(writer, program, func_data);
        asm_gen.generate()?;
    }
    Ok(())
}

/// Generate global variable initial value
fn generate_global_init(program: &Program, writer: &mut dyn Write, init: Value) -> Result<()> {
    let val_data = program.borrow_value(init);
    match val_data.kind() {
        ValueKind::Integer(int) => {
            writeln!(writer, "\t.word {}", int.value())?;
        }
        ValueKind::ZeroInit(_) => {
            let size = calc_type_size(val_data.ty());
            writeln!(writer, "\t.zero {}", size)?;
        }
        ValueKind::Aggregate(agg) => {
            for elem in agg.elems() {
                generate_global_init(program, writer, *elem)?;
            }
        }
        _ => panic!("Invaild global init value"),
    }
    Ok(())
}

/// Calcuate size of `Type` of RVI32
pub fn calc_type_size(ty: &Type) -> usize {
    match ty.kind() {
        TypeKind::Int32 => 4,
        TypeKind::Unit => 0,
        TypeKind::Array(base, len) => calc_type_size(base) * len,
        TypeKind::Pointer(_) => 4,
        _ => 0,
    }
}
