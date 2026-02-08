mod builder;
mod opt;
mod reg_alloc;
mod riscv;

use builder::RiscvFuncBuilder;
use koopa::ir::{Program, Type, TypeKind, Value, ValueKind};
use opt::peephole_optimize;
use riscv::{RiscvFunc, RiscvProg};
use std::io::{Result, Write};

/// Write RISC-V assembly code to `writer` from `Koopa IR` program
pub fn generate_asm(program: &Program, writer: &mut dyn Write) -> Result<()> {
    let mut prog = build_riscv_prog(program)?;
    // Apply peephole optimization
    peephole_optimize(&mut prog);

    write!(writer, "{}", prog)?;
    Ok(())
}

/// Build `RISC-V` program from `Koopa IR`
fn build_riscv_prog(program: &Program) -> Result<RiscvProg> {
    let mut prog = RiscvProg::new();
    prog.data_sec = build_data_section(program);
    prog.text_sec = build_text_section(program)?;
    Ok(prog)
}

/// Build `.data` section for global variables
fn build_data_section(program: &Program) -> Vec<String> {
    let mut lines = Vec::new();
    for &val in program.inst_layout() {
        let data = program.borrow_value(val);
        if let ValueKind::GlobalAlloc(alloc) = data.kind() {
            let name = data.name().as_deref().unwrap().trim_start_matches('@');
            lines.push(format!("\t.global {}", name));
            lines.push(format!("{}:", name));
            // 处理初值
            let init = alloc.init();
            build_global_init(program, init, &mut lines);
        }
    }
    lines
}

/// Build `.text` section for functions
fn build_text_section(program: &Program) -> Result<Vec<RiscvFunc>> {
    let mut funcs = Vec::new();
    for &func in program.func_layout() {
        let func_data = program.func(func);
        if func_data.layout().entry_bb().is_none() {
            continue; // skip declarations
        }
        let asm_gen = RiscvFuncBuilder::new(program, func_data);
        funcs.push(asm_gen.generate()?);
    }
    Ok(funcs)
}

/// Build global variable initial value
fn build_global_init(program: &Program, init: Value, out: &mut Vec<String>) {
    let val_data = program.borrow_value(init);
    match val_data.kind() {
        ValueKind::Integer(int) => {
            out.push(format!("\t.word {}", int.value()));
        }
        ValueKind::ZeroInit(_) => {
            let size = calc_type_size(val_data.ty());
            out.push(format!("\t.zero {}", size));
        }
        ValueKind::Aggregate(agg) => {
            for elem in agg.elems() {
                build_global_init(program, *elem, out);
            }
        }
        _ => panic!("Invaild global init value"),
    }
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
