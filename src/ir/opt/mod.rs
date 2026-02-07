use koopa::ir::Program;

use crate::ir::opt::dce::DeadCodeElimination;

mod dce;

pub fn optimize_program(program: &mut Program) {
    let funcs: Vec<_> = program.funcs_mut().keys().cloned().collect();

    for func in funcs {
        let func_data = program.func_mut(func);
        if func_data.layout().entry_bb().is_none() {
            continue;
        }

        let mut dce = DeadCodeElimination::new(func_data);
        dce.run();
    }
}