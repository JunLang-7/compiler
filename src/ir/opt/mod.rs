use dce::DeadCodeElimination;
use koopa::ir::Program;
use side_effect::analyze_side_effects;

mod dce;
mod side_effect;

pub fn optimize_program(program: &mut Program) {
    // Analyze side effects for all functions
    let side_effects = analyze_side_effects(program);

    let funcs: Vec<_> = program.funcs_mut().keys().cloned().collect();

    for func in funcs {
        let func_data = program.func_mut(func);
        if func_data.layout().entry_bb().is_none() {
            continue;
        }

        let mut dce = DeadCodeElimination::new(func_data, &side_effects);
        dce.run();
    }
}
