use const_prop::ConstantPropagation;
use dce::DeadCodeElimination;
use koopa::ir::Program;
use mem2reg::Mem2Reg;
use side_effect::analyze_side_effects;

mod const_prop;
mod dce;
mod dom;
mod mem2reg;
mod side_effect;

const MAX_OPT_PASSES: usize = 10;

pub fn optimize_program(program: &mut Program) {
    // Analyze side effects for all functions
    let side_effects = analyze_side_effects(program);

    let funcs: Vec<_> = program.funcs_mut().keys().cloned().collect();

    for func in funcs {
        let func_data = program.func_mut(func);
        if func_data.layout().entry_bb().is_none() {
            continue;
        }

        // Transform IR into SSA style
        let mut m2r = Mem2Reg::new(func_data);
        m2r.run();

        let mut changed = true;
        let mut pass_count = 0;
        while changed && pass_count < MAX_OPT_PASSES {
            changed = false;
            pass_count += 1;

            // Apply optimization passes
            // Constant propagation pass
            let mut const_prop = ConstantPropagation::new(func_data);
            changed |= const_prop.run();
            // Dead code elimination pass
            let mut dce = DeadCodeElimination::new(func_data, &side_effects);
            changed |= dce.run();
        }
    }
}
