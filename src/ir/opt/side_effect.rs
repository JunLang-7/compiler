/// Side effect analysis for functions
use koopa::ir::{Function, Program, TypeKind, Value, ValueKind};
use std::collections::HashMap;

/// Analyze which functions have side effects
pub fn analyze_side_effects(program: &Program) -> HashMap<Function, bool> {
    let mut side_effects = HashMap::new();
    let mut changed = true;

    // Initialize: all functions without body are assumed to have side effects (runtime library)
    for (&func, func_data) in program.funcs() {
        let has_body = func_data.layout().entry_bb().is_some();
        if !has_body {
            // Runtime library functions are treated as having side effects
            side_effects.insert(func, true);
        } else {
            side_effects.insert(func, false);
        }
    }

    // Iterate until convergence
    while changed {
        changed = false;

        for (&func, func_data) in program.funcs() {
            if side_effects[&func] {
                continue; // Already marked as having side effects
            }
            if func_data.layout().entry_bb().is_none() {
                continue;
            }

            // Check if function has side effects
            let mut has_effect = false;

            for (_, node) in func_data.layout().bbs() {
                for &inst in node.insts().keys() {
                    let val_data = func_data.dfg().value(inst);
                    match val_data.kind() {
                        // Store to global or parameter pointer
                        ValueKind::Store(store) => {
                            let dest = store.dest();
                            if is_global_or_param(func_data, dest) {
                                has_effect = true;
                                break;
                            }
                        }
                        // Call to function with side effects
                        ValueKind::Call(call) => {
                            let callee = call.callee();
                            if side_effects.get(&callee).copied().unwrap_or(true) {
                                has_effect = true;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                if has_effect {
                    break;
                }
            }

            if has_effect && !side_effects[&func] {
                side_effects.insert(func, true);
                changed = true;
            }
        }
    }

    side_effects
}

/// Check if a value is a global allocation or parameter pointer
pub fn is_global_or_param(func_data: &koopa::ir::FunctionData, val: Value) -> bool {
    if !func_data.dfg().values().contains_key(&val) {
        // Check if it's a function parameter or global value
        // Global values and parameters are not in function's dfg
        return true; // Conservative: treat as global/param
    }

    match func_data.dfg().value(val).kind() {
        ValueKind::GlobalAlloc(_) => true,
        ValueKind::FuncArgRef(_) => true,
        ValueKind::BlockArgRef(_) => true,
        ValueKind::GetPtr(gp) => is_global_or_param(func_data, gp.src()),
        ValueKind::GetElemPtr(gep) => is_global_or_param(func_data, gep.src()),
        ValueKind::Load(load) => {
            // Check if loading from a pointer-typed alloc
            let src = load.src();
            if let Some(ValueKind::Alloc(_)) = func_data.dfg().values().get(&src).map(|v| v.kind())
            {
                matches!(func_data.dfg().value(src).ty().kind(), 
                    TypeKind::Pointer(inner) if matches!(inner.kind(), TypeKind::Pointer(_)))
            } else {
                is_global_or_param(func_data, src)
            }
        }
        _ => false,
    }
}
