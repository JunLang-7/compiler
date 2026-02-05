use super::{Eval, GenContext, Symbol, generate_exp, get_array_type};
use crate::ast::{ConstInitVal, Decl, Exp, InitVal};
use crate::ir::util::{flatten_const, flatten_var};
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, Type, Value};

/// Generate Koopa IR for a declaration and return the latest insertion block
pub fn generate_decl(ctx: &mut GenContext, bb: BasicBlock, decl: &Decl) -> BasicBlock {
    let mut current_bb = bb;
    match decl {
        Decl::ConstDecl(const_decl) => {
            for def in &const_decl.const_defs {
                let ty = get_array_type(ctx, Type::get_i32(), &def.dims);
                let dims_i32: Vec<i32> = def.dims.iter().map(|dim| dim.exp.evaluate(ctx)).collect();
                if def.dims.is_empty() {
                    // Scalar Constant
                    match &def.const_init_val {
                        ConstInitVal::Exp(const_exp) => {
                            let val = const_exp.exp.evaluate(ctx);
                            ctx.insert_symbol(def.ident.clone(), Symbol::Const(val));
                        }
                        ConstInitVal::List(_) => {
                            panic!("Scalar constant cannot be initialized with a list")
                        }
                    }
                } else {
                    // Array Constant -> Treated as Variable in IR
                    let alloc = ctx.func_mut().dfg_mut().new_value().alloc(ty);
                    ctx.func_mut().dfg_mut().set_value_name(alloc, Some(format!("@{}", def.ident.clone())));
                    ctx.func_mut()
                        .layout_mut()
                        .bb_mut(current_bb)
                        .insts_mut()
                        .push_key_back(alloc)
                        .expect("failed to add alloc instruction");
                    ctx.insert_symbol(def.ident.clone(), Symbol::Array(alloc));
                    let flat_vals = flatten_const(ctx, &def.const_init_val, &dims_i32);
                    init_local_array_flat(ctx, &mut current_bb, alloc, &flat_vals, &dims_i32);
                }
            }
        }
        Decl::VarDecl(var_decl) => {
            for def in &var_decl.var_defs {
                let ty = get_array_type(ctx, Type::get_i32(), &def.dims);
                let dims_i32: Vec<i32> = def.dims.iter().map(|dim| dim.exp.evaluate(ctx)).collect();
                let alloc = ctx.func_mut().dfg_mut().new_value().alloc(ty);
                ctx.func_mut().dfg_mut().set_value_name(alloc, Some(format!("@{}", def.ident.clone())));
                ctx.func_mut()
                    .layout_mut()
                    .bb_mut(current_bb)
                    .insts_mut()
                    .push_key_back(alloc)
                    .expect("failed to add alloc instruction");
                if def.dims.is_empty() {
                    ctx.insert_symbol(def.ident.clone(), Symbol::Var(alloc));
                } else {
                    ctx.insert_symbol(def.ident.clone(), Symbol::Array(alloc));
                }

                if let Some(init_val) = &def.init_val {
                    if def.dims.is_empty() {
                        // Scalar Variable
                        if let InitVal::Exp(exp) = init_val {
                            let val_inst = generate_exp(ctx, &mut current_bb, exp);
                            let store = ctx.func_mut().dfg_mut().new_value().store(val_inst, alloc);
                            ctx.func_mut()
                                .layout_mut()
                                .bb_mut(current_bb)
                                .insts_mut()
                                .push_key_back(store)
                                .expect("failed to add store instruction");
                        }
                    } else {
                        // Array Variable, flatten and initialize
                        let flat_exps = flatten_var(ctx, init_val, &dims_i32);
                        init_local_array_exps(ctx, &mut current_bb, alloc, &flat_exps, &dims_i32);
                    }
                }
            }
        }
    }
    current_bb
}

/// Initialize a local array with flattened `i32` values
fn init_local_array_flat(
    ctx: &mut GenContext,
    bb: &mut BasicBlock,
    base_ptr: Value,
    flat_vals: &[i32],
    dims: &[i32],
) {
    for (i, &val) in flat_vals.iter().enumerate() {
        let ptr = get_elem_ptr_from_flat_idx(ctx, bb, base_ptr, i, dims);
        let val_inst = ctx.func_mut().dfg_mut().new_value().integer(val);
        let store = ctx.func_mut().dfg_mut().new_value().store(val_inst, ptr);
        ctx.func_mut()
            .layout_mut()
            .bb_mut(*bb)
            .insts_mut()
            .push_key_back(store)
            .expect("failed to add store instruction");
    }
}

/// Initialize a local array with flattened `Exp` values
fn init_local_array_exps(
    ctx: &mut GenContext,
    bb: &mut BasicBlock,
    base_ptr: Value,
    exps: &[Exp],
    dims: &[i32],
) {
    for (i, exp) in exps.iter().enumerate() {
        let ptr = get_elem_ptr_from_flat_idx(ctx, bb, base_ptr, i, dims);
        let val_inst = generate_exp(ctx, bb, exp);
        let store = ctx.func_mut().dfg_mut().new_value().store(val_inst, ptr);
        ctx.func_mut()
            .layout_mut()
            .bb_mut(*bb)
            .insts_mut()
            .push_key_back(store)
            .expect("failed to add store instruction");
    }
}

/// Get the pointer to an element in a multi-dimensional array given its flat index
fn get_elem_ptr_from_flat_idx(
    ctx: &mut GenContext,
    bb: &mut BasicBlock,
    base_ptr: Value,
    flat_idx: usize,
    dims: &[i32],
) -> Value {
    let mut ptr = base_ptr;
    let mut curr_idx = flat_idx;

    // 计算 strides: [d1*d2*..dn, ..., dn, 1]
    // 实际上我们可以边算边生成
    // 例如 dims=[2, 3, 4], flat_idx=5 (0, 1, 1)
    // stride[0] = 3*4=12. idx[0] = 5/12 = 0. rem = 5.
    // stride[1] = 4.      idx[1] = 5/4 = 1.  rem = 1.
    // stride[2] = 1.      idx[2] = 1/1 = 1.  rem = 0.
    let mut strides = vec![1; dims.len()];
    for i in (0..dims.len() - 1).rev() {
        strides[i] = strides[i + 1] * dims[i + 1] as usize;
    }

    for i in 0..dims.len() {
        let idx_val = curr_idx / strides[i];
        curr_idx %= strides[i];

        let idx_inst = ctx.func_mut().dfg_mut().new_value().integer(idx_val as i32);
        let next_ptr = ctx
            .func_mut()
            .dfg_mut()
            .new_value()
            .get_elem_ptr(ptr, idx_inst);
        ctx.func_mut()
            .layout_mut()
            .bb_mut(*bb)
            .insts_mut()
            .push_key_back(next_ptr)
            .expect("failed to add getelemptr instruction");
        ptr = next_ptr;
    }
    ptr
}
