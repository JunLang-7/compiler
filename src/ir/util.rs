use super::{Eval, GenContext};
use crate::ast::{ConstExp, ConstInitVal, Exp, InitVal};
use crate::ir::generate_exp;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, Type, TypeKind, Value};

/// Calculate array type from base type and dimensions
pub fn get_array_type(ctx: &mut GenContext, base_ty: Type, dims: &[ConstExp]) -> Type {
    let mut ty = base_ty;
    for dim in dims.iter().rev() {
        let len = dim.exp.evaluate(ctx.symbol_table());
        ty = Type::get_array(ty, len as usize);
    }
    ty
}

/// Generate pointer calculation for array access
pub fn generate_ptr(
    ctx: &mut GenContext,
    bb: &mut BasicBlock,
    base: Value,
    indices: &[Exp],
    is_ptr: bool, // true if base is a pointer (function param), false if array (alloc)
) -> Value {
    let mut ptr = base;
    for (i, index) in indices.iter().enumerate() {
        let idx_val = generate_exp(ctx, bb, index);

        if i == 0 && is_ptr {
            // First dimension of a pointer parameter: use getptr
            let getptr = ctx.func_mut().dfg_mut().new_value().get_ptr(ptr, idx_val);
            ctx.func_mut()
                .layout_mut()
                .bb_mut(*bb)
                .insts_mut()
                .push_key_back(getptr)
                .expect("failed to add getptr instruction");
            ptr = getptr;
        } else {
            // Array access: use getelemptr
            let getelemptr = ctx
                .func_mut()
                .dfg_mut()
                .new_value()
                .get_elem_ptr(ptr, idx_val);
            ctx.func_mut()
                .layout_mut()
                .bb_mut(*bb)
                .insts_mut()
                .push_key_back(getelemptr)
                .expect("failed to add getelemptr instruction");
            ptr = getelemptr;
        }
    }
    ptr
}

/// Generate initialization value for global variable
pub fn generate_global_init_val(ctx: &mut GenContext, init: &InitVal, ty: &Type) -> Value {
    match init {
        InitVal::Exp(exp) => {
            let val = exp.evaluate(ctx.symbol_table());
            ctx.program.new_value().integer(val)
        }
        InitVal::List(list) => {
            let mut elems = Vec::new();
            if let TypeKind::Array(elem_ty, len) = ty.kind() {
                for i in 0..*len {
                    if i < list.len() {
                        elems.push(generate_global_init_val(ctx, &list[i], elem_ty));
                    } else {
                        elems.push(generate_global_zero_init(ctx, elem_ty));
                    }
                }
                ctx.program.new_value().aggregate(elems)
            } else {
                panic!("Type mismatch for list initializer");
            }
        }
    }
}

/// Generate initialization value for global constant
pub fn generate_global_const_init_val(
    ctx: &mut GenContext,
    init: &ConstInitVal,
    ty: &Type,
) -> Value {
    match init {
        ConstInitVal::Exp(exp) => {
            let val = exp.exp.evaluate(ctx.symbol_table());
            ctx.program.new_value().integer(val)
        }
        ConstInitVal::List(list) => {
            let mut elems = Vec::new();
            if let TypeKind::Array(elem_ty, len) = ty.kind() {
                for i in 0..*len {
                    if i < list.len() {
                        elems.push(generate_global_const_init_val(ctx, &list[i], elem_ty));
                    } else {
                        elems.push(generate_global_zero_init(ctx, elem_ty));
                    }
                }
                ctx.program.new_value().aggregate(elems)
            } else {
                panic!("Type mismatch for list initializer");
            }
        }
    }
}

/// Generate zero initialization for global variable
pub fn generate_global_zero_init(ctx: &mut GenContext, ty: &Type) -> Value {
    ctx.program.new_value().zero_init(ty.clone())
}
