use super::{Eval, GenContext, Symbol, generate_exp, get_array_type};
use crate::ast::{ConstInitVal, Decl, InitVal};
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, Type, TypeKind, Value};

/// Generate Koopa IR for a declaration and return the latest insertion block
pub fn generate_decl(ctx: &mut GenContext, bb: BasicBlock, decl: &Decl) -> BasicBlock {
    let mut current_bb = bb;
    match decl {
        Decl::ConstDecl(const_decl) => {
            for def in &const_decl.const_defs {
                let ty = get_array_type(ctx, Type::get_i32(), &def.dims);
                if def.dims.is_empty() {
                    // Scalar Constant
                    match &def.const_init_val {
                        ConstInitVal::Exp(const_exp) => {
                            let val = const_exp.exp.evaluate(ctx.symbol_table());
                            ctx.insert_symbol(def.ident.clone(), Symbol::Const(val));
                        }
                        ConstInitVal::List(_) => {
                            panic!("Scalar constant cannot be initialized with a list")
                        }
                    }
                } else {
                    // Array Constant -> Treated as Variable in IR
                    let alloc = ctx.func_mut().dfg_mut().new_value().alloc(ty);
                    ctx.func_mut()
                        .layout_mut()
                        .bb_mut(current_bb)
                        .insts_mut()
                        .push_key_back(alloc)
                        .expect("failed to add alloc instruction");
                    ctx.insert_symbol(def.ident.clone(), Symbol::Array(alloc));
                    init_const_array(ctx, &mut current_bb, alloc, &def.const_init_val);
                }
            }
        }
        Decl::VarDecl(var_decl) => {
            for def in &var_decl.var_defs {
                let ty = get_array_type(ctx, Type::get_i32(), &def.dims);
                let alloc = ctx.func_mut().dfg_mut().new_value().alloc(ty);
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
                    init_array(ctx, &mut current_bb, alloc, init_val);
                }
            }
        }
    }
    current_bb
}

fn init_array(ctx: &mut GenContext, bb: &mut BasicBlock, ptr: Value, init: &InitVal) {
    match init {
        InitVal::Exp(exp) => {
            let val = generate_exp(ctx, bb, exp);
            let store = ctx.func_mut().dfg_mut().new_value().store(val, ptr);
            ctx.func_mut()
                .layout_mut()
                .bb_mut(*bb)
                .insts_mut()
                .push_key_back(store)
                .expect("failed to add store instruction");
        }
        InitVal::List(list) => {
            let ptr_ty = ctx.func_mut().dfg().value(ptr).ty().clone();
            if let TypeKind::Pointer(inner) = ptr_ty.kind() {
                if let TypeKind::Array(_, len) = inner.kind() {
                    for i in 0..*len {
                        let idx = ctx.func_mut().dfg_mut().new_value().integer(i as i32);
                        let elem_ptr = ctx.func_mut().dfg_mut().new_value().get_elem_ptr(ptr, idx);
                        ctx.func_mut()
                            .layout_mut()
                            .bb_mut(*bb)
                            .insts_mut()
                            .push_key_back(elem_ptr)
                            .expect("failed to add getelemptr");

                        if i < list.len() {
                            init_array(ctx, bb, elem_ptr, &list[i]);
                        } else {
                            zero_init_array(ctx, bb, elem_ptr);
                        }
                    }
                }
            }
        }
    }
}

fn init_const_array(ctx: &mut GenContext, bb: &mut BasicBlock, ptr: Value, init: &ConstInitVal) {
    match init {
        ConstInitVal::Exp(const_exp) => {
            // Evaluate constant expression
            let val = const_exp.exp.evaluate(ctx.symbol_table());
            let val_inst = ctx.func_mut().dfg_mut().new_value().integer(val);
            let store = ctx.func_mut().dfg_mut().new_value().store(val_inst, ptr);
            ctx.func_mut()
                .layout_mut()
                .bb_mut(*bb)
                .insts_mut()
                .push_key_back(store)
                .expect("failed to add store instruction");
        }
        ConstInitVal::List(list) => {
            let ptr_ty = ctx.func_mut().dfg().value(ptr).ty().clone();
            if let TypeKind::Pointer(inner) = ptr_ty.kind() {
                if let TypeKind::Array(_, len) = inner.kind() {
                    for i in 0..*len {
                        let idx = ctx.func_mut().dfg_mut().new_value().integer(i as i32);
                        let elem_ptr = ctx.func_mut().dfg_mut().new_value().get_elem_ptr(ptr, idx);
                        ctx.func_mut()
                            .layout_mut()
                            .bb_mut(*bb)
                            .insts_mut()
                            .push_key_back(elem_ptr)
                            .expect("failed to add getelemptr");

                        if i < list.len() {
                            init_const_array(ctx, bb, elem_ptr, &list[i]);
                        } else {
                            zero_init_array(ctx, bb, elem_ptr);
                        }
                    }
                }
            }
        }
    }
}

fn zero_init_array(ctx: &mut GenContext, bb: &mut BasicBlock, ptr: Value) {
    let ptr_ty = ctx.func_mut().dfg().value(ptr).ty().clone();
    if let TypeKind::Pointer(inner) = ptr_ty.kind() {
        match inner.kind() {
            TypeKind::Int32 => {
                let zero = ctx.func_mut().dfg_mut().new_value().integer(0);
                let store = ctx.func_mut().dfg_mut().new_value().store(zero, ptr);
                ctx.func_mut()
                    .layout_mut()
                    .bb_mut(*bb)
                    .insts_mut()
                    .push_key_back(store)
                    .expect("failed to add zero store");
            }
            TypeKind::Array(_, len) => {
                for i in 0..*len {
                    let idx = ctx.func_mut().dfg_mut().new_value().integer(i as i32);
                    let elem_ptr = ctx.func_mut().dfg_mut().new_value().get_elem_ptr(ptr, idx);
                    ctx.func_mut()
                        .layout_mut()
                        .bb_mut(*bb)
                        .insts_mut()
                        .push_key_back(elem_ptr)
                        .expect("failed to add getelemptr for zero init");
                    zero_init_array(ctx, bb, elem_ptr);
                }
            }
            _ => panic!("Unexpected type for zero init"),
        }
    }
}
