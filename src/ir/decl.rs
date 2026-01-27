use super::{Eval, GenContext, Symbol, generate_exp};
use crate::ast::Decl;
use koopa::ir::BasicBlock;
use koopa::ir::Type;
use koopa::ir::builder_traits::*;

/// Generate Koopa IR for a declaration and return the latest insertion block
pub fn generate_decl(ctx: &mut GenContext, bb: BasicBlock, decl: &Decl) -> BasicBlock {
    let mut current_bb = bb;
    match decl {
        Decl::ConstDecl(const_decl) => {
            for def in &const_decl.const_defs {
                let exp = &def.const_init_val.const_exp.exp;
                let value = exp.evaluate(&mut ctx.scopes.last_mut().unwrap());
                ctx.insert_symbol(def.ident.clone(), Symbol::Const(value));
            }
        }
        Decl::VarDecl(var_decl) => {
            for def in &var_decl.var_defs {
                // 在栈上分配内存
                let alloc = ctx.func_data.dfg_mut().new_value().alloc(Type::get_i32());
                ctx.func_data
                    .layout_mut()
                    .bb_mut(current_bb)
                    .insts_mut()
                    .push_key_back(alloc)
                    .expect("failed to add alloc instruction");
                ctx.insert_symbol(def.ident.clone(), Symbol::Var(alloc));
                // 处理初始化值
                if let Some(init_val) = &def.init_val {
                    let res = generate_exp(ctx, &mut current_bb, &init_val.exp);
                    let store = ctx.func_data.dfg_mut().new_value().store(res, alloc);
                    ctx.func_data
                        .layout_mut()
                        .bb_mut(current_bb)
                        .insts_mut()
                        .push_key_back(store)
                        .expect("failed to add store instruction");
                }
            }
        }
    }
    current_bb
}
