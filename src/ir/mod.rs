mod context;
mod decl;
mod eval;
mod exp;
mod stmt;

use crate::ast::{self, Block, BlockItem, FuncDef, GlobalItem};
use decl::generate_decl;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, FunctionData, Program};
use stmt::generate_stmt;

pub use context::{GenContext, Symbol};
pub use eval::Eval;
pub use exp::generate_exp;

/// Generate Koopa IR from the AST
pub fn generate_koopa(ast: &ast::CompUnit) -> Program {
    // create the program and function
    let mut program = Program::new();
    let mut ctx = GenContext::new(&mut program);

    for item in &ast.items {
        match item {
            GlobalItem::FuncDef(func_def) => {
                generate_func_def(&mut ctx, func_def);
            }
        }
    }
    program
}

/// Generate Koopa IR for a function definition
pub fn generate_func_def(ctx: &mut GenContext, func_def: &FuncDef) {
    let parm_ty = if let Some(func_f_parms) = &func_def.func_f_parms {
        func_f_parms
            .func_f_parms
            .iter()
            .map(|parm| {
                (
                    Some(format!("@{}", parm.ident.clone())),
                    parm.b_type.into_type(),
                )
            })
            .collect()
    } else {
        vec![]
    };
    let ret_ty = func_def.func_type.return_type();
    let func_data =
        FunctionData::with_param_names(format!("@{}", func_def.ident), parm_ty, ret_ty.clone());
    let parm_val = func_data.params().to_owned();
    let func = ctx.program.new_func(func_data);
    ctx.insert_symbol(func_def.ident.clone(), Symbol::Func(func));
    // 设置当前函数与作用域
    ctx.current_func = Some(func);
    ctx.enter_scope();
    // 在函数中创建一个基本块(entry basic block)
    let entry = ctx
        .func_mut()
        .dfg_mut()
        .new_bb()
        .basic_block(Some("%entry".into()));
    // 将基本块添加到函数布局中
    ctx.func_mut()
        .layout_mut()
        .bbs_mut()
        .push_key_back(entry)
        .expect("fail to push entry bb");
    // 处理函数参数
    if let Some(func_f_parms) = &func_def.func_f_parms {
        for (parm, value) in func_f_parms.func_f_parms.iter().zip(parm_val) {
            let alloc = ctx
                .func_mut()
                .dfg_mut()
                .new_value()
                .alloc(parm.b_type.into_type());
            let store = ctx.func_mut().dfg_mut().new_value().store(value, alloc);
            ctx.func_mut()
                .layout_mut()
                .bb_mut(entry)
                .insts_mut()
                .extend([alloc, store]);
            ctx.insert_symbol(parm.ident.clone(), Symbol::Var(alloc));
        }
    }
    // 生成函数体的中间表示
    let last_bb = generate_block(ctx, entry, &func_def.block);

    // 如果最后一个基本块没有终止指令，添加一个 ret
    if !ctx.is_bb_terminated(last_bb) {
        let ret_inst = if ret_ty.is_unit() {
            ctx.func_mut().dfg_mut().new_value().ret(None)
        } else {
            let zero = ctx.func_mut().dfg_mut().new_value().integer(0);
            ctx.func_mut().dfg_mut().new_value().ret(Some(zero))
        };
        ctx.func_mut()
            .layout_mut()
            .bb_mut(last_bb)
            .insts_mut()
            .push_key_back(ret_inst)
            .expect("failed to add return instruction");
    }

    // 退出作用域并重置当前函数
    ctx.exit_scope();
    ctx.current_func = None;
}

/// Generate Koopa IR for a block and return the latest insertion block
pub fn generate_block(ctx: &mut GenContext, bb: BasicBlock, block: &Block) -> BasicBlock {
    let mut current_bb = bb;
    for block_item in &block.block_items {
        // 如果当前基本块已经终止，停止生成
        if ctx.is_bb_terminated(current_bb) {
            break;
        }
        match block_item {
            BlockItem::Stmt(stmt) => {
                current_bb = generate_stmt(ctx, current_bb, &stmt);
            }
            BlockItem::Decl(decl) => {
                current_bb = generate_decl(ctx, current_bb, &decl);
            }
        }
    }
    current_bb
}
