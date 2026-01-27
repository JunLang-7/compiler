mod context;
mod decl;
mod eval;
mod exp;
mod stmt;

use crate::ast::{self, Block, BlockItem};
use decl::generate_decl;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, FunctionData, Program, Type};
use std::collections::HashMap;
use stmt::generate_stmt;

pub use context::{GenContext, Symbol};
pub use eval::Eval;
pub use exp::generate_exp;

/// Generate Koopa IR from the AST
pub fn generate_koopa(ast: &ast::CompUnit) -> Program {
    // create the program and function
    let mut program = Program::new();
    let func_data = FunctionData::new(format!("@{}", ast.func_def.ident), vec![], Type::get_i32());
    let func = program.new_func(func_data);
    let mut ctx = GenContext {
        func_data: program.func_mut(func),
        scopes: vec![HashMap::new()],
        loop_stack: vec![],
        if_counter: 0,
        while_counter: 0,
        land_counter: 0,
        lor_counter: 0,
    };

    // 在函数中创建一个基本块(entry basic block)
    let entry = ctx
        .func_data
        .dfg_mut()
        .new_bb()
        .basic_block(Some("%entry".into()));
    // 将基本块添加到函数布局中
    ctx.func_data
        .layout_mut()
        .bbs_mut()
        .push_key_back(entry)
        .expect("fail to push entry bb");
    // 生成函数体的中间表示
    generate_block(&mut ctx, entry, &ast.func_def.block);

    program
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
