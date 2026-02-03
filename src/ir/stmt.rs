use super::{GenContext, Symbol, generate_block, generate_exp, generate_ptr};
use crate::ast::{Block, Exp, If, LValAssign, Stmt, While};
use koopa::ir::BasicBlock;
use koopa::ir::builder_traits::*;

/// Generate Koopa IR for a statement and return the latest insertion block
pub fn generate_stmt(ctx: &mut GenContext, bb: BasicBlock, stmt: &Stmt) -> BasicBlock {
    let mut current_bb = bb;
    match stmt {
        Stmt::Return(exp) => generate_return_stmt(ctx, current_bb, exp),
        Stmt::LValAssign(lval_assign) => {
            generate_lval_assign_stmt(ctx, &mut current_bb, lval_assign)
        }
        Stmt::Block(blk) => current_bb = generate_block_stmt(ctx, current_bb, blk),
        Stmt::Exp(opt_exp) => generate_exp_stmt(ctx, &mut current_bb, opt_exp),
        Stmt::If(if_stmt) => current_bb = generate_if_stmt(ctx, current_bb, if_stmt),
        Stmt::While(wh) => current_bb = generate_while_stmt(ctx, current_bb, wh),
        Stmt::Break(_) => generate_break_stmt(ctx, current_bb),
        Stmt::Continue(_) => generate_continue_stmt(ctx, current_bb),
    }
    current_bb
}

/// Generate return statement
fn generate_return_stmt(ctx: &mut GenContext, bb: BasicBlock, exp: &Exp) {
    let mut current_bb = bb;
    let ret_val = generate_exp(ctx, &mut current_bb, exp);
    let ret = ctx.func_mut().dfg_mut().new_value().ret(Some(ret_val));
    ctx.func_mut()
        .layout_mut()
        .bb_mut(current_bb)
        .insts_mut()
        .push_key_back(ret)
        .expect("fail to push return value");
}

/// Generate variable assignment statement
fn generate_lval_assign_stmt(ctx: &mut GenContext, bb: &mut BasicBlock, lval_assign: &LValAssign) {
    let LValAssign { lval, exp } = lval_assign;
    let ptr = if let Some(symbol) = ctx.lookup_symbol(&lval.ident) {
        match symbol {
            Symbol::Const(_) => panic!("Cannot assign to constant!"),
            Symbol::Var(var) => {
                if lval.indices.is_empty() {
                    var
                } else {
                    // Array access via pointer variable
                    let load = ctx.func_mut().dfg_mut().new_value().load(var);
                    ctx.func_mut()
                        .layout_mut()
                        .bb_mut(*bb)
                        .insts_mut()
                        .push_key_back(load)
                        .expect("failed to add load instruction");
                    generate_ptr(ctx, bb, load, &lval.indices, true)
                }
            }
            Symbol::Array(arr) => {
                // Array variable
                generate_ptr(ctx, bb, arr, &lval.indices, false)
            }
            Symbol::Func(_) => panic!("Cannot assign to function!"),
        }
    } else {
        panic!("Cannot assign a undefined variable!");
    };

    let res = generate_exp(ctx, bb, exp);
    let store = ctx.func_mut().dfg_mut().new_value().store(res, ptr);
    ctx.func_mut()
        .layout_mut()
        .bb_mut(*bb)
        .insts_mut()
        .push_key_back(store)
        .expect("failed to push store instruction");
}

/// Generate block statement
fn generate_block_stmt(ctx: &mut GenContext, bb: BasicBlock, blk: &Block) -> BasicBlock {
    ctx.enter_scope();
    let result_bb = generate_block(ctx, bb, blk);
    ctx.exit_scope();
    result_bb
}

/// Generate expression statement
fn generate_exp_stmt(ctx: &mut GenContext, bb: &mut BasicBlock, opt_exp: &Option<Exp>) {
    if let Some(exp) = opt_exp {
        generate_exp(ctx, bb, exp);
    }
}

/// Generate if statement
fn generate_if_stmt(ctx: &mut GenContext, mut bb: BasicBlock, if_stmt: &If) -> BasicBlock {
    let If {
        cond,
        then_stmt,
        else_stmt,
    } = &if_stmt;

    // 生成条件表达式的结果
    let cond_val = generate_exp(ctx, &mut bb, cond);
    let if_counter = ctx.if_counter; // Store the value in a temporary variable

    // 创建基本块
    let then_bb = ctx
        .func_mut()
        .dfg_mut()
        .new_bb()
        .basic_block(Some(format!("%then{}", if_counter)));
    let else_bb = if else_stmt.is_some() {
        Some(
            ctx.func_mut()
                .dfg_mut()
                .new_bb()
                .basic_block(Some(format!("%else{}", if_counter))),
        )
    } else {
        None
    };
    let end_bb = ctx
        .func_mut()
        .dfg_mut()
        .new_bb()
        .basic_block(Some(format!("%end{}", if_counter)));
    ctx.if_counter += 1;

    // 将基本块添加到函数布局中
    if let Some(else_bb) = else_bb {
        ctx.func_mut()
            .layout_mut()
            .bbs_mut()
            .extend([then_bb, else_bb, end_bb]);
    } else {
        ctx.func_mut()
            .layout_mut()
            .bbs_mut()
            .extend([then_bb, end_bb]);
    }

    // 创建分支指令
    let branch = if let Some(else_bb) = else_bb {
        ctx.func_mut()
            .dfg_mut()
            .new_value()
            .branch(cond_val, then_bb, else_bb)
    } else {
        ctx.func_mut()
            .dfg_mut()
            .new_value()
            .branch(cond_val, then_bb, end_bb)
    };
    ctx.func_mut()
        .layout_mut()
        .bb_mut(bb)
        .insts_mut()
        .push_key_back(branch)
        .expect("failed to add branch insturction");

    // 生成then分支
    let then_end_bb = generate_stmt(ctx, then_bb, then_stmt);
    if !ctx.is_bb_terminated(then_end_bb) {
        let jmp = ctx.func_mut().dfg_mut().new_value().jump(end_bb);
        ctx.func_mut()
            .layout_mut()
            .bb_mut(then_end_bb)
            .insts_mut()
            .push_key_back(jmp)
            .expect("failed to add jump instruction from then to end");
    }

    // 生成else分支
    if let Some(else_stmt) = else_stmt {
        let else_end_bb = generate_stmt(ctx, else_bb.unwrap(), else_stmt);
        if !ctx.is_bb_terminated(else_end_bb) {
            let jmp = ctx.func_mut().dfg_mut().new_value().jump(end_bb);
            ctx.func_mut()
                .layout_mut()
                .bb_mut(else_end_bb)
                .insts_mut()
                .push_key_back(jmp)
                .expect("failed to add jump instruction from else to end");
        }
    }

    end_bb
}

/// Generate while statement
fn generate_while_stmt(ctx: &mut GenContext, mut bb: BasicBlock, wh: &While) -> BasicBlock {
    let While { cond, body } = &wh;

    // 创建基本块
    let while_counter = ctx.while_counter; // Store the value in a temporary variable
    let entry_bb = ctx
        .func_mut()
        .dfg_mut()
        .new_bb()
        .basic_block(Some(format!("%while_entry{}", while_counter)));
    let body_bb = ctx
        .func_mut()
        .dfg_mut()
        .new_bb()
        .basic_block(Some(format!("%while_body{}", while_counter)));
    let end_bb = ctx
        .func_mut()
        .dfg_mut()
        .new_bb()
        .basic_block(Some(format!("%while_end{}", while_counter)));
    ctx.while_counter += 1;

    // 将基本块添加到函数布局中
    ctx.func_mut()
        .layout_mut()
        .bbs_mut()
        .extend([entry_bb, body_bb, end_bb]);

    // 创建跳转到entry的指令
    let jmp_to_entry = ctx.func_mut().dfg_mut().new_value().jump(entry_bb);
    ctx.func_mut()
        .layout_mut()
        .bb_mut(bb)
        .insts_mut()
        .push_key_back(jmp_to_entry)
        .expect("failed to add jump instruction to while entry");

    // 更新当前基本块为entry块
    bb = entry_bb;
    // 创建条件分支指令
    let cond_val = generate_exp(ctx, &mut bb, cond);
    let branch = ctx
        .func_mut()
        .dfg_mut()
        .new_value()
        .branch(cond_val, body_bb, end_bb);
    // 将分支指令添加到当前基本块
    ctx.func_mut()
        .layout_mut()
        .bb_mut(bb)
        .insts_mut()
        .push_key_back(branch)
        .expect("failed to add branch instruction in while");

    // 生成循环体
    ctx.enter_loop(entry_bb, end_bb);
    let body_end_bb = generate_stmt(ctx, body_bb, body);
    ctx.exit_loop();
    if !ctx.is_bb_terminated(body_end_bb) {
        let jmp_back = ctx.func_mut().dfg_mut().new_value().jump(entry_bb);
        ctx.func_mut()
            .layout_mut()
            .bb_mut(body_end_bb)
            .insts_mut()
            .push_key_back(jmp_back)
            .expect("failed to add jump instruction from body to entry");
    }

    // 返回循环结束块
    end_bb
}

/// Generate break statement
fn generate_break_stmt(ctx: &mut GenContext, bb: BasicBlock) {
    if let Some((_, break_bb)) = ctx.current_loop() {
        let jmp = ctx.func_mut().dfg_mut().new_value().jump(break_bb);
        ctx.func_mut()
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .push_key_back(jmp)
            .expect("failed to add jump instruction for break");
    } else {
        panic!("'break' statement not within a loop");
    }
}

/// Generate continue statement
fn generate_continue_stmt(ctx: &mut GenContext, bb: BasicBlock) {
    if let Some((continue_bb, _)) = ctx.current_loop() {
        let jmp = ctx.func_mut().dfg_mut().new_value().jump(continue_bb);
        ctx.func_mut()
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .push_key_back(jmp)
            .expect("failed to add jump instruction for continue");
    } else {
        panic!("'continue' statement not within a loop");
    }
}
