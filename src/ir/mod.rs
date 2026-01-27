mod eval;
mod exp;

use crate::ast::{self, Block, BlockItem, Decl, If, Stmt, While};
use eval::Eval;
use exp::generate_exp;
use koopa::ir::{BasicBlock, FunctionData, Program, Type, Value};
use koopa::ir::{ValueKind, builder_traits::*};
use std::collections::HashMap;

/// enumerate for symbol table
#[derive(Clone, Copy)]
pub enum Symbol {
    Const(i32),
    Var(Value),
}

/// Context for Koopa IR generation
pub struct GenContext<'a> {
    func_data: &'a mut FunctionData,
    scopes: Vec<HashMap<String, Symbol>>,
    loop_stack: Vec<(BasicBlock, BasicBlock)>, // (continue_bb, break_bb)
    if_counter: usize,
    while_counter: usize,
    land_counter: usize,
    lor_counter: usize,
}

impl<'a> GenContext<'a> {
    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Exit the current scope
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    /// Insert a symbol into the current scope
    pub fn insert_symbol(&mut self, ident: String, symbol: Symbol) {
        self.scopes.last_mut().unwrap().insert(ident, symbol);
    }

    /// Lookup a symbol in the scopes
    pub fn lookup_symbol(&self, ident: &str) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(ident) {
                return Some(*symbol);
            }
        }
        None
    }

    /// Enter a loop with given continue and break basic blocks
    fn enter_loop(&mut self, entry: BasicBlock, end: BasicBlock) {
        self.loop_stack.push((entry, end));
    }

    /// Exit the current loop
    fn exit_loop(&mut self) {
        self.loop_stack.pop();
    }

    /// Get the continue and break basic blocks of the current loop
    fn current_loop(&self) -> Option<(BasicBlock, BasicBlock)> {
        self.loop_stack.last().cloned()
    }

    /// Check if a basic block is terminated
    pub fn is_bb_terminated(&mut self, bb: BasicBlock) -> bool {
        for (b, node) in self.func_data.layout().bbs() {
            if *b == bb {
                if let Some(last_inst) = node.insts().back_key() {
                    let last = self.func_data.dfg().value(*last_inst);
                    match last.kind() {
                        ValueKind::Return(_) | ValueKind::Branch(_) | ValueKind::Jump(_) => {
                            return true;
                        }
                        _ => return false,
                    }
                } else {
                    return false;
                }
            }
        }
        false
    }
}

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

/// Generate Koopa IR for a statement and return the latest insertion block
pub fn generate_stmt(ctx: &mut GenContext, bb: BasicBlock, stmt: &Stmt) -> BasicBlock {
    let mut current_bb = bb;
    match stmt {
        Stmt::Return(exp) => {
            // 生成语句的中间表示
            let ret_val = generate_exp(ctx, &mut current_bb, exp);
            let ret = ctx.func_data.dfg_mut().new_value().ret(Some(ret_val));
            // 将返回语句添加到基本块中
            ctx.func_data
                .layout_mut()
                .bb_mut(current_bb)
                .insts_mut()
                .push_key_back(ret)
                .expect("fail to push return value");
        }
        Stmt::LValAssign { lval, exp } => {
            let var = if let Some(symbol) = ctx.lookup_symbol(&lval.ident) {
                match symbol {
                    Symbol::Const(_) => unreachable!(),
                    Symbol::Var(var) => var,
                }
            } else {
                panic!("Cannot assign a undefined variable!");
            };
            // 生成表达式的结果
            let res = generate_exp(ctx, &mut current_bb, exp);
            // 将结果存入symbol table
            let store = ctx.func_data.dfg_mut().new_value().store(res, var);
            ctx.func_data
                .layout_mut()
                .bb_mut(current_bb)
                .insts_mut()
                .push_key_back(store)
                .expect("failed to push store instruction");
        }
        Stmt::Block(blk) => {
            ctx.enter_scope();
            current_bb = generate_block(ctx, current_bb, blk);
            ctx.exit_scope();
        }
        Stmt::Exp(opt_exp) => {
            if let Some(exp) = opt_exp {
                generate_exp(ctx, &mut current_bb, exp);
            } else {
                // 空表达式语句，不做任何操作
            }
        }
        Stmt::If(if_stmt) => {
            let If {
                cond,
                then_stmt,
                else_stmt,
            } = &if_stmt;
            // 生成条件表达式的结果
            let cond_val = generate_exp(ctx, &mut current_bb, cond);
            // 创建基本块
            let then_bb = ctx
                .func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some(format!("%then{}", ctx.if_counter)));
            let else_bb = if else_stmt.is_some() {
                Some(
                    ctx.func_data
                        .dfg_mut()
                        .new_bb()
                        .basic_block(Some(format!("%else{}", ctx.if_counter))),
                )
            } else {
                None
            };
            let end_bb = ctx
                .func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some(format!("%end{}", ctx.if_counter)));
            ctx.if_counter += 1;
            // 将基本块添加到函数布局中
            if let Some(else_bb) = else_bb {
                ctx.func_data
                    .layout_mut()
                    .bbs_mut()
                    .extend([then_bb, else_bb, end_bb]);
            } else {
                ctx.func_data
                    .layout_mut()
                    .bbs_mut()
                    .extend([then_bb, end_bb]);
            }
            // 创建分支指令
            let branch = if let Some(else_bb) = else_bb {
                ctx.func_data
                    .dfg_mut()
                    .new_value()
                    .branch(cond_val, then_bb, else_bb)
            } else {
                ctx.func_data
                    .dfg_mut()
                    .new_value()
                    .branch(cond_val, then_bb, end_bb)
            };
            ctx.func_data
                .layout_mut()
                .bb_mut(current_bb)
                .insts_mut()
                .push_key_back(branch)
                .expect("failed to add branch insturction");
            // 生成then分支
            let then_end_bb = generate_stmt(ctx, then_bb, then_stmt);
            if !ctx.is_bb_terminated(then_end_bb) {
                let jmp = ctx.func_data.dfg_mut().new_value().jump(end_bb);
                ctx.func_data
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
                    let jmp = ctx.func_data.dfg_mut().new_value().jump(end_bb);
                    ctx.func_data
                        .layout_mut()
                        .bb_mut(else_end_bb)
                        .insts_mut()
                        .push_key_back(jmp)
                        .expect("failed to add jump instruction from else to end");
                }
            }
            current_bb = end_bb;
        }
        Stmt::While(wh) => {
            let While { cond, body } = &wh;
            // 创建基本块
            let entry_bb = ctx
                .func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some(format!("%while_entry{}", ctx.while_counter)));
            let body_bb = ctx
                .func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some(format!("%while_body{}", ctx.while_counter)));
            let end_bb = ctx
                .func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some(format!("%while_end{}", ctx.while_counter)));
            ctx.while_counter += 1;
            // 将基本块添加到函数布局中
            ctx.func_data
                .layout_mut()
                .bbs_mut()
                .extend([entry_bb, body_bb, end_bb]);
            // 创建跳转到entry的指令
            let jmp_to_entry = ctx.func_data.dfg_mut().new_value().jump(entry_bb);
            ctx.func_data
                .layout_mut()
                .bb_mut(current_bb)
                .insts_mut()
                .push_key_back(jmp_to_entry)
                .expect("failed to add jump instruction to while entry");

            // 更新当前基本块为entry块
            current_bb = entry_bb;
            // 创建条件分支指令
            let cond_val = generate_exp(ctx, &mut current_bb, cond);
            let branch = ctx
                .func_data
                .dfg_mut()
                .new_value()
                .branch(cond_val, body_bb, end_bb);
            // 将分支指令添加到当前基本块
            ctx.func_data
                .layout_mut()
                .bb_mut(current_bb)
                .insts_mut()
                .push_key_back(branch)
                .expect("failed to add branch instruction in while");

            // 生成循环体
            ctx.enter_loop(entry_bb, end_bb);
            let body_end_bb = generate_stmt(ctx, body_bb, body);
            ctx.exit_loop();
            if !ctx.is_bb_terminated(body_end_bb) {
                let jmp_back = ctx.func_data.dfg_mut().new_value().jump(entry_bb);
                ctx.func_data
                    .layout_mut()
                    .bb_mut(body_end_bb)
                    .insts_mut()
                    .push_key_back(jmp_back)
                    .expect("failed to add jump instruction from body to entry");
            }

            // 更新当前基本块为循环结束块
            current_bb = end_bb;
        }
        Stmt::Break(_) => {
            if let Some((_, break_bb)) = ctx.current_loop() {
                let jmp = ctx.func_data.dfg_mut().new_value().jump(break_bb);
                ctx.func_data
                    .layout_mut()
                    .bb_mut(current_bb)
                    .insts_mut()
                    .push_key_back(jmp)
                    .expect("failed to add jump instruction for break");
            } else {
                panic!("'break' statement not within a loop");
            }
        }
        Stmt::Continue(_) => {
            if let Some((continue_bb, _)) = ctx.current_loop() {
                let jmp = ctx.func_data.dfg_mut().new_value().jump(continue_bb);
                ctx.func_data
                    .layout_mut()
                    .bb_mut(current_bb)
                    .insts_mut()
                    .push_key_back(jmp)
                    .expect("failed to add jump instruction for continue");
            } else {
                panic!("'continue' statement not within a loop");
            }
        }
    }
    current_bb
}

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
