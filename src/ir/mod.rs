mod eval;
mod exp;

use crate::ast::{self, Block, BlockItem, Decl, Stmt};
use eval::Eval;
use exp::generate_exp;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, FunctionData, Program, Type, Value};
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
    symbol_table: HashMap<String, Symbol>,
}

/// Generate Koopa IR from the AST
pub fn generate_koopa(ast: &ast::CompUnit) -> Program {
    // create the program and function
    let mut program = Program::new();
    let func_data = FunctionData::new(format!("@{}", ast.func_def.ident), vec![], Type::get_i32());
    let func = program.new_func(func_data);
    let mut ctx = GenContext {
        func_data: program.func_mut(func),
        symbol_table: HashMap::new(),
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

/// Generate Koopa IR for a block
pub fn generate_block(ctx: &mut GenContext, bb: BasicBlock, block: &Block) {
    for block_item in &block.block_items {
        match block_item {
            BlockItem::Stmt(stmt) => {
                match stmt {
                    Stmt::Return(exp) => {
                        // 生成语句的中间表示
                        let ret_val = generate_exp(ctx, bb, exp);
                        let ret = ctx.func_data.dfg_mut().new_value().ret(Some(ret_val));
                        // 将返回语句添加到基本块中
                        ctx.func_data
                            .layout_mut()
                            .bb_mut(bb)
                            .insts_mut()
                            .push_key_back(ret)
                            .expect("fail to push return value");
                    }
                    Stmt::LValAssign { lval, exp } => {
                        let var = if let Some(symbol) = ctx.symbol_table.get(&lval.ident) {
                            match symbol {
                                Symbol::Const(_) => unreachable!(),
                                Symbol::Var(var) => *var,
                            }
                        } else {
                            panic!("Cannot assign a undefined variable!");
                        };
                        // 生成表达式的结果
                        let res = generate_exp(ctx, bb, exp);
                        // 将结果存入symbol table
                        let store = ctx.func_data.dfg_mut().new_value().store(res, var);
                        ctx.func_data
                            .layout_mut()
                            .bb_mut(bb)
                            .insts_mut()
                            .push_key_back(store)
                            .expect("failed to push store instruction");
                    }
                }
            }
            BlockItem::Decl(decl) => {
                generate_decl(ctx, bb, &decl);
            }
        }
    }
}

/// Generate Koopa IR for a declaration
pub fn generate_decl(ctx: &mut GenContext, bb: BasicBlock, decl: &Decl) {
    match decl {
        Decl::ConstDecl(const_decl) => {
            for def in &const_decl.const_defs {
                let exp = &def.const_init_val.const_exp.exp;
                let value = exp.evaluate(&mut ctx.symbol_table);
                ctx.symbol_table
                    .insert(def.ident.clone(), Symbol::Const(value));
            }
        }
        Decl::VarDecl(var_decl) => {
            for def in &var_decl.var_defs {
                // 在栈上分配内存
                let alloc = ctx.func_data.dfg_mut().new_value().alloc(Type::get_i32());
                ctx.func_data
                    .layout_mut()
                    .bb_mut(bb)
                    .insts_mut()
                    .push_key_back(alloc)
                    .expect("failed to add alloc instruction");
                ctx.symbol_table
                    .insert(def.ident.clone(), Symbol::Var(alloc));
                // 处理初始化值
                if let Some(init_val) = &def.init_val {
                    let res = generate_exp(ctx, bb, &init_val.exp);
                    let store = ctx.func_data.dfg_mut().new_value().store(res, alloc);
                    ctx.func_data
                        .layout_mut()
                        .bb_mut(bb)
                        .insts_mut()
                        .push_key_back(store)
                        .expect("failed to add store instruction");
                }
            }
        }
    }
}
