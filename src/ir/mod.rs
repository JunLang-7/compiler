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

/// Generate Koopa IR from the AST
pub fn generate_koopa(ast: &ast::CompUnit) -> Program {
    // create the program and function
    let mut program = Program::new();
    let func_data = FunctionData::new(format!("@{}", ast.func_def.ident), vec![], Type::get_i32());
    let func = program.new_func(func_data);
    let func_data = program.func_mut(func);
    let mut symbol_table: HashMap<String, Symbol> = HashMap::new();

    // 在函数中创建一个基本块(entry basic block)
    let entry = func_data
        .dfg_mut()
        .new_bb()
        .basic_block(Some("%entry".into()));
    // 将基本块添加到函数布局中
    func_data
        .layout_mut()
        .bbs_mut()
        .push_key_back(entry)
        .expect("fail to push entry bb");
    // 生成函数体的中间表示
    generate_block(func_data, entry, &ast.func_def.block, &mut symbol_table);

    program
}

/// Generate Koopa IR for a block
pub fn generate_block(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    block: &Block,
    symbol_table: &mut HashMap<String, Symbol>,
) {
    for block_item in &block.block_items {
        match block_item {
            BlockItem::Stmt(stmt) => {
                match stmt {
                    Stmt::Return(exp) => {
                        // 生成语句的中间表示
                        let ret_val = generate_exp(func_data, bb, exp, symbol_table);
                        let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
                        // 将返回语句添加到基本块中
                        func_data
                            .layout_mut()
                            .bb_mut(bb)
                            .insts_mut()
                            .push_key_back(ret)
                            .expect("fail to push return value");
                    }
                    Stmt::LValAssign { lval, exp } => {
                        let var = if let Some(symbol) = symbol_table.get(&lval.ident) {
                            match symbol {
                                Symbol::Const(_) => unreachable!(),
                                Symbol::Var(var) => *var,
                            }
                        } else {
                            panic!("Cannot assign a undefined variable!");
                        };
                        // 生成表达式的结果
                        let res = generate_exp(func_data, bb, exp, symbol_table);
                        // 将结果存入symbol table
                        let store = func_data.dfg_mut().new_value().store(res, var);
                        func_data
                            .layout_mut()
                            .bb_mut(bb)
                            .insts_mut()
                            .push_key_back(store)
                            .expect("failed to push store instruction");
                    }
                }
            }
            BlockItem::Decl(decl) => {
                generate_decl(func_data, bb, &decl, symbol_table);
            }
        }
    }
}

/// Generate Koopa IR for a declaration
pub fn generate_decl(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    decl: &Decl,
    symbol_table: &mut HashMap<String, Symbol>,
) {
    match decl {
        Decl::ConstDecl(const_decl) => {
            for def in &const_decl.const_defs {
                let exp = &def.const_init_val.const_exp.exp;
                let value = exp.evaluate(symbol_table);
                symbol_table.insert(def.ident.clone(), Symbol::Const(value));
            }
        }
        Decl::VarDecl(var_decl) => {
            for def in &var_decl.var_defs {
                // 在栈上分配内存
                let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                func_data
                    .layout_mut()
                    .bb_mut(bb)
                    .insts_mut()
                    .push_key_back(alloc)
                    .expect("failed to add alloc instruction");
                symbol_table.insert(def.ident.clone(), Symbol::Var(alloc));
                // 处理初始化值
                if let Some(init_val) = &def.init_val {
                    let res = generate_exp(func_data, bb, &init_val.exp, symbol_table);
                    let store = func_data.dfg_mut().new_value().store(res, alloc);
                    func_data
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
