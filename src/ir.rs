use std::collections::HashMap;

use crate::ast::{
    self, AddExp, AddOp, Block, BlockItem, Decl, EqExp, EqOp, Exp, LAndExp, LOrExp, MulExp, MulOp,
    PrimaryExp, RelExp, RelOp, UnaryExp, UnaryOp,
};
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, BinaryOp, FunctionData, Program, Type, Value};

/// Generate Koopa IR from the AST
pub fn generate_koopa(ast: &ast::CompUnit) -> Program {
    // create the program and function
    let mut program = Program::new();
    let func_data = FunctionData::new(format!("@{}", ast.func_def.ident), vec![], Type::get_i32());
    let func = program.new_func(func_data);
    let func_data = program.func_mut(func);
    let mut symbol_table: HashMap<String, i32> = HashMap::new();

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
    symbol_table: &mut HashMap<String, i32>,
) {
    for block_item in &block.block_items {
        match block_item {
            BlockItem::Stmt(stmt) => {
                // 生成语句的中间表示
                let ret_val = generate_exp(func_data, bb, &stmt.exp, symbol_table);
                let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
                // 将返回语句添加到基本块中
                func_data
                    .layout_mut()
                    .bb_mut(bb)
                    .insts_mut()
                    .push_key_back(ret)
                    .expect("fail to push return value");
            }
            BlockItem::Decl(decl) => {
                generate_decl(&decl, symbol_table);
            }
        }
    }
}

/// Generate Koopa IR for a declaration
pub fn generate_decl(decl: &Decl, symbol_table: &mut HashMap<String, i32>) {
    match decl {
        Decl::ConstDecl(const_decl) => {
            for def in &const_decl.const_defs {
                let exp = &def.const_init_val.const_exp.exp;
                let value = exp.evaluate(symbol_table);
                symbol_table.insert(def.ident.clone(), value);
            }
        }
    }
}

/// Generate Koopa IR for an expression
pub fn generate_exp(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    exp: &Exp,
    symbol_table: &mut HashMap<String, i32>,
) -> Value {
    generate_lor_exp(func_data, bb, &exp.lor_exp, symbol_table)
}

/// Generate Koopa IR for a unary expression
pub fn generate_unary_exp(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    unary_exp: &ast::UnaryExp,
    symbol_table: &mut HashMap<String, i32>,
) -> Value {
    match unary_exp {
        UnaryExp::PrimaryExp(primary_exp) => {
            generate_primary_exp(func_data, bb, primary_exp, symbol_table)
        }
        UnaryExp::UnaryOp { op, exp } => {
            let val = generate_unary_exp(func_data, bb, exp, symbol_table);
            let zero = func_data.dfg_mut().new_value().integer(0);
            let result = match op {
                UnaryOp::Plus => val,
                UnaryOp::Minus => func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::Sub, zero, val),
                UnaryOp::Not => func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::Eq, zero, val),
            };
            // 如果不是加号操作符, 则需要将结果加入到基本块中
            if !matches!(op, UnaryOp::Plus) {
                func_data
                    .layout_mut()
                    .bb_mut(bb)
                    .insts_mut()
                    .push_key_back(result)
                    .expect("failed to push unary exp");
            }
            result
        }
    }
}

/// Generate Koopa IR for a primary expression
pub fn generate_primary_exp(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    primary_exp: &ast::PrimaryExp,
    symbol_table: &mut HashMap<String, i32>,
) -> Value {
    match primary_exp {
        PrimaryExp::Exp(exp) => generate_exp(func_data, bb, exp, symbol_table),
        PrimaryExp::Number(num) => func_data.dfg_mut().new_value().integer(*num),
        PrimaryExp::LVal(lval) => {
            if let Some(&val) = symbol_table.get(&lval.ident) {
                func_data.dfg_mut().new_value().integer(val)
            } else {
                panic!("Undefined variable: {}", lval.ident);
            }
        }
    }
}

/// Generate Koopa IR for an additive expression
pub fn generate_add_exp(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    add_exp: &ast::AddExp,
    symbol_table: &mut HashMap<String, i32>,
) -> Value {
    match add_exp {
        AddExp::MulExp(mul_exp) => generate_mul_exp(func_data, bb, mul_exp, symbol_table),
        AddExp::AddOp { lhs, op, rhs } => {
            let lhs_val = generate_add_exp(func_data, bb, lhs, symbol_table);
            let rhs_val = generate_mul_exp(func_data, bb, rhs, symbol_table);
            let result = match op {
                AddOp::Plus => {
                    func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Add, lhs_val, rhs_val)
                }
                AddOp::Minus => {
                    func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Sub, lhs_val, rhs_val)
                }
            };
            func_data
                .layout_mut()
                .bb_mut(bb)
                .insts_mut()
                .push_key_back(result)
                .expect("failed to push add exp");
            result
        }
    }
}

/// Generate Koopa IR for a multiplicative expression
pub fn generate_mul_exp(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    mul_exp: &ast::MulExp,
    symbol_table: &mut HashMap<String, i32>,
) -> Value {
    match mul_exp {
        MulExp::UnaryExp(unary_exp) => generate_unary_exp(func_data, bb, unary_exp, symbol_table),
        MulExp::MulOp { lhs, op, rhs } => {
            let lhs_val = generate_mul_exp(func_data, bb, lhs, symbol_table);
            let rhs_val = generate_unary_exp(func_data, bb, rhs, symbol_table);
            let result = match op {
                MulOp::Mul => {
                    func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Mul, lhs_val, rhs_val)
                }
                MulOp::Div => {
                    func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Div, lhs_val, rhs_val)
                }
                MulOp::Mod => {
                    func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Mod, lhs_val, rhs_val)
                }
            };
            func_data
                .layout_mut()
                .bb_mut(bb)
                .insts_mut()
                .push_key_back(result)
                .expect("failed to push mul exp");
            result
        }
    }
}

/// Generate Koopa IR for a relational expression
pub fn generate_rel_exp(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    rel_exp: &RelExp,
    symbol_table: &mut HashMap<String, i32>,
) -> Value {
    match rel_exp {
        RelExp::AddExp(add_exp) => generate_add_exp(func_data, bb, add_exp, symbol_table),
        RelExp::RelOp { lhs, op, rhs } => {
            let lhs_val = generate_rel_exp(func_data, bb, lhs, symbol_table);
            let rhs_val = generate_add_exp(func_data, bb, rhs, symbol_table);
            let result = match op {
                RelOp::Lt => func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::Lt, lhs_val, rhs_val),
                RelOp::Gt => func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::Gt, lhs_val, rhs_val),
                RelOp::Le => func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::Le, lhs_val, rhs_val),
                RelOp::Ge => func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::Ge, lhs_val, rhs_val),
            };
            func_data
                .layout_mut()
                .bb_mut(bb)
                .insts_mut()
                .push_key_back(result)
                .expect("failed to push rel exp");
            result
        }
    }
}

/// Generate Koopa IR for an equality expression
pub fn generate_eq_exp(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    eq_exp: &ast::EqExp,
    symbol_table: &mut HashMap<String, i32>,
) -> Value {
    match eq_exp {
        EqExp::RelExp(rel_exp) => generate_rel_exp(func_data, bb, rel_exp, symbol_table),
        EqExp::EqOp { lhs, op, rhs } => {
            let lhs_val = generate_eq_exp(func_data, bb, lhs, symbol_table);
            let rhs_val = generate_rel_exp(func_data, bb, rhs, symbol_table);
            let result = match op {
                EqOp::Eq => func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::Eq, lhs_val, rhs_val),
                EqOp::Ne => {
                    func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::NotEq, lhs_val, rhs_val)
                }
            };
            func_data
                .layout_mut()
                .bb_mut(bb)
                .insts_mut()
                .push_key_back(result)
                .expect("failed to push eq exp");
            result
        }
    }
}

/// Generate Koopa IR for a logical AND expression
pub fn generate_land_exp(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    land_exp: &LAndExp,
    symbol_table: &mut HashMap<String, i32>,
) -> Value {
    match land_exp {
        LAndExp::EqExp(eq_exp) => generate_eq_exp(func_data, bb, eq_exp, symbol_table),
        LAndExp::LAndOp { lhs, rhs } => {
            let lhs_val = generate_land_exp(func_data, bb, lhs, symbol_table);
            let rhs_val = generate_eq_exp(func_data, bb, rhs, symbol_table);
            let zero = func_data.dfg_mut().new_value().integer(0);
            let lhs_ne0 = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, lhs_val, zero);
            let rhs_ne0 = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, rhs_val, zero);
            let result = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::And, lhs_ne0, rhs_ne0);
            func_data
                .layout_mut()
                .bb_mut(bb)
                .insts_mut()
                .extend([lhs_ne0, rhs_ne0, result]);
            result
        }
    }
}

/// Generate Koopa IR for a logical OR expression
pub fn generate_lor_exp(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    lor_exp: &LOrExp,
    symbol_table: &mut HashMap<String, i32>,
) -> Value {
    match lor_exp {
        LOrExp::LAndExp(land_exp) => generate_land_exp(func_data, bb, land_exp, symbol_table),
        LOrExp::LOrOp { lhs, rhs } => {
            let lhs_val = generate_lor_exp(func_data, bb, lhs, symbol_table);
            let rhs_val = generate_land_exp(func_data, bb, rhs, symbol_table);
            let zero = func_data.dfg_mut().new_value().integer(0);
            let lhs_ne0 = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, lhs_val, zero);
            let rhs_ne0 = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, rhs_val, zero);
            let result = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::Or, lhs_ne0, rhs_ne0);
            func_data
                .layout_mut()
                .bb_mut(bb)
                .insts_mut()
                .extend([lhs_ne0, rhs_ne0, result]);
            result
        }
    }
}
