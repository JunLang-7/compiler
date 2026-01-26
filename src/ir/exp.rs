use super::Symbol;
use crate::ast::*;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, BinaryOp, FunctionData, Value};
use std::collections::HashMap;

/// Generate Koopa IR for an expression
pub fn generate_exp(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    exp: &Exp,
    symbol_table: &mut HashMap<String, Symbol>,
) -> Value {
    generate_lor_exp(func_data, bb, &exp.lor_exp, symbol_table)
}

/// Generate Koopa IR for a unary expression
pub fn generate_unary_exp(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    unary_exp: &UnaryExp,
    symbol_table: &mut HashMap<String, Symbol>,
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
    primary_exp: &PrimaryExp,
    symbol_table: &mut HashMap<String, Symbol>,
) -> Value {
    match primary_exp {
        PrimaryExp::Exp(exp) => generate_exp(func_data, bb, exp, symbol_table),
        PrimaryExp::Number(num) => func_data.dfg_mut().new_value().integer(*num),
        PrimaryExp::LVal(lval) => {
            if let Some(&symbol) = symbol_table.get(&lval.ident) {
                match symbol {
                    Symbol::Const(val) => func_data.dfg_mut().new_value().integer(val),
                    Symbol::Var(val) => {
                        // 取出变量的值
                        let load = func_data.dfg_mut().new_value().load(val);
                        func_data
                            .layout_mut()
                            .bb_mut(bb)
                            .insts_mut()
                            .push_key_back(load)
                            .expect("failed to add load instruction");
                        load
                    }
                }
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
    add_exp: &AddExp,
    symbol_table: &mut HashMap<String, Symbol>,
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
    mul_exp: &MulExp,
    symbol_table: &mut HashMap<String, Symbol>,
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
    symbol_table: &mut HashMap<String, Symbol>,
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
    eq_exp: &EqExp,
    symbol_table: &mut HashMap<String, Symbol>,
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
    symbol_table: &mut HashMap<String, Symbol>,
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
    symbol_table: &mut HashMap<String, Symbol>,
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
