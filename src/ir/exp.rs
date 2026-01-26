use super::{GenContext, Symbol};
use crate::ast::*;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, BinaryOp, Value};

/// Generate Koopa IR for an expression
pub fn generate_exp(ctx: &mut GenContext, bb: BasicBlock, exp: &Exp) -> Value {
    generate_lor_exp(ctx, bb, &exp.lor_exp)
}

/// Generate Koopa IR for a unary expression
pub fn generate_unary_exp(ctx: &mut GenContext, bb: BasicBlock, unary_exp: &UnaryExp) -> Value {
    match unary_exp {
        UnaryExp::PrimaryExp(primary_exp) => generate_primary_exp(ctx, bb, primary_exp),
        UnaryExp::UnaryOp { op, exp } => {
            let val = generate_unary_exp(ctx, bb, exp);
            let zero = ctx.func_data.dfg_mut().new_value().integer(0);
            let result = match op {
                UnaryOp::Plus => val,
                UnaryOp::Minus => {
                    ctx.func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Sub, zero, val)
                }
                UnaryOp::Not => ctx
                    .func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::Eq, zero, val),
            };
            // 如果不是加号操作符, 则需要将结果加入到基本块中
            if !matches!(op, UnaryOp::Plus) {
                ctx.func_data
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
    ctx: &mut GenContext,
    bb: BasicBlock,
    primary_exp: &PrimaryExp,
) -> Value {
    match primary_exp {
        PrimaryExp::Exp(exp) => generate_exp(ctx, bb, exp),
        PrimaryExp::Number(num) => ctx.func_data.dfg_mut().new_value().integer(*num),
        PrimaryExp::LVal(lval) => {
            if let Some(&symbol) = ctx.symbol_table.get(&lval.ident) {
                match symbol {
                    Symbol::Const(val) => ctx.func_data.dfg_mut().new_value().integer(val),
                    Symbol::Var(val) => {
                        // 取出变量的值
                        let load = ctx.func_data.dfg_mut().new_value().load(val);
                        ctx.func_data
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
pub fn generate_add_exp(ctx: &mut GenContext, bb: BasicBlock, add_exp: &AddExp) -> Value {
    match add_exp {
        AddExp::MulExp(mul_exp) => generate_mul_exp(ctx, bb, mul_exp),
        AddExp::AddOp { lhs, op, rhs } => {
            let lhs_val = generate_add_exp(ctx, bb, lhs);
            let rhs_val = generate_mul_exp(ctx, bb, rhs);
            let result = match op {
                AddOp::Plus => {
                    ctx.func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Add, lhs_val, rhs_val)
                }
                AddOp::Minus => {
                    ctx.func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Sub, lhs_val, rhs_val)
                }
            };
            ctx.func_data
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
pub fn generate_mul_exp(ctx: &mut GenContext, bb: BasicBlock, mul_exp: &MulExp) -> Value {
    match mul_exp {
        MulExp::UnaryExp(unary_exp) => generate_unary_exp(ctx, bb, unary_exp),
        MulExp::MulOp { lhs, op, rhs } => {
            let lhs_val = generate_mul_exp(ctx, bb, lhs);
            let rhs_val = generate_unary_exp(ctx, bb, rhs);
            let result = match op {
                MulOp::Mul => {
                    ctx.func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Mul, lhs_val, rhs_val)
                }
                MulOp::Div => {
                    ctx.func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Div, lhs_val, rhs_val)
                }
                MulOp::Mod => {
                    ctx.func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Mod, lhs_val, rhs_val)
                }
            };
            ctx.func_data
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
pub fn generate_rel_exp(ctx: &mut GenContext, bb: BasicBlock, rel_exp: &RelExp) -> Value {
    match rel_exp {
        RelExp::AddExp(add_exp) => generate_add_exp(ctx, bb, add_exp),
        RelExp::RelOp { lhs, op, rhs } => {
            let lhs_val = generate_rel_exp(ctx, bb, lhs);
            let rhs_val = generate_add_exp(ctx, bb, rhs);
            let result = match op {
                RelOp::Lt => {
                    ctx.func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Lt, lhs_val, rhs_val)
                }
                RelOp::Gt => {
                    ctx.func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Gt, lhs_val, rhs_val)
                }
                RelOp::Le => {
                    ctx.func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Le, lhs_val, rhs_val)
                }
                RelOp::Ge => {
                    ctx.func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Ge, lhs_val, rhs_val)
                }
            };
            ctx.func_data
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
pub fn generate_eq_exp(ctx: &mut GenContext, bb: BasicBlock, eq_exp: &EqExp) -> Value {
    match eq_exp {
        EqExp::RelExp(rel_exp) => generate_rel_exp(ctx, bb, rel_exp),
        EqExp::EqOp { lhs, op, rhs } => {
            let lhs_val = generate_eq_exp(ctx, bb, lhs);
            let rhs_val = generate_rel_exp(ctx, bb, rhs);
            let result = match op {
                EqOp::Eq => {
                    ctx.func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Eq, lhs_val, rhs_val)
                }
                EqOp::Ne => {
                    ctx.func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::NotEq, lhs_val, rhs_val)
                }
            };
            ctx.func_data
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
pub fn generate_land_exp(ctx: &mut GenContext, bb: BasicBlock, land_exp: &LAndExp) -> Value {
    match land_exp {
        LAndExp::EqExp(eq_exp) => generate_eq_exp(ctx, bb, eq_exp),
        LAndExp::LAndOp { lhs, rhs } => {
            let lhs_val = generate_land_exp(ctx, bb, lhs);
            let rhs_val = generate_eq_exp(ctx, bb, rhs);
            let zero = ctx.func_data.dfg_mut().new_value().integer(0);
            let lhs_ne0 =
                ctx.func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::NotEq, lhs_val, zero);
            let rhs_ne0 =
                ctx.func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::NotEq, rhs_val, zero);
            let result =
                ctx.func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::And, lhs_ne0, rhs_ne0);
            ctx.func_data
                .layout_mut()
                .bb_mut(bb)
                .insts_mut()
                .extend([lhs_ne0, rhs_ne0, result]);
            result
        }
    }
}

/// Generate Koopa IR for a logical OR expression
pub fn generate_lor_exp(ctx: &mut GenContext, bb: BasicBlock, lor_exp: &LOrExp) -> Value {
    match lor_exp {
        LOrExp::LAndExp(land_exp) => generate_land_exp(ctx, bb, land_exp),
        LOrExp::LOrOp { lhs, rhs } => {
            let lhs_val = generate_lor_exp(ctx, bb, lhs);
            let rhs_val = generate_land_exp(ctx, bb, rhs);
            let zero = ctx.func_data.dfg_mut().new_value().integer(0);
            let lhs_ne0 =
                ctx.func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::NotEq, lhs_val, zero);
            let rhs_ne0 =
                ctx.func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::NotEq, rhs_val, zero);
            let result = ctx
                .func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::Or, lhs_ne0, rhs_ne0);
            ctx.func_data
                .layout_mut()
                .bb_mut(bb)
                .insts_mut()
                .extend([lhs_ne0, rhs_ne0, result]);
            result
        }
    }
}
