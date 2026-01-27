use super::{GenContext, Symbol};
use crate::ast::*;
use koopa::ir::{BasicBlock, BinaryOp, Value};
use koopa::ir::{Type, builder_traits::*};

/// Generate Koopa IR for an expression
pub fn generate_exp(ctx: &mut GenContext, bb: &mut BasicBlock, exp: &Exp) -> Value {
    generate_lor_exp(ctx, bb, &exp.lor_exp)
}

/// Generate Koopa IR for a unary expression
pub fn generate_unary_exp(
    ctx: &mut GenContext,
    bb: &mut BasicBlock,
    unary_exp: &UnaryExp,
) -> Value {
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
                    .bb_mut(*bb)
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
    bb: &mut BasicBlock,
    primary_exp: &PrimaryExp,
) -> Value {
    match primary_exp {
        PrimaryExp::Exp(exp) => generate_exp(ctx, bb, exp),
        PrimaryExp::Number(num) => ctx.func_data.dfg_mut().new_value().integer(*num),
        PrimaryExp::LVal(lval) => {
            if let Some(symbol) = ctx.lookup_symbol(&lval.ident) {
                match symbol {
                    Symbol::Const(val) => ctx.func_data.dfg_mut().new_value().integer(val),
                    Symbol::Var(val) => {
                        // 取出变量的值
                        let load = ctx.func_data.dfg_mut().new_value().load(val);
                        ctx.func_data
                            .layout_mut()
                            .bb_mut(*bb)
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
pub fn generate_add_exp(ctx: &mut GenContext, bb: &mut BasicBlock, add_exp: &AddExp) -> Value {
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
                .bb_mut(*bb)
                .insts_mut()
                .push_key_back(result)
                .expect("failed to push add exp");
            result
        }
    }
}

/// Generate Koopa IR for a multiplicative expression
pub fn generate_mul_exp(ctx: &mut GenContext, bb: &mut BasicBlock, mul_exp: &MulExp) -> Value {
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
                .bb_mut(*bb)
                .insts_mut()
                .push_key_back(result)
                .expect("failed to push mul exp");
            result
        }
    }
}

/// Generate Koopa IR for a relational expression
pub fn generate_rel_exp(ctx: &mut GenContext, bb: &mut BasicBlock, rel_exp: &RelExp) -> Value {
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
                .bb_mut(*bb)
                .insts_mut()
                .push_key_back(result)
                .expect("failed to push rel exp");
            result
        }
    }
}

/// Generate Koopa IR for an equality expression
pub fn generate_eq_exp(ctx: &mut GenContext, bb: &mut BasicBlock, eq_exp: &EqExp) -> Value {
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
                .bb_mut(*bb)
                .insts_mut()
                .push_key_back(result)
                .expect("failed to push eq exp");
            result
        }
    }
}

/// Generate Koopa IR for a logical AND expression
pub fn generate_land_exp(ctx: &mut GenContext, bb: &mut BasicBlock, land_exp: &LAndExp) -> Value {
    match land_exp {
        LAndExp::EqExp(eq_exp) => generate_eq_exp(ctx, bb, eq_exp),
        LAndExp::LAndOp { lhs, rhs } => {
            // 处理短路逻辑
            let lhs_val = generate_land_exp(ctx, bb, lhs);
            // 创建两个基本块
            let true_bb = ctx
                .func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some(format!("%land_true{}", ctx.land_counter)));
            let end_bb = ctx
                .func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some(format!("%land_end{}", ctx.land_counter)));
            ctx.func_data
                .layout_mut()
                .bbs_mut()
                .extend([true_bb, end_bb]);
            ctx.land_counter += 1;
            // 创建初始指令
            let res_ptr = ctx.func_data.dfg_mut().new_value().alloc(Type::get_i32());
            let zero = ctx.func_data.dfg_mut().new_value().integer(0);
            let init_store = ctx.func_data.dfg_mut().new_value().store(zero, res_ptr);
            let br = ctx
                .func_data
                .dfg_mut()
                .new_value()
                .branch(lhs_val, true_bb, end_bb);
            ctx.func_data
                .layout_mut()
                .bb_mut(*bb)
                .insts_mut()
                .extend([res_ptr, init_store, br]);

            // 处理true分支
            *bb = true_bb;
            let rhs_val = generate_eq_exp(ctx, bb, rhs);
            let rhs_ne0 =
                ctx.func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::NotEq, rhs_val, zero);
            let store = ctx.func_data.dfg_mut().new_value().store(rhs_ne0, res_ptr);
            let jmp = ctx.func_data.dfg_mut().new_value().jump(end_bb);
            ctx.func_data
                .layout_mut()
                .bb_mut(*bb)
                .insts_mut()
                .extend([rhs_ne0, store, jmp]);

            // 处理end分支
            *bb = end_bb;
            let load = ctx.func_data.dfg_mut().new_value().load(res_ptr);
            ctx.func_data
                .layout_mut()
                .bb_mut(*bb)
                .insts_mut()
                .push_key_back(load)
                .expect("failed to add load instruction");
            load
        }
    }
}

/// Generate Koopa IR for a logical OR expression
pub fn generate_lor_exp(ctx: &mut GenContext, bb: &mut BasicBlock, lor_exp: &LOrExp) -> Value {
    match lor_exp {
        LOrExp::LAndExp(land_exp) => generate_land_exp(ctx, bb, land_exp),
        LOrExp::LOrOp { lhs, rhs } => {
            // 处理短路逻辑
            let lhs_val = generate_lor_exp(ctx, bb, lhs);
            // 创建两个基本块
            let false_bb = ctx
                .func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some(format!("%lor_false{}", ctx.lor_counter)));
            let end_bb = ctx
                .func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some(format!("%lor_end{}", ctx.lor_counter)));
            ctx.func_data
                .layout_mut()
                .bbs_mut()
                .extend([false_bb, end_bb]);
            ctx.lor_counter += 1;
            // 创建初始指令
            let res_ptr = ctx.func_data.dfg_mut().new_value().alloc(Type::get_i32());
            let one = ctx.func_data.dfg_mut().new_value().integer(1);
            let init_store = ctx.func_data.dfg_mut().new_value().store(one, res_ptr);
            let br = ctx
                .func_data
                .dfg_mut()
                .new_value()
                .branch(lhs_val, end_bb, false_bb);
            ctx.func_data
                .layout_mut()
                .bb_mut(*bb)
                .insts_mut()
                .extend([res_ptr, init_store, br]);

            // 处理false分支
            *bb = false_bb;
            let rhs_val = generate_land_exp(ctx, bb, rhs);
            let zero = ctx.func_data.dfg_mut().new_value().integer(0);
            let rhs_ne0 =
                ctx.func_data
                    .dfg_mut()
                    .new_value()
                    .binary(BinaryOp::NotEq, rhs_val, zero);
            let store = ctx.func_data.dfg_mut().new_value().store(rhs_ne0, res_ptr);
            let jmp = ctx.func_data.dfg_mut().new_value().jump(end_bb);
            ctx.func_data
                .layout_mut()
                .bb_mut(*bb)
                .insts_mut()
                .extend([rhs_ne0, store, jmp]);

            // 处理end分支
            *bb = end_bb;
            let load = ctx.func_data.dfg_mut().new_value().load(res_ptr);
            ctx.func_data
                .layout_mut()
                .bb_mut(*bb)
                .insts_mut()
                .push_key_back(load)
                .expect("failed to add load instruction");
            load
        }
    }
}
