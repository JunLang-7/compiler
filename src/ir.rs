use crate::ast::{self, Exp, UnaryExp, UnaryOp};
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, BinaryOp, FunctionData, Program, Type, Value};

/// Generate Koopa IR from the AST
pub fn generate_koopa(ast: &ast::CompUnit) -> Program {
    // create the program and function
    let mut program = Program::new();
    let func_data = FunctionData::new(format!("@{}", ast.func_def.ident), vec![], Type::get_i32());
    let func = program.new_func(func_data);
    let func_data = program.func_mut(func);

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

    // 在基本块中添加指令
    let ret_val = generate_exp(func_data, entry, &ast.func_def.block.stmt.exp);
    // 在基本块中添加返回语句
    let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
    // 将返回语句添加到基本块中
    func_data
        .layout_mut()
        .bb_mut(entry)
        .insts_mut()
        .push_key_back(ret)
        .expect("fail to push return value");
    program
}

/// Generate Koopa IR for an expression
pub fn generate_exp(func_data: &mut FunctionData, bb: BasicBlock, exp: &Exp) -> Value {
    generate_unary_exp(func_data, bb, &exp.unary_exp)
}

/// Generate Koopa IR for a unary expression
pub fn generate_unary_exp(
    func_data: &mut FunctionData,
    bb: BasicBlock,
    unary_exp: &ast::UnaryExp,
) -> Value {
    match unary_exp {
        UnaryExp::PrimaryExp(primary_exp) => generate_primary_exp(func_data, bb, primary_exp),
        UnaryExp::UnaryOp { op, exp } => {
            let val = generate_unary_exp(func_data, bb, exp);
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
) -> Value {
    match primary_exp {
        ast::PrimaryExp::Exp(exp) => generate_exp(func_data, bb, exp),
        ast::PrimaryExp::Number(num) => func_data.dfg_mut().new_value().integer(*num),
    }
}
