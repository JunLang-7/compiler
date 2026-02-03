use super::Symbol;
use crate::ast::*;
use std::collections::HashMap;

pub trait Eval {
    fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32;
}

impl Eval for Exp {
    fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
        match &self.lor_exp {
            LOrExp::LAndExp(land_exp) => land_exp.evaluate(symbol_table),
            LOrExp::LOrOp { lhs, rhs } => {
                let left = lhs.evaluate(symbol_table);
                if left != 0 {
                    1
                } else {
                    let right = rhs.evaluate(symbol_table);
                    if right != 0 { 1 } else { 0 }
                }
            }
        }
    }
}

impl Eval for UnaryExp {
    fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
        match &self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.evaluate(symbol_table),
            UnaryExp::UnaryOp { op, exp } => {
                let val = exp.evaluate(symbol_table);
                match op {
                    UnaryOp::Plus => val,
                    UnaryOp::Minus => -val,
                    UnaryOp::Not => {
                        if val == 0 {
                            1
                        } else {
                            0
                        }
                    }
                }
            }
            UnaryExp::FuncCall(_) => {
                panic!("Function calls are not supported in constant expressions.")
            }
        }
    }
}

impl Eval for PrimaryExp {
    fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
        match &self {
            PrimaryExp::Exp(exp) => exp.evaluate(symbol_table),
            PrimaryExp::Number(num) => *num,
            PrimaryExp::LVal(lval) => {
                if let Some(val) = symbol_table.get(&lval.ident) {
                    match *val {
                        Symbol::Const(val) => val,
                        Symbol::Var(_) => panic!(
                            "Error: Variable '{}' cannot be used in a constant expression.",
                            lval.ident
                        ),
                        Symbol::Func(_) => panic!(
                            "Error: Function '{}' cannot be used in a constant expression.",
                            lval.ident
                        ),
                        Symbol::Array(_) => panic!(
                            "Error: Array '{}' cannot be used in a constant expression.",
                            lval.ident
                        ),
                    }
                } else {
                    panic!("Undefined variable: {}", lval.ident);
                }
            }
        }
    }
}

impl Eval for AddExp {
    fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
        match &self {
            AddExp::MulExp(mul_exp) => mul_exp.evaluate(symbol_table),
            AddExp::AddOp { lhs, op, rhs } => {
                let left = lhs.evaluate(symbol_table);
                let right = rhs.evaluate(symbol_table);
                match op {
                    AddOp::Plus => left + right,
                    AddOp::Minus => left - right,
                }
            }
        }
    }
}

impl Eval for MulExp {
    fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
        match &self {
            MulExp::UnaryExp(unary_exp) => unary_exp.evaluate(symbol_table),
            MulExp::MulOp { lhs, op, rhs } => {
                let left = lhs.evaluate(symbol_table);
                let right = rhs.evaluate(symbol_table);
                match op {
                    MulOp::Mul => left * right,
                    MulOp::Div => left / right,
                    MulOp::Mod => left % right,
                }
            }
        }
    }
}

impl Eval for RelExp {
    fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
        match &self {
            RelExp::AddExp(add_exp) => add_exp.evaluate(symbol_table),
            RelExp::RelOp { lhs, op, rhs } => {
                let left = lhs.evaluate(symbol_table);
                let right = rhs.evaluate(symbol_table);
                match op {
                    RelOp::Lt => {
                        if left < right {
                            1
                        } else {
                            0
                        }
                    }
                    RelOp::Gt => {
                        if left > right {
                            1
                        } else {
                            0
                        }
                    }
                    RelOp::Le => {
                        if left <= right {
                            1
                        } else {
                            0
                        }
                    }
                    RelOp::Ge => {
                        if left >= right {
                            1
                        } else {
                            0
                        }
                    }
                }
            }
        }
    }
}

impl Eval for EqExp {
    fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
        match &self {
            EqExp::RelExp(rel_exp) => rel_exp.evaluate(symbol_table),
            EqExp::EqOp { lhs, op, rhs } => {
                let left = lhs.evaluate(symbol_table);
                let right = rhs.evaluate(symbol_table);
                match op {
                    EqOp::Eq => {
                        if left == right {
                            1
                        } else {
                            0
                        }
                    }
                    EqOp::Ne => {
                        if left != right {
                            1
                        } else {
                            0
                        }
                    }
                }
            }
        }
    }
}

impl Eval for LAndExp {
    fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
        match &self {
            LAndExp::EqExp(eq_exp) => eq_exp.evaluate(symbol_table),
            LAndExp::LAndOp { lhs, rhs } => {
                let left = lhs.evaluate(symbol_table);
                if left == 0 {
                    0
                } else {
                    let right = rhs.evaluate(symbol_table);
                    if right != 0 { 1 } else { 0 }
                }
            }
        }
    }
}

impl Eval for LOrExp {
    fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
        match &self {
            LOrExp::LAndExp(land_exp) => land_exp.evaluate(symbol_table),
            LOrExp::LOrOp { lhs, rhs } => {
                let left = lhs.evaluate(symbol_table);
                if left != 0 {
                    1
                } else {
                    let right = rhs.evaluate(symbol_table);
                    if right != 0 { 1 } else { 0 }
                }
            }
        }
    }
}
