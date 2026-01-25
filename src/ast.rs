#![allow(dead_code)]

use std::collections::HashMap;

use crate::ir::Symbol;

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}
#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

#[derive(Debug)]
pub enum Stmt {
    Return(Exp),
    LValAssign { lval: LVal, exp: Exp },
}

#[derive(Debug)]
pub struct Exp {
    pub lor_exp: LOrExp,
}

impl Exp {
    pub fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
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

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    UnaryOp { op: UnaryOp, exp: Box<UnaryExp> },
}

impl UnaryExp {
    pub fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
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
        }
    }
}

#[derive(Debug)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    Number(i32),
    LVal(Box<LVal>),
}

impl PrimaryExp {
    pub fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
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
                    }
                } else {
                    panic!("Undefined variable: {}", lval.ident);
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

#[derive(Debug)]
pub enum AddExp {
    MulExp(MulExp),
    AddOp {
        lhs: Box<AddExp>,
        op: AddOp,
        rhs: Box<MulExp>,
    },
}

impl AddExp {
    pub fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
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

#[derive(Debug)]
pub enum MulExp {
    UnaryExp(UnaryExp),
    MulOp {
        lhs: Box<MulExp>,
        op: MulOp,
        rhs: Box<UnaryExp>,
    },
}

impl MulExp {
    pub fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
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

#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
pub enum AddOp {
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum RelExp {
    AddExp(AddExp),
    RelOp {
        lhs: Box<RelExp>,
        op: RelOp,
        rhs: Box<AddExp>,
    },
}

impl RelExp {
    pub fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
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

#[derive(Debug)]
pub enum EqExp {
    RelExp(RelExp),
    EqOp {
        lhs: Box<EqExp>,
        op: EqOp,
        rhs: Box<RelExp>,
    },
}

impl EqExp {
    pub fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
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

#[derive(Debug)]
pub enum LAndExp {
    EqExp(EqExp),
    LAndOp { lhs: Box<LAndExp>, rhs: Box<EqExp> },
}

impl LAndExp {
    pub fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
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

#[derive(Debug)]
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrOp { lhs: Box<LOrExp>, rhs: Box<LAndExp> },
}

impl LOrExp {
    pub fn evaluate(&self, symbol_table: &mut HashMap<String, Symbol>) -> i32 {
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

#[derive(Debug)]
pub enum RelOp {
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug)]
pub enum EqOp {
    Eq,
    Ne,
}

#[derive(Debug)]
pub enum LAndOp {
    And,
}

#[derive(Debug)]
pub enum LOrOp {
    Or,
}

#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

#[derive(Debug)]
pub struct ConstDecl {
    pub b_type: BType,
    pub const_defs: Vec<ConstDef>,
}

#[derive(Debug)]
pub enum BType {
    Int,
}

#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub const_init_val: ConstInitVal,
}

#[derive(Debug)]
pub struct ConstInitVal {
    pub const_exp: ConstExp,
}

#[derive(Debug)]
pub struct ConstExp {
    pub exp: Exp,
}

#[derive(Debug)]
pub struct LVal {
    pub ident: String,
}

#[derive(Debug)]
pub struct VarDecl {
    pub b_type: BType,
    pub var_defs: Vec<VarDef>,
}

#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub init_val: Option<InitVal>,
}

#[derive(Debug)]
pub struct InitVal {
    pub exp: Exp,
}

/// Check if the AST is valid
pub fn check_ast(ast: &CompUnit) {
    if ast.func_def.ident != "main" {
        panic!("The function name must be 'main'");
    }
}
