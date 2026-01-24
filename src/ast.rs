#![allow(dead_code)]

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
    pub stmt: Stmt,
}

#[derive(Debug)]
pub struct Stmt {
    pub exp: Exp,
}

#[derive(Debug)]
pub struct Exp {
    pub unary_exp: UnaryExp,
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    UnaryOp { op: UnaryOp, exp: Box<UnaryExp> },
}

#[derive(Debug)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    Number(i32),
}

#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

/// Check if the AST is valid
pub fn check_ast(ast: &CompUnit) {
    if ast.func_def.ident != "main" {
        panic!("The function name must be 'main'");
    }
}
