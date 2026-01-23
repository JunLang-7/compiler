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
    pub num: i32,
}

/// Check if the AST is valid
pub fn check_ast(ast: &CompUnit) {
    if ast.func_def.ident != "main" {
        panic!("The function name must be 'main'");
    }
}
