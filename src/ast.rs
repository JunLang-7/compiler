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
    pub lor_exp: LOrExp,
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

#[derive(Debug)]
pub enum AddExp {
    MulExp(MulExp),
    AddOp {
        lhs: Box<AddExp>,
        op: AddOp,
        rhs: Box<MulExp>,
    },
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

#[derive(Debug)]
pub enum EqExp {
    RelExp(RelExp),
    EqOp {
        lhs: Box<EqExp>,
        op: EqOp,
        rhs: Box<RelExp>,
    },
}

#[derive(Debug)]
pub enum LAndExp {
    EqExp(EqExp),
    LAndOp { lhs: Box<LAndExp>, rhs: Box<EqExp> },
}

#[derive(Debug)]
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrOp { lhs: Box<LOrExp>, rhs: Box<LAndExp> },
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

/// Check if the AST is valid
pub fn check_ast(ast: &CompUnit) {
    if ast.func_def.ident != "main" {
        panic!("The function name must be 'main'");
    }
}
