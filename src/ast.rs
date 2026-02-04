#![allow(dead_code)]

use koopa::ir::Type;

#[derive(Debug, Clone)]
pub struct CompUnit {
    pub items: Vec<GlobalItem>,
}

#[derive(Debug, Clone)]
pub enum GlobalItem {
    Decl(Decl),
    FuncDef(FuncDef),
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub func_f_parms: Option<FuncFParms>,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub enum FuncType {
    Int,
    Void,
}

impl FuncType {
    pub fn return_type(&self) -> Type {
        match self {
            FuncType::Int => Type::get_i32(),
            FuncType::Void => Type::get_unit(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Return(Option<Exp>),
    LValAssign(LValAssign),
    Block(Block),
    Exp(Option<Exp>),
    If(If),
    While(While),
    Break(Break),
    Continue(Continue),
}

#[derive(Debug, Clone)]
pub struct Exp {
    pub lor_exp: LOrExp,
}

impl Exp {
    pub fn number(num: i32) -> Self {
        Self {
            lor_exp: LOrExp::LAndExp(LAndExp::EqExp(EqExp::RelExp(RelExp::AddExp(
                AddExp::MulExp(MulExp::UnaryExp(UnaryExp::PrimaryExp(PrimaryExp::Number(
                    num,
                )))),
            )))),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    UnaryOp { op: UnaryOp, exp: Box<UnaryExp> },
    FuncCall(FuncCall),
}

#[derive(Debug, Clone)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    Number(i32),
    LVal(Box<LVal>),
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

#[derive(Debug, Clone)]
pub enum AddExp {
    MulExp(MulExp),
    AddOp {
        lhs: Box<AddExp>,
        op: AddOp,
        rhs: Box<MulExp>,
    },
}

#[derive(Debug, Clone)]
pub enum MulExp {
    UnaryExp(UnaryExp),
    MulOp {
        lhs: Box<MulExp>,
        op: MulOp,
        rhs: Box<UnaryExp>,
    },
}

#[derive(Debug, Clone)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone)]
pub enum AddOp {
    Plus,
    Minus,
}

#[derive(Debug, Clone)]
pub enum RelExp {
    AddExp(AddExp),
    RelOp {
        lhs: Box<RelExp>,
        op: RelOp,
        rhs: Box<AddExp>,
    },
}

#[derive(Debug, Clone)]
pub enum EqExp {
    RelExp(RelExp),
    EqOp {
        lhs: Box<EqExp>,
        op: EqOp,
        rhs: Box<RelExp>,
    },
}

#[derive(Debug, Clone)]
pub enum LAndExp {
    EqExp(EqExp),
    LAndOp { lhs: Box<LAndExp>, rhs: Box<EqExp> },
}

#[derive(Debug, Clone)]
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrOp { lhs: Box<LOrExp>, rhs: Box<LAndExp> },
}

#[derive(Debug, Clone)]
pub enum RelOp {
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone)]
pub enum EqOp {
    Eq,
    Ne,
}

#[derive(Debug, Clone)]
pub enum LAndOp {
    And,
}

#[derive(Debug, Clone)]
pub enum LOrOp {
    Or,
}

#[derive(Debug, Clone)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub b_type: BType,
    pub const_defs: Vec<ConstDef>,
}

#[derive(Debug, Clone)]
pub enum BType {
    Int,
}

impl BType {
    pub fn into_type(&self) -> Type {
        match self {
            BType::Int => Type::get_i32(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstDef {
    pub ident: String,
    pub dims: Vec<ConstExp>,
    pub const_init_val: ConstInitVal,
}

#[derive(Debug, Clone)]
pub enum ConstInitVal {
    Exp(ConstExp),
    List(Vec<ConstInitVal>),
}

#[derive(Debug, Clone)]
pub struct ConstExp {
    pub exp: Exp,
}

#[derive(Debug, Clone)]
pub struct LVal {
    pub ident: String,
    pub indices: Vec<Exp>,
}

#[derive(Debug, Clone)]
pub struct LValAssign {
    pub lval: LVal,
    pub exp: Exp,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub b_type: BType,
    pub var_defs: Vec<VarDef>,
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub ident: String,
    pub dims: Vec<ConstExp>,
    pub init_val: Option<InitVal>,
}

#[derive(Debug, Clone)]
pub enum InitVal {
    Exp(Exp),
    List(Vec<InitVal>),
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Exp,
    pub then_stmt: Box<Stmt>,
    pub else_stmt: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Exp,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Break;

#[derive(Debug, Clone)]
pub struct Continue;

#[derive(Debug, Clone)]
pub struct FuncFParms {
    pub func_f_parms: Vec<FuncFParm>,
}

#[derive(Debug, Clone)]
pub struct FuncFParm {
    pub b_type: BType,
    pub ident: String,
    pub dims: Option<Vec<ConstExp>>,
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub ident: String,
    pub func_r_parms: Option<FuncRParms>,
}

#[derive(Debug, Clone)]
pub struct FuncRParms {
    pub exps: Vec<Exp>,
}
