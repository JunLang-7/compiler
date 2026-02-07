mod context;
mod decl;
mod eval;
mod exp;
mod opt;
mod stmt;
mod util;

use crate::ast::{self, Block, BlockItem, ConstInitVal, Decl, FuncDecl, FuncDef, GlobalItem};
use decl::generate_decl;
use koopa::ir::{BasicBlock, FunctionData, Program};
use koopa::ir::{Type, builder_traits::*};
use stmt::generate_stmt;
use util::*;

pub use context::{GenContext, Symbol};
pub use eval::Eval;
pub use exp::generate_exp;
pub use opt::optimize_program;

/// Generate Koopa IR from the AST
pub fn generate_koopa(ast: &ast::CompUnit) -> Program {
    // create the program and function
    let mut program = Program::new();
    let mut ctx = GenContext::new(&mut program);

    // generate declarations
    let mut new_decl = |name: &str, params_ty, ret_ty| {
        let func = ctx.program.new_func(FunctionData::new_decl(
            format!("@{}", name),
            params_ty,
            ret_ty,
        ));
        ctx.insert_symbol(name.into(), Symbol::Func(func));
    };
    new_decl("getint", vec![], Type::get_i32());
    new_decl("getch", vec![], Type::get_i32());
    new_decl(
        "getarray",
        vec![Type::get_pointer(Type::get_i32())],
        Type::get_i32(),
    );
    new_decl("putint", vec![Type::get_i32()], Type::get_unit());
    new_decl("putch", vec![Type::get_i32()], Type::get_unit());
    new_decl(
        "putarray",
        vec![Type::get_i32(), Type::get_pointer(Type::get_i32())],
        Type::get_unit(),
    );
    new_decl("starttime", vec![], Type::get_unit());
    new_decl("stoptime", vec![], Type::get_unit());

    for item in &ast.items {
        match item {
            GlobalItem::Decl(decl) => {
                generate_global_decl(&mut ctx, decl);
            }
            GlobalItem::FuncDef(func_def) => {
                generate_func_def(&mut ctx, func_def);
            }
            GlobalItem::FuncDecl(func_decl) => {
                generate_func_decl(&mut ctx, func_decl);
            }
        }
    }
    program
}

/// Generate Koopa IR for a global variable declaration
pub fn generate_global_decl(ctx: &mut GenContext, decl: &Decl) {
    match decl {
        Decl::ConstDecl(const_decl) => {
            for def in &const_decl.const_defs {
                let ty = get_array_type(ctx, Type::get_i32(), &def.dims);
                if def.dims.is_empty() {
                    // Scalar
                    if let ConstInitVal::Exp(const_exp) = &def.const_init_val {
                        let val = const_exp.exp.evaluate(ctx);
                        ctx.insert_symbol(def.ident.clone(), Symbol::Const(val));
                    } else {
                        panic!("Scalar constant initialized with list");
                    }
                } else {
                    // Array
                    let init_val = generate_global_const_init_val(ctx, &def.const_init_val, &ty);
                    let alloc = ctx.program.new_value().global_alloc(init_val);
                    ctx.program
                        .set_value_name(alloc, Some(format!("@{}", def.ident)));
                    ctx.insert_symbol(def.ident.clone(), Symbol::Array(alloc));
                }
            }
        }
        Decl::VarDecl(var_decl) => {
            for def in &var_decl.var_defs {
                let ty = get_array_type(ctx, Type::get_i32(), &def.dims);
                let init_val = if let Some(init) = &def.init_val {
                    generate_global_init_val(ctx, init, &ty)
                } else {
                    generate_global_zero_init(ctx, &ty)
                };
                let alloc = ctx.program.new_value().global_alloc(init_val);
                ctx.program
                    .set_value_name(alloc, Some(format!("@{}", def.ident)));

                if def.dims.is_empty() {
                    ctx.insert_symbol(def.ident.clone(), Symbol::Var(alloc));
                } else {
                    ctx.insert_symbol(def.ident.clone(), Symbol::Array(alloc));
                }
            }
        }
    }
}

/// Generate Koopa IR for a function definition
pub fn generate_func_def(ctx: &mut GenContext, func_def: &FuncDef) {
    let parm_ty = if let Some(func_f_parms) = &func_def.func_f_parms {
        func_f_parms
            .func_f_parms
            .iter()
            .map(|parm| {
                let ty = if let Some(dims) = &parm.dims {
                    let mut base_ty = parm.b_type.into_type();
                    for dim in dims.iter().rev() {
                        let size = dim.exp.evaluate(ctx);
                        base_ty = Type::get_array(base_ty, size as usize);
                    }
                    Type::get_pointer(base_ty)
                } else {
                    parm.b_type.into_type()
                };
                (Some(format!("@{}", parm.ident.clone())), ty)
            })
            .collect()
    } else {
        vec![]
    };
    let ret_ty = func_def.func_type.return_type();

    let func = if let Some(symbol) = ctx.lookup_symbol(&func_def.ident) {
        if let Symbol::Func(f) = symbol {
            if !ctx.program.func(f).layout().bbs().is_empty() {
                panic!("Function {} already defined", func_def.ident);
            }
            f
        } else {
            panic!("Symbol {} is defined but not a function", func_def.ident);
        }
    } else {
        let func_data =
            FunctionData::with_param_names(format!("@{}", func_def.ident), parm_ty, ret_ty.clone());
        let func = ctx.program.new_func(func_data);
        ctx.insert_symbol(func_def.ident.clone(), Symbol::Func(func));
        func
    };

    // 设置当前函数与作用域
    ctx.current_func = Some(func);
    ctx.current_ret_ty = Some(ret_ty.clone());

    // Update parameter names
    let parm_val = ctx.func_mut().params().to_owned();
    if let Some(func_f_parms) = &func_def.func_f_parms {
        for (parm, &val) in func_f_parms.func_f_parms.iter().zip(parm_val.iter()) {
            ctx.func_mut()
                .dfg_mut()
                .set_value_name(val, Some(format!("@{}", parm.ident)));
        }
    }

    ctx.enter_scope();
    // 在函数中创建一个基本块(entry basic block)
    let entry = ctx
        .func_mut()
        .dfg_mut()
        .new_bb()
        .basic_block(Some("%entry".into()));
    // 将基本块添加到函数布局中
    ctx.func_mut()
        .layout_mut()
        .bbs_mut()
        .push_key_back(entry)
        .expect("fail to push entry bb");
    // 处理函数参数
    if let Some(func_f_parms) = &func_def.func_f_parms {
        for (parm, value) in func_f_parms.func_f_parms.iter().zip(parm_val) {
            let ty = if let Some(dims) = &parm.dims {
                let mut base_ty = parm.b_type.into_type();
                for dim in dims.iter().rev() {
                    let size = dim.exp.evaluate(ctx);
                    base_ty = Type::get_array(base_ty, size as usize);
                }
                Type::get_pointer(base_ty)
            } else {
                parm.b_type.into_type()
            };

            let alloc = ctx.func_mut().dfg_mut().new_value().alloc(ty);
            let store = ctx.func_mut().dfg_mut().new_value().store(value, alloc);
            ctx.func_mut()
                .layout_mut()
                .bb_mut(entry)
                .insts_mut()
                .extend([alloc, store]);

            // Treat array parameters as variables (Symbol::Var)
            ctx.insert_symbol(parm.ident.clone(), Symbol::Var(alloc));
        }
    }
    // 生成函数体的中间表示
    let last_bb = generate_block(ctx, entry, &func_def.block);

    // 如果最后一个基本块没有终止指令，添加一个 ret
    if !ctx.is_bb_terminated(last_bb) {
        let ret_inst = if ret_ty.is_unit() {
            ctx.func_mut().dfg_mut().new_value().ret(None)
        } else {
            let zero = ctx.func_mut().dfg_mut().new_value().integer(0);
            ctx.func_mut().dfg_mut().new_value().ret(Some(zero))
        };
        ctx.func_mut()
            .layout_mut()
            .bb_mut(last_bb)
            .insts_mut()
            .push_key_back(ret_inst)
            .expect("failed to add return instruction");
    }

    // 退出作用域并重置当前函数
    ctx.exit_scope();
    ctx.current_ret_ty = None;
    ctx.current_func = None;
}

/// Generate Koopa IR for a function declaration
pub fn generate_func_decl(ctx: &mut GenContext, func_decl: &FuncDecl) {
    let parm_ty = if let Some(func_f_parms) = &func_decl.func_f_parms {
        func_f_parms
            .func_f_parms
            .iter()
            .map(|parm| {
                let ty = if let Some(dims) = &parm.dims {
                    let mut base_ty = parm.b_type.into_type();
                    for dim in dims.iter().rev() {
                        let size = dim.exp.evaluate(ctx);
                        base_ty = Type::get_array(base_ty, size as usize);
                    }
                    Type::get_pointer(base_ty)
                } else {
                    parm.b_type.into_type()
                };
                (Some(format!("@{}", parm.ident.clone())), ty)
            })
            .collect()
    } else {
        vec![]
    };
    let ret_ty = func_decl.func_type.return_type();
    let func_data =
        FunctionData::with_param_names(format!("@{}", func_decl.ident), parm_ty, ret_ty.clone());
    let func = ctx.program.new_func(func_data);
    ctx.insert_symbol(func_decl.ident.clone(), Symbol::Func(func));
}

/// Generate Koopa IR for a block and return the latest insertion block
pub fn generate_block(ctx: &mut GenContext, bb: BasicBlock, block: &Block) -> BasicBlock {
    let mut current_bb = bb;
    for block_item in &block.block_items {
        // 如果当前基本块已经终止，停止生成
        if ctx.is_bb_terminated(current_bb) {
            break;
        }
        match block_item {
            BlockItem::Stmt(stmt) => {
                current_bb = generate_stmt(ctx, current_bb, &stmt);
            }
            BlockItem::Decl(decl) => {
                current_bb = generate_decl(ctx, current_bb, &decl);
            }
        }
    }
    current_bb
}
