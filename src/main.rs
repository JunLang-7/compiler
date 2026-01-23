mod ast;
mod ir;

use crate::ir::{GenerateAsm, generate_koopa};
use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{File, read_to_string};
use std::io::Result;

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);

fn main() -> Result<()> {
    // 解析命令行参数
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    // 读取输入文件
    let input = read_to_string(input)?;

    // 调用 lalrpop 生成的 parser 解析输入文件
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

    // 输出解析得到的 AST
    println!("{:#?}", ast);
    // 简单检查一下 AST 是否符合预期
    if ast.func_def.ident != "main" {
        panic!("The function name must be 'main'");
    }

    // 使用 Koopa 前端将 AST 转换为中间表示
    let program = generate_koopa(&ast);
    
    // 输出结果到文件
    match mode.as_str() {
        "-koopa" => {
            // 将中间表示输出
            KoopaGenerator::new(File::create(output)?).generate_on(&program)?;
        }
        "-riscv" => {
            // 将 Koopa IR 转换为 RISC-V 汇编并输出
            program.generate(&mut File::create(output)?)?;
        }
        _ => panic!("Unknown mode: {}", mode),
    }

    Ok(())
}
