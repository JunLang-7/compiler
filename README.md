# SysY Compiler

一个基于 Rust 实现的 SysY 语言编译器，能够将 SysY 源代码编译为 Koopa IR 和 RISC-V 汇编代码。本项目参考[北大编译实践在线文档](https://pku-minic.github.io/online-doc/)实现。

## 项目简介

这是一个基于 Rust 语言实现的 SysY 语言编译器。该编译器能够将 SysY 源代码（C 语言的一个子集）编译为 Koopa IR（中间表示），并最终生成 RISC-V 32/64 汇编代码。

本项目采用 Rust 2024 Edition 开发，使用了 lalrpop 作为词法分析和语法分析工具，以及 koopa crate 用于处理中间表示。

## 功能特性

本项目目前已完成 Lv 9.3 及其之前的相关功能，主要特性包括：

- 基础结构: main 函数定义与返回。
- 表达式:
    - 一元运算符 (+, -, !)。
    - 二元算术运算符 (+, -, *, /, %)。
    - 二元逻辑运算符 (&&, ||)，支持短路求值。
    - 关系运算符 (<, >, <=, >=).
- 声明与作用域:
    - 常量声明 (const) 与变量声明 (int)。
    - 代码块 ({}) 与多级作用域嵌套。
    - 全局变量与全局常量声明及初始化。
- 控制流:
    - if-else 条件语句。
    - while 循环语句。
    - break 和 continue 跳转语句。
- 函数:
    - 自定义函数定义与调用。
    - SysY 运行时库函数调用 (getint, getch, putint 等)。
    - 多参数支持（支持超过 8 个参数，处理栈传递）。
- 数组:
    - 一维及多维数组的声明与访问。
    - 复杂的数组初始化列表（支持嵌套 {}、自动补零、展平）。
    - 数组作为函数参数传递（指针退化）。
- 后端优化:
    - 栈帧分析与优化。
    - 大立即数处理（自动转换为 li + add 指令）。

## 构建与运行

### Docker 环境 (推荐)

可以使用预配置好的 Docker 镜像 `maxxing/compiler-dev` 进行开发与构建，其中已包含 Rust 环境与 Koopa/RISC-V 工具链。

```bash
docker pull maxxing/compiler-dev
docker run -it --rm -v "$(pwd)":/root/compiler maxxing/compiler-dev bash
```

### 依赖

- Rust (edition 2024)
- LALRPOP (用于词法/语法分析)

### 构建

```bash
cargo build --release
```

### 运行

编译器接受三个命令行参数：模式、输入文件和输出文件。支持生成 Koopa IR 或 RISC-V 汇编。

#### 命令格式：

```bash
cargo run -- [mode] [input_file] -o [output_file]
```

#### 参数说明：
- mode:
    - `-koopa`: 将 SysY 源码编译为 Koopa IR (.koopa).
    - `-riscv`: 将 SysY 源码编译为 RISC-V 汇编 (.S).
- input_file: 输入的 .c 或 .sy 源文件路径。
- output_file: 输出文件路径。

#### 示例：
```bash
# 生成 Koopa IR
cargo run -- -koopa hello.c -o hello.koopa

# 生成 RISC-V 汇编
cargo run -- -riscv hello.c -o hello.S
```

或者直接使用构建好的二进制文件：

```bash
./target/release/compiler -riscv hello.c -o hello.S
```

## 项目结构

```
.
├── Cargo.toml          # 项目配置
├── build.rs            # 构建脚本 (LALRPOP)
├── src/
│   ├── main.rs         # 入口文件，处理命令行参数与流程控制
│   ├── ast.rs          # 抽象语法树定义
│   ├── sysy.lalrpop    # 词法与语法描述文件
│   ├── ir/             # 中间代码生成 (SysY -> Koopa IR)
│   │   ├── stmt.rs     # 语句生成
│   │   ├── exp.rs      # 表达式生成
│   │   ├── decl.rs     # 声明生成
│   │   ├── context.rs  # 符号表与上下文管理
│   │   └── ...
│   └── asm/            # 目标代码生成 (Koopa IR -> RISC-V)
│       ├── asm_gen.rs  # 汇编生成逻辑
│       └── ...
└── README.md
```

## 参考资料

- [北大编译实践在线文档](https://pku-minic.github.io/online-doc)
- [Rust 语言官方文档](https://www.rust-lang.org/)
- [Koopa IR 规范](https://docs.rs/koopa/latest/koopa/index.html)
