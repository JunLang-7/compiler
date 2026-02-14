# SysY Compiler

一个基于 Rust 实现的 SysY 语言编译器，能够将 SysY 源代码编译为 Koopa IR 和 RISC-V 汇编代码。本项目参考[北大编译实践在线文档](https://pku-minic.github.io/online-doc/)实现。

## 项目简介

这是一个基于 Rust 语言实现的 SysY 语言编译器。该编译器能够将 SysY 源代码（C 语言的一个子集）编译为 Koopa IR（中间表示），并最终生成 RISC-V 32/64 汇编代码。

本项目采用 Rust 2024 Edition 开发，使用了 lalrpop 作为词法分析和语法分析工具，以及 koopa crate 用于处理中间表示。

## 功能特性

本项目目前已完成基础相关功能，主要特性包括：

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

## 进阶特性

依据[文档](https://pku-minic.github.io/online-doc/#/lv9p-reincarnation/)中的方向，进行如下的优化：

### 线性扫描寄存器分配（LSRA）
Linear Scan Register Allocation

采用线性扫描寄存器分配算法，提高寄存器利用率，减少栈溢出：
- **活跃区间分析**：通过活跃变量分析计算每个值的生存期。
- **寄存器选择策略**：
  - 优先使用 callee-saved 寄存器（s0-s11）
  - 支持 caller-saved 寄存器（t3-t6）作为补充
- **溢出处理**：当寄存器不足时自动将变量溢出到栈上。
- **ABI 约束**：遵循 RISC-V 调用约定，对参数和返回值寄存器增加偏好。
- **寄存器合并（Register Coalescing）**：
    - 针对块参数（Phi/Block Args）建立合并提示，在满足活跃区间不重叠时优先分配同一物理寄存器。
    - 避免多参数在同一块入口被错误合并，确保块参数区间在入口处重叠以防止覆盖。
    - 与并行移动结合，能将大量隐式 Move 消除为零成本。

### 死代码消除（DCE）
Dead Code Elimination

基于标记-清除算法的死代码消除优化，在 Koopa IR 层面进行：
- **活跃性分析**：识别对程序输出无影响的指令。
- **副作用检测**：
  - 保留有副作用的操作（如函数调用、全局变量访问）
  - 分析函数调用的副作用传播
  - 消除无副作用且结果未使用的函数调用
- **Store 优化**：消除从未被读取的局部变量存储。
- **递归传播**：通过数据流分析标记所有必要的指令。
- **块参数传播**：当 BlockArg 被标记时，反向标记所有前驱 Jump/Branch 的对应实参，避免丢失必要的值依赖。
- **安全删除**：仅在 `used_by` 为空时移除指令，并设置迭代上限防止死循环。

### 窥孔优化（Peephole Optimization）

在最终 RISC-V 汇编层面进行的局部优化：

**冗余消除**：
- **Load/Store 对消除**：`sw` 后紧跟相同地址的 `lw` 时，直接转换为 `mv` 或删除
- **连续立即数加载消除**：对同一寄存器的连续 `li` 指令只保留最后一条
- **冗余 Move 消除**：删除 `mv rd, rd` 等无效指令以及合并连续`mv`指令
- **零操作消除**：删除 `addi rd, rs, 0`（当 rd == rs）等空操作

**强度削减**（Strength Reduction）：
- **乘法优化**：当乘数为 2 的幂次时，将 `mul` 转换为 `slli`（左移）
- **除法优化**：当除数为 2 的幂次时，将 `div` 转换为 `sra`（算术右移）
- **取模优化**：当取模数为 2 的幂次时，将 `rem` 转换为 `and` (r-1) （位与）

**控制流化简**（Control Flow Simplification）:
- **跳转线程化**：将连续的跳转链（如 A->B->C）合并为直接跳转（A->C），减少分支跳转开销。
- **冗余跳转消除**：当无条件跳转的目标块紧邻当前块之后时，直接移除该跳转指令。
- **不可达代码消除**：基于控制流图的可达性分析，移除从函数入口无法到达的基本块（包括因常量折叠或逻辑优化产生的不可达路径）。

### SSA 格式（Mem2Reg）
Memory to Register

将可提升的局部内存变量转换为 SSA 形式：
- **插入 Phi**：在支配边界放置 Phi，合并不同路径的值。
- **变量重命名**：将同一变量在不同定义点拆分为多个 SSA 值。
- **消除冗余 load/store**：将局部变量访问提升为寄存器形式。
- **后端支持**：汇编生成支持 SSA 的块参数传递与并行移动。

### 后端 SSA 支持
面向 SSA 的块参数传递与并行移动：
- **块参数传递**：为带参数的跳转/分支生成边缘块。
- **并行移动**：处理寄存器与栈槽之间的并行复制与循环。
- **关键边拆分**：创建了额外的 Trampoline 块，在里面执行参数 Move 再跳转到真正的目标块

### 稀疏条件常量传播（SCCP）
Sparse Conditional Constant Propagation

在控制流与数据流联合分析基础上进行常量传播：
- **常量折叠**：支持二元运算常量折叠与同基本块的局部内存常量传播。
- **路径敏感**：仅在可达路径上传播常量。
- **分支化简**：可判定条件时将条件分支简化为绝对分支。
- **联合 DCE**：配合死代码消除进一步清理无效计算。

### 全局值编号（GVN）
Global Value Numbering

通过表达式编号消除等价计算：
- **公共子表达式消除**：在支配范围内复用已计算结果。
- **交换律支持**：对可交换操作进行规范化匹配。
- **与 Phi 协同**：在 SSA 形式下识别等价表达式。

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
    - `-perf`: 将 SysY 源码编译为 RISC-V 汇编 (.S), 用于性能测试.
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
│   │   ├── mod.rs      # IR 模块入口
│   │   ├── stmt.rs     # 语句生成
│   │   ├── exp.rs      # 表达式生成
│   │   ├── decl.rs     # 声明生成
│   │   ├── context.rs  # 符号表与上下文管理
│   │   ├── eval.rs     # 编译期常量求值
│   │   ├── util.rs     # 工具函数
│   │   └── opt/        # 中间代码优化
|   |       ├── const_prop.rs   # 常量传播 / SCCP
│   │       ├── dce.rs          # 死代码消除
│   │       ├── dom.rs          # 支配关系分析
│   │       ├── gvn.rs          # 全局值编号
│   │       ├── mem2reg.rs      # SSA 转换
│   │       ├── mod.rs          # 优化模块入口
│   │       └── side_effect.rs  # 副作用分析
│   └── asm/            # 目标代码生成 (Koopa IR -> RISC-V)
│       ├── mod.rs          # 汇编模块入口
│       ├── builder.rs      # 汇编生成器（含 SSA 块参数/并行移动支持）
│       ├── riscv.rs        # RISC-V 指令与程序结构定义
│       ├── opt/            # 汇编层优化
│       │   └── cfg.rs      # 控制流化简
│       │   └── peephole.rs # 窥孔优化
│       └── reg_alloc/      # 寄存器分配
│           ├── mod.rs      # 寄存器分配模块入口
│           ├── liveness.rs # 活跃性分析
│           └── lsra.rs     # 线性扫描寄存器分配
└── README.md
```

## 参考资料

- [北大编译实践在线文档](https://pku-minic.github.io/online-doc)
- [Rust 语言官方文档](https://www.rust-lang.org/)
- [Koopa IR 规范](https://docs.rs/koopa/latest/koopa/index.html)
