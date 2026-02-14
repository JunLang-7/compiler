/// Peephole optimization for RISC-V assembly
use crate::asm::riscv::{Inst, Reg, RiscvProg};

/// Perform peephole optimization on a RISC-V function
pub fn peephole_optimize(prog: &mut RiscvProg) {
    for func in prog.text_sec.iter_mut() {
        for block in &mut func.blocks {
            let old_insts = std::mem::replace(&mut block.insts, Vec::new());
            block.insts = optimize_block(old_insts);
        }
    }
}

/// Optimize a sequence of instructions using peephole techniques
fn optimize_block(insts: Vec<Inst>) -> Vec<Inst> {
    let mut new_insts = Vec::new();

    for inst in insts {
        if !try_optimize_window(&mut new_insts, &inst) {
            new_insts.push(inst);
        }
    }
    new_insts
}

/// Try to match last inst and current inst for optimization
/// If matched, modify `history` and return true, no need to push `current`
fn try_optimize_window(history: &mut Vec<Inst>, current: &Inst) -> bool {
    if history.is_empty() {
        return false;
    }

    let last_idx = history.len() - 1;
    let last = &history[last_idx];

    // Pattern 1: Remove redundant load/store
    // Case:
    //  sw src, offset(base)
    //  lw dst, offset(base)
    // Optimization:
    //  if src == dst, delete lw
    //  Otherwise, replace lw with mv dst, src
    if let Inst::Sw(src, base1, off1) = last {
        if let Inst::Lw(dst, base2, off2) = current {
            if base1 == base2 && off1 == off2 {
                if src == dst {
                    return true; // drop current lw
                } else {
                    history.push(Inst::Mv(*dst, *src));
                    return true;
                }
            }
        }
    }

    // Pattern 2: Continuous li instructions elimination
    // Case:
    //  li rd, imm1
    //  li rd, imm2
    // Optimization:
    //  drop the first li
    if let Inst::Li(rd1, _) = last {
        if let Inst::Li(rd2, _) = current {
            if rd1 == rd2 {
                history.pop();
                return false;
            }
        }
    }

    // Pattern 3: Remove redundant moves
    // Case:
    //  mv rd1, rs1
    //  mv rd2, rs2
    // Optimization:
    //  if rs1 == rd2
    //    if rd1 == rs2, drop second mv (first mv A,B is still needed; second mv B,A is no-op)
    //    Otherwise, replace second mv with mv rd2, rs1 (bypass rd1)
    if let Inst::Mv(rd1, rs1) = last {
        if let Inst::Mv(rd2, rs2) = current {
            if rd1 == rs2 {
                let rs1_copy = *rs1;
                let rd2_copy = *rd2;
                if rs1_copy == rd2_copy {
                    // mv A, B; mv B, A => keep first, drop second
                } else {
                    // mv A, B; mv C, A => keep first (A may be needed later),
                    // replace second with mv C, B (bypass A)
                    history.push(Inst::Mv(rd2_copy, rs1_copy));
                }
                return true;
            }
        }
    }

    // Pattern 4: Remove redundant arithmetic with zero
    match current {
        Inst::Addi(rd, rs, 0) if rd == rs => return true,
        Inst::Add(rd, rs, Reg::Zero) if rd == rs => return true,
        Inst::Mv(rd, rs) if rd == rs => return true,
        _ => {}
    }

    // Pattern 5: Strength reduction
    match current {
        Inst::Mul(rd, _, Reg::Zero) => {
            history.push(Inst::Li(*rd, 0));
            return true;
        }
        Inst::Mul(rd, rs, reg) => {
            // Check if last instruction is li reg, imm where imm is power of 2
            if let Inst::Li(li_rd, imm) = last {
                if li_rd == reg && *imm > 0 && (*imm & (*imm - 1)) == 0 {
                    // imm is a power of 2, calculate log2 and use shift
                    let shift = imm.trailing_zeros() as i32;
                    history.pop(); // remove li
                    history.push(Inst::Slli(*rd, *rs, shift));
                    return true;
                }
            }
        }
        Inst::Div(rd, rs, reg) => {
            // Check if last instruction is li reg, imm where imm is power of 2
            if let Inst::Li(li_rd, imm) = last {
                if li_rd == reg && *imm > 0 && (*imm & (*imm - 1)) == 0 {
                    // imm is a power of 2, use arithmetic shift right
                    let shift = imm.trailing_zeros() as i32;
                    history.pop(); // remove original li
                    history.push(Inst::Li(*reg, shift)); // load shift amount
                    history.push(Inst::Sra(*rd, *rs, *reg)); // use sra for division
                    return true;
                }
            }
        }
        Inst::Rem(rd, rs, reg) => {
            // Check if last instruction is li reg, imm where imm is power of 2
            if let Inst::Li(li_rd, imm) = last {
                if li_rd == reg && *imm > 0 && (*imm & (*imm - 1)) == 0 {
                    // imm is a power of 2, use bitwise and with imm-1
                    let mask = *imm - 1;
                    history.pop(); // remove original li
                    history.push(Inst::Li(*reg, mask)); // load mask
                    history.push(Inst::And(*rd, *rs, *reg)); // use and for remainder
                    return true;
                }
            }
        }
        _ => {}
    }

    false
}
