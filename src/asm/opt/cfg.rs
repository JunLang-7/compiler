use crate::asm::riscv::{Inst, RiscvFunc, RiscvProg};
use std::collections::{HashMap, HashSet};

/// Simplify control flow in the RISC-V program
pub fn simplify_control_flow(prog: &mut RiscvProg) {
    for func in prog.text_sec.iter_mut() {
        let mut changed = true;
        while changed {
            changed = false;
            changed |= thread_jumps(func);
            changed |= remove_redundant_jumps(func);
            changed |= remove_unreachable_blocks(func);
        }
    }
}

/// Jump threading
/// If `Label1` jumps to `Label2`, and `Label2` jumps to `Label3`,
/// then modify `Label1` to jump directly to `Label3`.
fn thread_jumps(func: &mut RiscvFunc) -> bool {
    let mut changed = false;
    let mut trampoline_map = HashMap::new();

    // Recognize trampoline blocks
    for block in &func.blocks {
        // filter lines that aren't instructions or are no-ops
        let meaningful_lines: Vec<&Inst> = block
            .insts
            .iter()
            .filter(|i| !matches!(i, Inst::Li(_, _) if false))
            .collect();
        // If the block contains only a jump instruction, record it
        if meaningful_lines.len() == 1 {
            if let Inst::J(target) = meaningful_lines[0] {
                trampoline_map.insert(block.label.clone(), target.clone());
            }
        }
    }

    if trampoline_map.is_empty() {
        return false;
    }

    // Resolve chain of final targets
    let mut final_targets = HashMap::new();
    for (start_label, _) in &trampoline_map {
        let mut curr = start_label;
        let mut visited = HashSet::new();
        visited.insert(curr);

        while let Some(next) = trampoline_map.get(curr) {
            if visited.contains(next) {
                break; // Detected cycle
            }
            visited.insert(next);
            curr = next;
        }

        if curr != start_label {
            final_targets.insert(start_label.clone(), curr.clone());
        }
    }

    // Update jumps to final targets
    for block in &mut func.blocks {
        for inst in &mut block.insts {
            match inst {
                Inst::J(target) => {
                    if let Some(final_tgt) = final_targets.get(target) {
                        *target = final_tgt.clone();
                        changed = true;
                    }
                }
                Inst::Bnez(_, target) => {
                    if let Some(final_tgt) = final_targets.get(target) {
                        *target = final_tgt.clone();
                        changed = true;
                    }
                }
                _ => {}
            }
        }
    }

    changed
}

/// Remove redundant jumps
/// If a block ends with a jump to the next sequential block, remove that jump
fn remove_redundant_jumps(func: &mut RiscvFunc) -> bool {
    let mut changed = false;
    for i in 0..func.blocks.len() {
        if i + 1 < func.blocks.len() {
            let next_label = func.blocks[i + 1].label.clone();
            let block = &mut func.blocks[i];
            // Check if the last instruction is an unconditional jump to the next block
            if let Some(last_inst) = block.insts.last() {
                if let Inst::J(target) = last_inst {
                    if *target == next_label {
                        block.insts.pop();
                        changed = true;
                    }
                }
            }
        }
    }
    changed
}

/// Remove unreachable blocks
fn remove_unreachable_blocks(func: &mut RiscvFunc) -> bool {
    // build label to index map
    let mut label_to_index = HashMap::new();
    for (i, block) in func.blocks.iter().enumerate() {
        if !block.label.is_empty() {
            label_to_index.insert(block.label.clone(), i);
        }
    }

    let mut reachable = HashSet::new();
    let mut worklist = Vec::new();

    if !func.blocks.is_empty() {
        reachable.insert(0);
        worklist.push(0);
    }

    while let Some(idx) = worklist.pop() {
        let block = &func.blocks[idx];
        let mut successors = Vec::new();

        // Determine the successors
        let mut has_terminal = false;
        for inst in &block.insts {
            match inst {
                Inst::J(target) => {
                    if let Some(&tgt_idx) = label_to_index.get(target) {
                        successors.push(tgt_idx);
                    }
                    has_terminal = true;
                    break;
                }
                Inst::Bnez(_, target) => {
                    if let Some(&tgt_idx) = label_to_index.get(target) {
                        successors.push(tgt_idx);
                    }
                }
                Inst::Ret => {
                    has_terminal = true;
                    break;
                }
                _ => {}
            }
        }

        if !has_terminal {
            if idx + 1 < func.blocks.len() {
                successors.push(idx + 1);
            }
        }

        for succ in successors {
            if !reachable.contains(&succ) {
                reachable.insert(succ);
                worklist.push(succ);
            }
        }
    }

    if reachable.len() == func.blocks.len() {
        return false; // All blocks are reachable
    }

    let old_len = func.blocks.len();
    let mut new_blocks = Vec::new();
    for (i, block) in func.blocks.drain(..).enumerate() {
        if reachable.contains(&i) {
            new_blocks.push(block);
        }
    }
    func.blocks = new_blocks;
    old_len != func.blocks.len()
}
