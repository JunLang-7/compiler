use koopa::ir::{BasicBlock, FunctionData, Value, ValueKind};
use std::collections::{HashSet, VecDeque};

/// Remove unreachable basic blocks from the function.
pub fn remove_unreachable_blocks(func: &mut FunctionData) -> bool {
    let Some(entry) = func.layout().entry_bb() else {
        return false;
    };

    let mut reachable = HashSet::new();
    let mut queue = VecDeque::new();
    reachable.insert(entry);
    queue.push_back(entry);

    while let Some(bb) = queue.pop_front() {
        let Some(term) = func
            .layout()
            .bbs()
            .node(&bb)
            .and_then(|node| node.insts().back_key().copied())
        else {
            continue;
        };

        // Check terminator instruction to find successor blocks
        match func.dfg().value(term).kind() {
            ValueKind::Jump(jmp) => {
                let to = jmp.target();
                if reachable.insert(to) {
                    queue.push_back(to);
                }
            }
            ValueKind::Branch(br) => {
                let t = br.true_bb();
                let f = br.false_bb();
                if reachable.insert(t) {
                    queue.push_back(t);
                }
                if reachable.insert(f) {
                    queue.push_back(f);
                }
            }
            _ => {}
        }
    }

    let all_bbs: Vec<BasicBlock> = func.layout().bbs().keys().copied().collect();
    let unreachable_bbs: Vec<BasicBlock> = all_bbs
        .into_iter()
        .filter(|bb| !reachable.contains(bb))
        .collect();

    if unreachable_bbs.is_empty() {
        return false;
    }

    for &bb in &unreachable_bbs {
        let Some(node) = func.layout().bbs().node(&bb) else {
            continue;
        };
        let mut dead_insts: HashSet<Value> = node.insts().keys().copied().collect();

        // Iteratively remove dead instructions until no more can be removed
        while !dead_insts.is_empty() {
            let mut progress = false;
            let snapshot: Vec<Value> = dead_insts.iter().copied().collect();
            for inst in snapshot {
                if !func.dfg().values().contains_key(&inst) {
                    dead_insts.remove(&inst);
                    progress = true;
                    continue;
                }
                // If the instruction is still used by other instructions, it cannot be removed yet
                if !func.dfg().value(inst).used_by().is_empty() {
                    continue;
                }

                func.layout_mut().bb_mut(bb).insts_mut().remove(&inst);
                func.dfg_mut().remove_value(inst);
                dead_insts.remove(&inst);
                progress = true;
            }

            if !progress {
                break;
            }
        }

        // Finally, remove any remaining instructions in the unreachable block
        if let Some(node) = func.layout().bbs().node(&bb) {
            let remain: Vec<Value> = node.insts().keys().copied().collect();
            for inst in remain {
                if func.dfg().values().contains_key(&inst) {
                    func.layout_mut().bb_mut(bb).insts_mut().remove(&inst);
                    let _ = func.dfg_mut().remove_value(inst);
                }
            }
        }
    }

    // Remove the unreachable blocks themselves
    for &bb in &unreachable_bbs {
        func.layout_mut().bbs_mut().remove(&bb);
    }

    for &bb in &unreachable_bbs {
        if func.dfg().bbs().contains_key(&bb) {
            func.dfg_mut().remove_bb(bb);
        }
    }

    true
}
