/// Loop Invariant Code Motion (LICM) optimization pass for Koopa IR.
use super::dom::DomInfo;
use super::loop_analysis::{LoopAnalysis, LoopInfo};
use koopa::ir::{BasicBlock, FunctionData, Value, ValueKind};
use std::collections::HashSet;

/// LoopInvariantCodeMotion performs the LICM optimization on a function's control flow graph,
/// using loop analysis to identify natural loops and move invariant instructions to preheaders.
pub struct LoopInvariantCodeMotion<'a> {
    func: &'a mut FunctionData,
    dom: DomInfo,
}

impl<'a> LoopInvariantCodeMotion<'a> {
    pub fn new(func: &'a mut FunctionData) -> Self {
        let dom = DomInfo::new(func);
        Self { func, dom }
    }

    pub fn run(&mut self) -> bool {
        let mut changed = false;

        let analyzer = LoopAnalysis::new(self.func, &self.dom);
        let loops = analyzer.loops.clone();

        for loop_info in loops {
            if self.run_on_loop(&loop_info) {
                changed = true;
            }
        }

        changed
    }

    fn run_on_loop(&mut self, loop_info: &LoopInfo) -> bool {
        let preheader = match loop_info.preheader {
            Some(bb) => bb,
            None => return false, // Can't move code if there's no preheader
        };

        let mut changed = false;
        let mut invariants = HashSet::new();
        let mut to_hoist = Vec::new();

        let mut progress = true;
        while progress {
            progress = false;

            for &bb in &loop_info.blocks {
                let insts: Vec<Value> = self
                    .func
                    .layout()
                    .bbs()
                    .node(&bb)
                    .unwrap()
                    .insts()
                    .keys()
                    .cloned()
                    .collect();
                for inst in insts {
                    if invariants.contains(&inst) {
                        continue;
                    }

                    if self.is_safe_to_hoist(inst)
                        && self.are_operands_invariant(inst, loop_info, &invariants)
                    {
                        invariants.insert(inst);
                        to_hoist.push((inst, bb));
                        progress = true;
                        changed = true;
                    }
                }
            }
        }

        // Hoist the identified invariant instructions to the preheader
        if !to_hoist.is_empty() {
            let term = *self
                .func
                .layout()
                .bbs()
                .node(&preheader)
                .unwrap()
                .insts()
                .back_key()
                .unwrap();
            for (inst, old_bb) in to_hoist {
                self.func
                    .layout_mut()
                    .bb_mut(old_bb)
                    .insts_mut()
                    .remove(&inst);
                let mut cursor = self
                    .func
                    .layout_mut()
                    .bb_mut(preheader)
                    .insts_mut()
                    .cursor_mut(term);
                cursor
                    .insert_key_before(inst)
                    .expect("failed to insert key before terminator");
            }
        }

        changed
    }

    /// Check if an instruction is safe to hoist (e.g., no side effects, not a terminator)
    fn is_safe_to_hoist(&self, inst: Value) -> bool {
        let kind = self.func.dfg().value(inst).kind().clone();
        match kind {
            ValueKind::Binary(_) | ValueKind::GetElemPtr(_) | ValueKind::GetPtr(_) => true, // Pure computations
            _ => false, // Conservatively assume other instructions may have side effects
        }
    }

    /// Check if all operands of the instruction are loop-invariant
    fn are_operands_invariant(
        &self,
        inst: Value,
        loop_info: &LoopInfo,
        invariants: &HashSet<Value>,
    ) -> bool {
        let value_data = self.func.dfg().value(inst);
        let opreands = match value_data.kind() {
            ValueKind::Binary(bin) => vec![bin.lhs(), bin.rhs()],
            ValueKind::GetElemPtr(get) => vec![get.src(), get.index()],
            ValueKind::GetPtr(gp) => vec![gp.src(), gp.index()],
            _ => vec![],
        };
        for op in opreands {
            if !self.is_operand_invariant(op, loop_info, invariants) {
                return false;
            }
        }
        true
    }

    fn is_operand_invariant(
        &self,
        op: Value,
        loop_info: &LoopInfo,
        invariants: &HashSet<Value>,
    ) -> bool {
        if !self.func.dfg().values().contains_key(&op) {
            // Non-local values (e.g., globals) are not in the function DFG; be conservative.
            return false;
        }
        if invariants.contains(&op) {
            return true; // Already identified as invariant
        }
        let op_data = self.func.dfg().value(op);
        match op_data.kind() {
            ValueKind::Integer(_) | ValueKind::Undef(_) => true,
            ValueKind::GlobalAlloc(_) => true,
            ValueKind::FuncArgRef(_) => true,
            _ => {
                let def_bb = self.get_inst_parent(op);
                if let Some(bb) = def_bb {
                    !loop_info.blocks.contains(&bb) // Defined outside the loop
                } else {
                    let arg_bb = self.get_block_arg_parent(op);
                    if let Some(bb) = arg_bb {
                        !loop_info.blocks.contains(&bb) // Defined outside the loop
                    } else {
                        false // No parent block found
                    }
                }
            }
        }
    }

    /// Get the parent block of an instruction, if it is defined in some basic block
    fn get_inst_parent(&self, inst: Value) -> Option<BasicBlock> {
        for (&bb, node) in self.func.layout().bbs() {
            if node.insts().keys().any(|&i| i == inst) {
                return Some(bb);
            }
        }
        None
    }

    /// Get the parent block of a block argument, if it is a parameter of some basic block
    fn get_block_arg_parent(&self, arg: Value) -> Option<BasicBlock> {
        for (&bb, _) in self.func.layout().bbs() {
            if self.func.dfg().bb(bb).params().iter().any(|&p| p == arg) {
                return Some(bb);
            }
        }
        None
    }
}
