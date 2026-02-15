/// Induction Variable Optimization (IVO) pass for loops in the intermediate representation (IR).
/// This pass identifies basic induction variables (BIVs) in loops and applies strength reduction to replace expensive operations with cheaper ones, and eliminates induction variables when possible.
use super::dom::DomInfo;
use super::loop_analysis::{LoopAnalysis, LoopInfo};
use koopa::ir::{
    BasicBlock, BinaryOp, FunctionData, Type, Value, ValueKind,
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
};
use std::collections::{HashMap, HashSet};

/// Basic Induction Variable (BIV) for strength reduction
struct BIV {
    param_idx: usize,
    param_val: Value,
    init_val: Value,
    step: i32,
}

/// Derived Induction Variable instruction that can be strength reduced
struct DIV {
    inst: Value,
    biv_idx: usize,
    multiplier: i32,
}

/// InductionVariableOptimizer performs induction variable optimization on loops,
/// identifying basic induction variables and replacing expensive operations with cheaper ones and eliminating induction variables when possible.
pub struct InductionVariableOptimizer<'a> {
    func: &'a mut FunctionData,
    dom: DomInfo,
}

impl<'a> InductionVariableOptimizer<'a> {
    /// Create a new InductionVariableOptimizer for the given function and its dominator information
    pub fn new(func: &'a mut FunctionData) -> Self {
        let dom = DomInfo::new(func);
        Self { func, dom }
    }

    /// Run the strength reduction optimization on the function, returning true if any changes were made
    pub fn run(&mut self) -> bool {
        let mut changed = false;

        let analyzer = LoopAnalysis::new(&self.func, &self.dom);
        let loops = analyzer.loops.clone();

        for loop_info in loops {
            if self.run_on_loop(&loop_info) {
                changed = true;
            }
        }

        changed
    }

    /// Run strength reduction on a single loop, returning true if any changes were made
    pub fn run_on_loop(&mut self, loop_info: &LoopInfo) -> bool {
        let preheader = match loop_info.preheader {
            Some(bb) => bb,
            None => return false, // Can't perform strength reduction if there's no preheader
        };
        if loop_info.latches.len() != 1 {
            return false; // Only handle single-latch loops for simplicity
        }

        let latch = *loop_info.latches.iter().next().unwrap();
        let header = loop_info.header;

        // Step 1: Identify basic induction variables (BIVs) in the loop
        let bivs = self.find_bivs(header, preheader, latch);
        if bivs.is_empty() {
            return false; // No BIVs found, can't perform strength reduction
        }

        let mut biv_map: HashMap<Value, &BIV> = HashMap::new();
        for biv in &bivs {
            biv_map.insert(biv.param_val, biv);
        }

        // Step 2: Find DIV instructions that can be strength reduced using the identified BIVs
        let mut divs = Vec::new();
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
                if let Some(div) = self.analyze_div(inst, &biv_map) {
                    divs.push(div);
                }
            }
        }
        if divs.is_empty() {
            return false; // No DIVs found that can be optimized
        }

        let mut changed = false;
        let mut test_replaced_for_bivs = HashSet::new();

        // Step 3: Perform strength reduction on the identified DIV instructions
        for div in divs {
            let biv = bivs
                .iter()
                .find(|biv| biv.param_idx == div.biv_idx)
                .unwrap();

            // Calculate the new step size for the derived induction variable
            let k = div.multiplier;
            let c = biv.step;
            let step_size = c.wrapping_mul(k);

            if step_size == 0 {
                continue; // LICM should handle it if the step size is zero
            }

            // Create a block argument in the header
            let ty = self.func.dfg().value(div.inst).ty().clone();
            let new_param = self.get_new_param(header, ty);

            // Modify the preheader: div_init = biv.init_val * k
            let div_init = self.insert_math_inst(preheader, biv.init_val, k, BinaryOp::Mul);
            self.append_jump_arg(preheader, header, div_init);
            // Modify the latch: div_next = new_param + C * k
            let div_next = self.insert_math_inst(latch, new_param, step_size, BinaryOp::Add);
            self.append_jump_arg(latch, header, div_next);
            // Replace the original DIV with the new parameter
            // Substitute all uses of div.inst with new_param
            let users: Vec<Value> = self
                .func
                .dfg()
                .value(div.inst)
                .used_by()
                .iter()
                .cloned()
                .collect();
            for user in users {
                let mut data = self.func.dfg().value(user).clone();
                Self::subst_value_in_kind(data.kind_mut(), div.inst, new_param);
                self.func.dfg_mut().replace_value_with(user).raw(data);
            }

            // Step 4: Try to replace the loop condition test if it involves the BIV with a test on the new parameter, which is the Induction Variable Elimination (IVE)
            if !test_replaced_for_bivs.contains(&biv.param_idx) {
                let replaced =
                    self.replace_linear_test(header, preheader, biv, k, new_param, loop_info);
                if replaced {
                    test_replaced_for_bivs.insert(biv.param_idx);
                    self.try_eliminate_biv(header, latch, biv);
                }
            }

            changed = true;
        }

        changed
    }

    /// Get or create a new block argument in the loop header for the strength reduced variable
    fn get_new_param(&mut self, header: BasicBlock, ty: Type) -> Value {
        let temp_bb = self
            .func
            .dfg_mut()
            .new_bb()
            .basic_block_with_params(None, vec![ty]);
        let block_arg = self.func.dfg().bb(temp_bb).params()[0];

        // 修正 BlockArgRef 的 index 为目标 BB 中的正确位置
        let correct_index = self.func.dfg().bb(header).params().len();
        let mut data = self.func.dfg().value(block_arg).clone();
        if let ValueKind::BlockArgRef(arg) = data.kind_mut() {
            *arg.index_mut() = correct_index;
        }
        self.func.dfg_mut().replace_value_with(block_arg).raw(data);

        // 将参数转移到目标 BB
        self.func
            .dfg_mut()
            .bb_mut(header)
            .params_mut()
            .push(block_arg);

        // 清理临时 BB
        self.func.dfg_mut().bb_mut(temp_bb).params_mut().clear();
        self.func.dfg_mut().remove_bb(temp_bb);
        block_arg
    }

    /// Find basic induction variables (BIVs) in the loop by analyzing the loop header's parameters and their updates in the latch
    fn find_bivs(&self, header: BasicBlock, preheader: BasicBlock, latch: BasicBlock) -> Vec<BIV> {
        let mut bivs = Vec::new();
        let params = self.func.dfg().bb(header).params().to_vec();

        for (idx, &param) in params.iter().enumerate() {
            let init_val = self.get_jump_arg(preheader, header, idx);
            let next_val = self.get_jump_arg(latch, header, idx);

            if let Some(step) = self.analyze_step(next_val, param) {
                bivs.push(BIV {
                    param_idx: idx,
                    param_val: param,
                    init_val,
                    step,
                });
            }
        }
        bivs
    }

    /// Analyze an instruction to determine if it is a candidate for strength reduction based on the identified BIVs
    fn analyze_div(&self, inst: Value, biv_map: &HashMap<Value, &BIV>) -> Option<DIV> {
        let inst_data = self.func.dfg().value(inst);
        if let ValueKind::Binary(bin) = inst_data.kind() {
            if bin.op() == BinaryOp::Mul {
                let lhs = bin.lhs();
                let rhs = bin.rhs();
                if let Some(biv) = biv_map.get(&lhs) {
                    if let Some(multiplier) = self.get_i32_const(rhs) {
                        return Some(DIV {
                            inst,
                            biv_idx: biv.param_idx,
                            multiplier,
                        });
                    }
                }
                if let Some(biv) = biv_map.get(&rhs) {
                    if let Some(multiplier) = self.get_i32_const(lhs) {
                        return Some(DIV {
                            inst,
                            biv_idx: biv.param_idx,
                            multiplier,
                        });
                    }
                }
            } else if bin.op() == BinaryOp::Shl {
                let lhs = bin.lhs();
                let rhs = bin.rhs();
                if let Some(biv) = biv_map.get(&lhs) {
                    if let Some(shift_amount) = self.get_i32_const(rhs) {
                        let multiplier = 1 << shift_amount;
                        return Some(DIV {
                            inst,
                            biv_idx: biv.param_idx,
                            multiplier,
                        });
                    }
                }
            } else if bin.op() == BinaryOp::Div {
                let lhs = bin.lhs();
                let rhs = bin.rhs();
                if let Some(biv) = biv_map.get(&lhs) {
                    if let Some(divisor) = self.get_i32_const(rhs) {
                        if divisor != 0 && divisor & (divisor - 1) == 0 {
                            // For division, the multiplier is the reciprocal of the divisor
                            let multiplier = 1 / divisor;
                            return Some(DIV {
                                inst,
                                biv_idx: biv.param_idx,
                                multiplier,
                            });
                        }
                    }
                }
            }
        }
        None
    }

    /// Try linear test replacement for the loop condition if it involves the BIV,
    /// replacing it with a test on the new parameter after strength reduction.
    /// It's the Induction Variable Elimination (IVE).
    fn replace_linear_test(
        &mut self,
        header: BasicBlock,
        preheader: BasicBlock,
        biv: &BIV,
        multiplier: i32,
        new_param: Value,
        loop_info: &LoopInfo,
    ) -> bool {
        if multiplier <= 0 {
            return false; // Only handle positive multipliers for now
        }

        let term = *self
            .func
            .layout()
            .bbs()
            .node(&header)
            .unwrap()
            .insts()
            .back_key()
            .expect("Loop header must end with a terminator");
        let term_data = self.func.dfg().value(term).clone();

        if let ValueKind::Branch(br) = term_data.kind() {
            let cond = br.cond();
            let cond_data = self.func.dfg().value(cond).clone();

            if let ValueKind::Binary(bin) = cond_data.kind() {
                let (lhs, rhs) = (bin.lhs(), bin.rhs());
                // Check if the condition involves the BIV
                let (is_lhs_biv, limit_val) = if lhs == biv.param_val {
                    (true, rhs)
                } else if rhs == biv.param_val {
                    (false, lhs)
                } else {
                    return false; // Condition does not involve the BIV
                };

                // Check if the limit value is loop-invariant
                if !self.is_loop_invariant(limit_val, loop_info) {
                    return false; // Limit value must be loop-invariant for the transformation to be valid
                }

                // Start Replacing
                // new_limit = limit_val * multiplier
                let new_limit =
                    self.insert_math_inst(preheader, limit_val, multiplier, BinaryOp::Mul);

                let new_lhs = if is_lhs_biv { new_param } else { new_limit };
                let new_rhs = if is_lhs_biv { new_limit } else { new_param };
                // Replace the condition with the new test
                self.func
                    .dfg_mut()
                    .replace_value_with(cond)
                    .binary(bin.op(), new_lhs, new_rhs);

                return true;
            }
        }
        false
    }

    /// Try to remove an unused BIV parameter and its corresponding jump args.
    fn try_eliminate_biv(&mut self, header: BasicBlock, latch: BasicBlock, biv: &BIV) -> bool {
        let next_val = self.get_jump_arg(latch, header, biv.param_idx);
        let users: Vec<Value> = self
            .func
            .dfg()
            .value(biv.param_val)
            .used_by()
            .iter()
            .cloned()
            .collect();
        for user in users {
            if user == next_val {
                continue;
            }
            let user_data = self.func.dfg().value(user);
            let dead_user = user_data.used_by().is_empty()
                && matches!(
                    user_data.kind(),
                    ValueKind::Binary(_) | ValueKind::GetPtr(_) | ValueKind::GetElemPtr(_)
                );
            if !dead_user {
                return false;
            }
        }

        if !self.remove_param_from_header(header, biv.param_idx) {
            return false;
        }
        self.remove_arg_from_all_preds(header, biv.param_idx);
        true
    }

    /// Remove the parameter at the specified index from the loop header and update all related BlockArgRef indices accordingly.
    fn remove_param_from_header(&mut self, header: BasicBlock, idx: usize) -> bool {
        let mut params = self.func.dfg().bb(header).params().to_vec();
        if idx >= params.len() {
            return false;
        }
        params.remove(idx);
        let params_mut = self.func.dfg_mut().bb_mut(header).params_mut();
        params_mut.clear();
        params_mut.extend(params.iter().cloned());

        for (new_idx, &param) in params.iter().enumerate() {
            let mut data = self.func.dfg().value(param).clone();
            if let ValueKind::BlockArgRef(arg) = data.kind_mut() {
                *arg.index_mut() = new_idx;
            }
            self.func.dfg_mut().replace_value_with(param).raw(data);
        }
        true
    }

    /// Remove the argument at the specified index from all predecessor blocks' terminators that jump to the target block, and update the terminators accordingly.
    fn remove_arg_from_all_preds(&mut self, target: BasicBlock, idx: usize) {
        let bbs: Vec<BasicBlock> = self.func.layout().bbs().keys().cloned().collect();
        for bb in bbs {
            self.remove_arg_from_terminator(bb, target, idx);
        }
    }

    /// Remove the argument at the specified index from the terminator instruction in the `from` block that jumps to the `to` block, and update the terminator accordingly.
    fn remove_arg_from_terminator(&mut self, from: BasicBlock, to: BasicBlock, idx: usize) {
        let term = self
            .func
            .layout()
            .bbs()
            .node(&from)
            .and_then(|node| node.insts().back_key())
            .cloned();
        let Some(term) = term else {
            return;
        };
        let term_data = self.func.dfg().value(term).clone();

        match term_data.kind() {
            ValueKind::Jump(jmp) => {
                if jmp.target() != to {
                    return;
                }
                let mut args = jmp.args().to_vec();
                if idx >= args.len() {
                    return;
                }
                args.remove(idx);
                self.func
                    .dfg_mut()
                    .replace_value_with(term)
                    .jump_with_args(to, args);
            }
            ValueKind::Branch(br) => {
                let cond = br.cond();
                let true_bb = br.true_bb();
                let false_bb = br.false_bb();
                let mut t_args = br.true_args().to_vec();
                let mut f_args = br.false_args().to_vec();
                let mut changed = false;
                if true_bb == to && idx < t_args.len() {
                    t_args.remove(idx);
                    changed = true;
                }
                if false_bb == to && idx < f_args.len() {
                    f_args.remove(idx);
                    changed = true;
                }
                if !changed {
                    return;
                }
                if true_bb == false_bb {
                    self.func
                        .dfg_mut()
                        .replace_value_with(term)
                        .jump_with_args(true_bb, t_args);
                } else {
                    self.func
                        .dfg_mut()
                        .replace_value_with(term)
                        .branch_with_args(cond, true_bb, false_bb, t_args, f_args);
                }
            }
            _ => {}
        }
    }

    /// Analyze the next value of a potential BIV to determine if it follows the induction variable pattern (e.g., param + step)
    fn analyze_step(&self, next_val: Value, param: Value) -> Option<i32> {
        let val_data = self.func.dfg().value(next_val);
        if let ValueKind::Binary(bin) = val_data.kind() {
            if bin.op() == BinaryOp::Add {
                if bin.lhs() == param {
                    return self.get_i32_const(bin.rhs());
                }
                if bin.rhs() == param {
                    return self.get_i32_const(bin.lhs());
                }
            } else if bin.op() == BinaryOp::Sub {
                if bin.lhs() == param {
                    if let Some(c) = self.get_i32_const(bin.rhs()) {
                        return Some(-c);
                    }
                }
                if bin.rhs() == param {
                    return self.get_i32_const(bin.lhs());
                }
            }
        }
        None
    }

    /// Helper function to extract an i32 constant value from a Value, if it is an integer constant
    fn get_i32_const(&self, val: Value) -> Option<i32> {
        if let ValueKind::Integer(i) = self.func.dfg().value(val).kind() {
            Some(i.value())
        } else {
            None
        }
    }

    /// Get the argument value passed from `from` block to `to` block at index `arg_idx`
    fn get_jump_arg(&self, from: BasicBlock, to: BasicBlock, arg_idx: usize) -> Value {
        let term = self
            .func
            .layout()
            .bbs()
            .node(&from)
            .unwrap()
            .insts()
            .back_key()
            .expect("Basic block must end with terminator");
        let term_data = self.func.dfg().value(*term);
        match term_data.kind() {
            ValueKind::Jump(jmp) => jmp.args()[arg_idx],
            ValueKind::Branch(br) => {
                if br.true_bb() == to {
                    br.true_args()[arg_idx]
                } else {
                    br.false_args()[arg_idx]
                }
            }
            _ => unreachable!(),
        }
    }

    /// Append a new argument to the jump or branch instruction from `from` block to `to` block, used for passing the new induction variable value
    fn append_jump_arg(&mut self, from: BasicBlock, to: BasicBlock, new_arg: Value) {
        let term = self
            .func
            .layout()
            .bbs()
            .node(&from)
            .unwrap()
            .insts()
            .back_key()
            .expect("Basic block must end with terminator");
        let term = *term;
        let term_data = self.func.dfg().value(term).clone();
        match term_data.kind() {
            ValueKind::Jump(jmp) => {
                let target = jmp.target();
                let mut new_args = jmp.args().to_vec();
                new_args.push(new_arg);
                self.func
                    .dfg_mut()
                    .replace_value_with(term)
                    .jump_with_args(target, new_args);
            }
            ValueKind::Branch(br) => {
                let cond = br.cond();
                let true_bb = br.true_bb();
                let false_bb = br.false_bb();
                let mut t_args = br.true_args().to_vec();
                let mut f_args = br.false_args().to_vec();
                if true_bb == to {
                    t_args.push(new_arg);
                }
                if false_bb == to {
                    f_args.push(new_arg);
                }
                self.func
                    .dfg_mut()
                    .replace_value_with(term)
                    .branch_with_args(cond, true_bb, false_bb, t_args, f_args);
            }
            _ => unreachable!(),
        }
    }

    /// Insert a new math instruction (e.g., add, mul) in the specified basic block, using the given base value and constant value, and return the result value
    fn insert_math_inst(
        &mut self,
        bb: BasicBlock,
        base_val: Value,
        const_val: i32,
        op: BinaryOp,
    ) -> Value {
        let c_val = self.func.dfg_mut().new_value().integer(const_val);
        let math_inst = self.func.dfg_mut().new_value().binary(op, base_val, c_val);

        let term = *self
            .func
            .layout()
            .bbs()
            .node(&bb)
            .unwrap()
            .insts()
            .back_key()
            .expect("Basic block must end with terminator");
        let mut cursor = self
            .func
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .cursor_mut(term);
        cursor
            .insert_key_before(math_inst)
            .expect("failed to insert math instruction");
        math_inst
    }

    /// Check if a value is loop-invariant
    fn is_loop_invariant(&self, val: Value, loop_info: &LoopInfo) -> bool {
        let val_data = self.func.dfg().value(val);
        match val_data.kind() {
            ValueKind::Integer(_)
            | ValueKind::Undef(_)
            | ValueKind::GlobalAlloc(_)
            | ValueKind::FuncArgRef(_) => true, // Constants and global references are loop-invariant
            _ => {
                for (&bb, node) in self.func.layout().bbs() {
                    if node.insts().contains_key(&val)
                        || self.func.dfg().bb(bb).params().contains(&val)
                    {
                        // If the defining instruction or block argument is inside the loop, it's not invariant
                        return !loop_info.blocks.contains(&bb);
                    }
                }
                false
            }
        }
    }

    /// Replace old_val with new_val in a ValueKind
    fn subst_value_in_kind(kind: &mut ValueKind, old: Value, new: Value) {
        let r = |v: &mut Value| {
            if *v == old {
                *v = new;
            }
        };
        match kind {
            ValueKind::Binary(bin) => {
                r(bin.lhs_mut());
                r(bin.rhs_mut());
            }
            ValueKind::Store(store) => {
                r(store.value_mut());
                r(store.dest_mut());
            }
            ValueKind::Load(load) => {
                r(load.src_mut());
            }
            ValueKind::Branch(br) => {
                r(br.cond_mut());
                for arg in br.true_args_mut() {
                    r(arg);
                }
                for arg in br.false_args_mut() {
                    r(arg);
                }
            }
            ValueKind::Jump(jmp) => {
                for arg in jmp.args_mut() {
                    r(arg);
                }
            }
            ValueKind::Call(call) => {
                for arg in call.args_mut() {
                    r(arg);
                }
            }
            ValueKind::GetPtr(gp) => {
                r(gp.src_mut());
                r(gp.index_mut());
            }
            ValueKind::GetElemPtr(gep) => {
                r(gep.src_mut());
                r(gep.index_mut());
            }
            ValueKind::Return(ret) => {
                if let Some(v) = ret.value_mut() {
                    r(v);
                }
            }
            _ => {}
        }
    }
}
