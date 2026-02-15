/// A simple constant propagation pass for Koopa IR.
use koopa::ir::{
    BasicBlock, BinaryOp, FunctionData, Value, ValueKind,
    builder::{LocalInstBuilder, ValueBuilder},
};
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LatticeVal {
    Top,
    Const(i32),
    Bottom,
}

pub struct ConstantPropagation<'a> {
    func: &'a mut FunctionData,
    /// Mapping from values to their lattice values
    values: HashMap<Value, LatticeVal>,
    /// Mapping from values to their uses
    uses: HashMap<Value, Vec<Value>>,
    /// Mapping from instructions to their parent basic blocks
    inst_parent: HashMap<Value, BasicBlock>,
    /// executable edges (SourceBB, TargetBB)
    executable_edges: HashSet<(BasicBlock, BasicBlock)>,
    /// predecessors of each basic block that are executable
    executable_preds: HashMap<BasicBlock, Vec<BasicBlock>>,
    /// reachable blocks in the control flow graph
    executable_blocks: HashSet<BasicBlock>,
    /// store the `Instruction` which has changed its state
    ssa_worklist: VecDeque<Value>,
    /// store the reachable edge of (SourceBB, TargetBB)
    flow_worklist: VecDeque<(BasicBlock, BasicBlock)>,
    /// pending flow edges to avoid duplicates
    flow_pending: HashSet<(BasicBlock, BasicBlock)>,
}

impl<'a> ConstantPropagation<'a> {
    /// Create a constant propagation instance
    pub fn new(func: &'a mut FunctionData) -> Self {
        Self {
            func,
            values: HashMap::new(),
            uses: HashMap::new(),
            inst_parent: HashMap::new(),
            executable_edges: HashSet::new(),
            executable_preds: HashMap::new(),
            executable_blocks: HashSet::new(),
            ssa_worklist: VecDeque::new(),
            flow_worklist: VecDeque::new(),
            flow_pending: HashSet::new(),
        }
    }

    /// Implementation of constant propagation algorithm
    pub fn run(&mut self) -> bool {
        self.build_use_def();
        self.init();
        self.propagate();
        self.rewrite()
    }

    /// Build use-def chains for all values in the function
    fn build_use_def(&mut self) {
        for (&bb, node) in self.func.layout().bbs() {
            for &inst in node.insts().keys() {
                self.inst_parent.insert(inst, bb);
                let val_data = self.func.dfg().value(inst);
                let operands = match val_data.kind() {
                    ValueKind::Binary(bin) => vec![bin.lhs(), bin.rhs()],
                    ValueKind::Return(ret) => ret.value().into_iter().collect(),
                    ValueKind::Load(load) => vec![load.src()],
                    ValueKind::Store(store) => vec![store.value(), store.dest()],
                    ValueKind::Branch(br) => br
                        .true_args()
                        .iter()
                        .chain(br.false_args())
                        .copied()
                        .chain(std::iter::once(br.cond()))
                        .collect(),
                    ValueKind::Jump(jmp) => jmp.args().to_vec(),
                    ValueKind::Call(call) => call.args().to_vec(),
                    ValueKind::GetElemPtr(gep) => vec![gep.src(), gep.index()],
                    ValueKind::GetPtr(gp) => vec![gp.src(), gp.index()],
                    _ => vec![],
                };
                for op in operands {
                    self.uses.entry(op).or_default().push(inst);
                }
            }
        }
    }

    /// Initialize the lattice values for all values in the function
    fn init(&mut self) {
        // Initialize all values
        // we only care about values that are "Top" initially.
        // Constants are "Const". Params are "Bottom".
        let value_keys: Vec<Value> = self.func.dfg().values().keys().cloned().collect();
        for val in value_keys {
            let val_data = self.func.dfg().value(val);
            let lat = match val_data.kind() {
                ValueKind::Integer(int) => LatticeVal::Const(int.value()),
                ValueKind::FuncArgRef(_) => LatticeVal::Bottom,
                ValueKind::BlockArgRef(_) => LatticeVal::Top, // Phis & Block args
                ValueKind::Undef(_) => LatticeVal::Top,
                ValueKind::Binary(_) => LatticeVal::Top,
                // Instructions that don't produce values (Store, Branch, Jump, Return)
                // Treat as Top to wait for propagation
                _ => LatticeVal::Top,
            };
            if lat != LatticeVal::Top {
                self.values.insert(val, lat);
            }
        }

        if let Some(entry) = self.func.layout().entry_bb() {
            self.mark_block_executable(entry);
        }
    }

    /// Perform the constant propagation analysis
    fn propagate(&mut self) {
        while !self.ssa_worklist.is_empty() || !self.flow_worklist.is_empty() {
            // First handle flow list
            while let Some((from, to)) = self.flow_worklist.pop_front() {
                self.flow_pending.remove(&(from, to));
                // Mark target block executable and process its instructions if needed.
                if !self.executable_blocks.contains(&to) {
                    self.mark_block_executable(to);
                }

                // update block args of `to`
                self.update_block_args(from, to);
            }

            while let Some(inst) = self.ssa_worklist.pop_front() {
                self.visit_inst(inst);
            }
        }
    }

    /// Rewrite the IR based on the final lattice values after propagation
    /// returns true if any changes were made
    fn rewrite(&mut self) -> bool {
        let mut changed = false;
        // Collect existing integer constants to reuse values
        let mut existing_inst: HashMap<i32, Value> = HashMap::new();
        for (&val, val_data) in self.func.dfg().values() {
            if let ValueKind::Integer(int) = val_data.kind() {
                existing_inst.insert(int.value(), val);
            }
        }

        // Collect values to process first (before mutable borrows)
        let values_to_process: Vec<(Value, LatticeVal)> =
            self.values.iter().map(|(&val, &lat)| (val, lat)).collect();

        let mut replacements = HashMap::new();

        // Collect instructions to modify
        for (val, lat) in values_to_process {
            // Check operands and replace if they are Const
            if let LatticeVal::Const(c) = lat {
                // Only replace users of binary results with constants.
                let val_kind = self.func.dfg().value(val).kind().clone();
                if matches!(
                    val_kind,
                    ValueKind::Binary(_)
                        | ValueKind::Load(_)
                        | ValueKind::GetElemPtr(_)
                        | ValueKind::GetPtr(_)
                        | ValueKind::BlockArgRef(_)
                ) {
                    let const_val = self.get_or_create_const(&mut existing_inst, c);
                    replacements.insert(val, const_val);
                }
            }
        }
        let executable_bbs: Vec<BasicBlock> = self.executable_blocks.iter().cloned().collect();
        for bb in executable_bbs {
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
            // perform the replacements in the IR
            for inst in insts {
                changed |= self.rewrite_inst_op(inst, &replacements, &mut existing_inst);
            }
        }

        changed
    }

    /// Evaluate a binary operation given the lattice values of its operands
    fn eval_binary(&self, op: BinaryOp, lhs: LatticeVal, rhs: LatticeVal) -> LatticeVal {
        match (lhs, rhs) {
            (LatticeVal::Bottom, _) | (_, LatticeVal::Bottom) => LatticeVal::Bottom,
            (LatticeVal::Top, _) | (_, LatticeVal::Top) => LatticeVal::Top,
            (LatticeVal::Const(l), LatticeVal::Const(r)) => {
                let res = match op {
                    BinaryOp::Add => l.wrapping_add(r),
                    BinaryOp::Sub => l.wrapping_sub(r),
                    BinaryOp::Mul => l.wrapping_mul(r),
                    BinaryOp::Div => {
                        if r == 0 {
                            return LatticeVal::Bottom;
                        } else {
                            l.wrapping_div(r)
                        }
                    }
                    BinaryOp::Mod => {
                        if r == 0 {
                            return LatticeVal::Bottom;
                        } else {
                            l.wrapping_rem(r)
                        }
                    }
                    BinaryOp::And => l & r,
                    BinaryOp::Or => l | r,
                    BinaryOp::Xor => l ^ r,
                    BinaryOp::Shl => l.wrapping_shl(r as u32),
                    BinaryOp::Shr => (l as u32).wrapping_shr(r as u32) as i32,
                    BinaryOp::Sar => l.wrapping_shr(r as u32),
                    BinaryOp::Eq => (l == r) as i32,
                    BinaryOp::NotEq => (l != r) as i32,
                    BinaryOp::Lt => (l < r) as i32,
                    BinaryOp::Gt => (l > r) as i32,
                    BinaryOp::Le => (l <= r) as i32,
                    BinaryOp::Ge => (l >= r) as i32,
                };
                LatticeVal::Const(res)
            }
        }
    }

    /// Helper method to get the new operand value for rewriting
    fn get_new_op(
        &mut self,
        op: Value,
        replacements: &HashMap<Value, Value>,
        existing_inst: &mut HashMap<i32, Value>,
    ) -> Option<Value> {
        if let Some(&new_val) = replacements.get(&op) {
            return Some(new_val);
        }
        if let Some(LatticeVal::Const(c)) = self.values.get(&op) {
            return Some(self.get_or_create_const(existing_inst, *c));
        }
        None
    }

    /// Rewrite the operands of an instruction based on the constant propagation results
    /// returns true if any changes were made
    fn rewrite_inst_op(
        &mut self,
        inst: Value,
        replacements: &HashMap<Value, Value>,
        existing_inst: &mut HashMap<i32, Value>,
    ) -> bool {
        let mut changed = false;
        let val_data = self.func.dfg().value(inst).clone();
        match val_data.kind() {
            ValueKind::Binary(bin) => {
                let new_lhs = self
                    .get_new_op(bin.lhs(), replacements, existing_inst)
                    .unwrap_or(bin.lhs());
                let new_rhs = self
                    .get_new_op(bin.rhs(), replacements, existing_inst)
                    .unwrap_or(bin.rhs());
                if new_lhs != bin.lhs() || new_rhs != bin.rhs() {
                    changed = true;
                    self.func
                        .dfg_mut()
                        .replace_value_with(inst)
                        .binary(bin.op(), new_lhs, new_rhs);
                }
            }
            ValueKind::Branch(br) => {
                let new_cond = self
                    .get_new_op(br.cond(), replacements, existing_inst)
                    .unwrap_or(br.cond());
                let cond_is_const = matches!(
                    self.func.dfg().value(new_cond).kind(),
                    ValueKind::Integer(_)
                );
                if cond_is_const {
                    let val =
                        if let ValueKind::Integer(int) = self.func.dfg().value(new_cond).kind() {
                            int.value()
                        } else {
                            panic!("Expected integer value");
                        };

                    let (target_bb, target_args) = if val != 0 {
                        (br.true_bb(), br.true_args())
                    } else {
                        (br.false_bb(), br.false_args())
                    };

                    let new_target_args: Vec<Value> = target_args
                        .iter()
                        .map(|&arg| {
                            self.get_new_op(arg, replacements, existing_inst)
                                .unwrap_or(arg)
                        })
                        .collect();
                    changed = true;
                    // cond branch ro absolute jump
                    self.func
                        .dfg_mut()
                        .replace_value_with(inst)
                        .jump_with_args(target_bb, new_target_args);
                } else if new_cond != br.cond() {
                    changed = true;
                    let new_true_args: Vec<Value> = br
                        .true_args()
                        .iter()
                        .map(|&arg| {
                            self.get_new_op(arg, replacements, existing_inst)
                                .unwrap_or(arg)
                        })
                        .collect();
                    let new_false_args: Vec<Value> = br
                        .false_args()
                        .iter()
                        .map(|&arg| {
                            self.get_new_op(arg, replacements, existing_inst)
                                .unwrap_or(arg)
                        })
                        .collect();
                    self.func
                        .dfg_mut()
                        .replace_value_with(inst)
                        .branch_with_args(
                            new_cond,
                            br.true_bb(),
                            br.false_bb(),
                            new_true_args,
                            new_false_args,
                        );
                }
            }
            ValueKind::Jump(jmp) => {
                let new_args: Vec<Value> = jmp
                    .args()
                    .iter()
                    .map(|&arg| {
                        self.get_new_op(arg, replacements, existing_inst)
                            .unwrap_or(arg)
                    })
                    .collect();
                if new_args.iter().zip(jmp.args()).any(|(&n, &o)| n != o) {
                    changed = true;
                    self.func
                        .dfg_mut()
                        .replace_value_with(inst)
                        .jump_with_args(jmp.target(), new_args);
                }
            }
            ValueKind::Call(call) => {
                let new_args: Vec<Value> = call
                    .args()
                    .iter()
                    .map(|&arg| {
                        self.get_new_op(arg, replacements, existing_inst)
                            .unwrap_or(arg)
                    })
                    .collect();
                if new_args.iter().zip(call.args()).any(|(&n, &o)| n != o) {
                    changed = true;
                    self.func
                        .dfg_mut()
                        .replace_value_with(inst)
                        .call(call.callee(), new_args);
                }
            }
            ValueKind::Return(ret) => {
                if let Some(val) = ret.value() {
                    let new_val = self
                        .get_new_op(val, replacements, existing_inst)
                        .unwrap_or(val);
                    if new_val != val {
                        changed = true;
                        self.func
                            .dfg_mut()
                            .replace_value_with(inst)
                            .ret(Some(new_val));
                    }
                }
            }
            ValueKind::Load(_) => {
                // src is ptr, not const int
            }
            ValueKind::Store(store) => {
                let new_val = self
                    .get_new_op(store.value(), replacements, existing_inst)
                    .unwrap_or(store.value());
                if new_val != store.value() {
                    changed = true;
                    self.func
                        .dfg_mut()
                        .replace_value_with(inst)
                        .store(new_val, store.dest());
                }
            }
            ValueKind::GetElemPtr(gep) => {
                let new_src = self
                    .get_new_op(gep.src(), replacements, existing_inst)
                    .unwrap_or(gep.src());
                let new_index = self
                    .get_new_op(gep.index(), replacements, existing_inst)
                    .unwrap_or(gep.index());
                if new_src != gep.src() || new_index != gep.index() {
                    changed = true;
                    self.func
                        .dfg_mut()
                        .replace_value_with(inst)
                        .get_elem_ptr(new_src, new_index);
                }
            }
            ValueKind::GetPtr(gp) => {
                let new_src = self
                    .get_new_op(gp.src(), replacements, existing_inst)
                    .unwrap_or(gp.src());
                let new_index = self
                    .get_new_op(gp.index(), replacements, existing_inst)
                    .unwrap_or(gp.index());
                if new_src != gp.src() || new_index != gp.index() {
                    changed = true;
                    self.func
                        .dfg_mut()
                        .replace_value_with(inst)
                        .get_ptr(new_src, new_index);
                }
            }
            _ => {}
        }
        changed
    }

    /// Helper to get an existing constant value or create a new one if it doesn't exist
    fn get_or_create_const(
        &mut self,
        existing_inst: &mut HashMap<i32, Value>,
        value: i32,
    ) -> Value {
        if let Some(&v) = existing_inst.get(&value) {
            v
        } else {
            let v = self.func.dfg_mut().new_value().integer(value);
            existing_inst.insert(value, v);
            v
        }
    }

    /// Update the block arguments of a target basic block based on the new argument values
    fn update_block_args(&mut self, _from: BasicBlock, to: BasicBlock) {
        let params = self.func.dfg().bb(to).params().to_vec();
        if params.is_empty() {
            return;
        }

        let preds = match self.executable_preds.get(&to) {
            Some(preds) => preds,
            None => return,
        };

        for (i, &param) in params.iter().enumerate() {
            let mut new_lat = LatticeVal::Top;
            let mut first = true;
            for &pred in preds {
                let arg_val = self.get_jump_arg(pred, to, i);
                let arg_lat = self.get_val_lat(arg_val);
                if first {
                    new_lat = arg_lat;
                    first = false;
                } else {
                    new_lat = self.meet(new_lat, arg_lat);
                }
            }

            // Update state
            let old_lat = self.get_val_lat(param);
            if new_lat != old_lat {
                self.values.insert(param, new_lat);
                if let Some(users) = self.uses.get(&param) {
                    for &user in users {
                        self.ssa_worklist.push_back(user);
                    }
                }
            }
        }
    }

    /// Meet operation for lattice values: combine two lattice values to get the new lattice value
    fn meet(&self, v1: LatticeVal, v2: LatticeVal) -> LatticeVal {
        match (v1, v2) {
            (LatticeVal::Top, x) | (x, LatticeVal::Top) => x,
            (LatticeVal::Bottom, _) | (_, LatticeVal::Bottom) => LatticeVal::Bottom,
            (LatticeVal::Const(c1), LatticeVal::Const(c2)) => {
                if c1 == c2 {
                    LatticeVal::Const(c1)
                } else {
                    LatticeVal::Bottom
                }
            }
        }
    }

    /// Makr block executable and add all instruction into SSA worklist
    fn mark_block_executable(&mut self, bb: BasicBlock) {
        if self.executable_blocks.contains(&bb) {
            return;
        }
        self.executable_blocks.insert(bb);

        for &inst in self.func.layout().bbs().node(&bb).unwrap().insts().keys() {
            self.ssa_worklist.push_back(inst);
        }
    }

    /// Mark edge executable and enqueue flow
    fn mark_edge_executable(&mut self, from: BasicBlock, to: BasicBlock) {
        if !self.executable_edges.contains(&(from, to)) {
            self.executable_edges.insert((from, to));
            self.executable_preds.entry(to).or_default().push(from);
            self.enqueue_flow(from, to);
        }
    }

    /// Enqueue a flow edge if not already pending
    fn enqueue_flow(&mut self, from: BasicBlock, to: BasicBlock) {
        if self.flow_pending.insert((from, to)) {
            self.flow_worklist.push_back((from, to));
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

    /// Visit an instruction and update its lattice value based on its operands
    fn visit_inst(&mut self, inst: Value) {
        let val_kind = self.func.dfg().value(inst).kind().clone();
        let old_lat = self.get_val_lat(inst);

        let new_lat = match val_kind {
            ValueKind::Binary(bin) => {
                let lhs = self.get_val_lat(bin.lhs());
                let rhs = self.get_val_lat(bin.rhs());
                self.eval_binary(bin.op(), lhs, rhs)
            }
            ValueKind::Branch(br) => {
                let cond_lat = self.get_val_lat(br.cond());
                let curr_bb = self.get_inst_parent(inst);
                match cond_lat {
                    LatticeVal::Const(c) => {
                        if c != 0 {
                            self.mark_edge_executable(curr_bb, br.true_bb());
                            self.enqueue_flow(curr_bb, br.true_bb());
                        } else {
                            self.mark_edge_executable(curr_bb, br.false_bb());
                            self.enqueue_flow(curr_bb, br.false_bb());
                        }
                    }
                    // Overdefined condition: conservatively execute both edges.
                    LatticeVal::Bottom => {
                        self.mark_edge_executable(curr_bb, br.true_bb());
                        self.mark_edge_executable(curr_bb, br.false_bb());
                        self.enqueue_flow(curr_bb, br.true_bb());
                        self.enqueue_flow(curr_bb, br.false_bb());
                    }
                    // Unknown condition: do not speculate edge executability.
                    LatticeVal::Top => {}
                }
                return;
            }
            ValueKind::Jump(jmp) => {
                let curr_bb = self.get_inst_parent(inst);
                self.mark_edge_executable(curr_bb, jmp.target());
                self.enqueue_flow(curr_bb, jmp.target());
                return;
            }
            // 在 SSA 下，Load 应该已经被 Mem2Reg 消除了（除非是全局变量/数组）。
            // 如果是数组 Load，通常视为 Bottom。
            ValueKind::Load(_) | ValueKind::Call(_) => LatticeVal::Bottom,
            // Store, Return 等不产生 Value (或 Unit)
            _ => LatticeVal::Bottom,
        };

        if new_lat != old_lat {
            self.values.insert(inst, new_lat);
            if let Some(users) = self.uses.get(&inst) {
                for &user in users {
                    self.ssa_worklist.push_back(user);
                }
            }
        }
    }

    /// Get the lattice value of a given value
    fn get_val_lat(&self, val: Value) -> LatticeVal {
        *self.values.get(&val).unwrap_or(&LatticeVal::Top)
    }

    /// Get the parent basic block of an instruction
    fn get_inst_parent(&self, inst: Value) -> BasicBlock {
        if let Some(&bb) = self.inst_parent.get(&inst) {
            return bb;
        }
        self.get_inst_parent_slow(inst)
    }

    /// Slow path to find the parent basic block of an instruction
    fn get_inst_parent_slow(&self, inst: Value) -> BasicBlock {
        for (&bb, node) in self.func.layout().bbs() {
            if node.insts().contains_key(&inst) {
                return bb;
            }
        }
        panic!("Instruction not found in any block");
    }
}
