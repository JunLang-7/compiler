use super::LiveInterval;
use koopa::ir::{BasicBlock, FunctionData, Value, ValueKind, layout::BasicBlockNode};
use std::collections::{HashMap, HashSet};

/// Liveness Analysis for a function
pub struct LivenessAnalysis<'a> {
    func: &'a FunctionData,
    inst_ids: HashMap<Value, u32>,
    call_inst_ids: HashSet<u32>,
    pub intervals: HashMap<Value, LiveInterval>,
}

impl<'a> LivenessAnalysis<'a> {
    /// Create a new Liveness Analysis instance
    pub fn new(func: &'a FunctionData) -> Self {
        Self {
            func,
            inst_ids: HashMap::new(),
            call_inst_ids: HashSet::new(),
            intervals: HashMap::new(),
        }
    }

    /// Perform liveness analysis and compute live intervals
    pub fn analyze(&mut self) {
        self.number_instructions();
        self.compute_liveness();
        self.mark_cross_call();
        self.compute_hints();
    }

    /// Number instructions linearly for liveness analysis
    fn number_instructions(&mut self) {
        let mut id = 0;

        // Handle parameters
        for &parm in self.func.params() {
            self.inst_ids.insert(parm, id);
            self.intervals.insert(
                parm,
                LiveInterval {
                    value: parm,
                    start: id,
                    end: id + 1,
                    reg: None,
                    cross_call: false,
                    reg_hint: None,
                    coalesce_hint: None,
                },
            );
        }
        id += 2;

        // Handle instructions in basic blocks in layout order
        for (&bb, node) in self.func.layout().bbs() {
            // Handle BB params (SSA block arguments)
            for &param in self.func.dfg().bb(bb).params() {
                self.inst_ids.insert(param, id);
                self.intervals.insert(
                    param,
                    LiveInterval {
                        value: param,
                        start: id,
                        end: id + 1,
                        reg: None,
                        cross_call: false,
                        reg_hint: None,
                        coalesce_hint: None,
                    },
                );
                id += 2;
            }
            for &inst in node.insts().keys() {
                self.inst_ids.insert(inst, id);
                // Record call instructions
                if let ValueKind::Call(_) = self.func.dfg().value(inst).kind() {
                    self.call_inst_ids.insert(id);
                }
                id += 2;
            }
        }
    }

    /// Compute liveness sets and live intervals
    fn compute_liveness(&mut self) {
        let mut live_in: HashMap<BasicBlock, HashSet<Value>> = HashMap::new();
        let mut live_out: HashMap<BasicBlock, HashSet<Value>> = HashMap::new();
        let mut use_def: HashMap<BasicBlock, (HashSet<Value>, HashSet<Value>)> = HashMap::new();

        // Map for access to basic block nodes
        let bb_nodes: HashMap<BasicBlock, _> = self
            .func
            .layout()
            .bbs()
            .iter()
            .map(|(&b, n)| (b, n))
            .collect();
        let bbs: Vec<BasicBlock> = self.func.layout().bbs().keys().cloned().collect();

        // Step 1: Calculate Live Use & Live Def
        self.live_use_def(&bbs, &bb_nodes, &mut use_def);
        // Step 2: Calculate Live In & Live Out
        self.live_in_out(&bbs, &use_def, &mut live_in, &mut live_out);
        // Step 3: Compute Live Intervals
        self.live_intervals(&bbs, &bb_nodes, &live_out);
    }

    /// Calculate Live Use and Live Def sets
    fn live_use_def(
        &self,
        bbs: &Vec<BasicBlock>,
        bb_nodes: &HashMap<BasicBlock, &BasicBlockNode>,
        use_def: &mut HashMap<BasicBlock, (HashSet<Value>, HashSet<Value>)>,
    ) {
        for &bb in bbs.iter() {
            let mut live_uses = HashSet::new();
            let mut live_defs = HashSet::new();

            for &param in self.func.dfg().bb(bb).params() {
                live_defs.insert(param);
            }

            let node = bb_nodes.get(&bb).unwrap();
            for &inst in node.insts().keys() {
                let val_data = self.func.dfg().value(inst);

                // Add operands to uses
                match val_data.kind() {
                    ValueKind::Binary(bin) => {
                        self.check_use(bin.lhs(), &mut live_uses, &live_defs);
                        self.check_use(bin.rhs(), &mut live_uses, &live_defs);
                    }
                    ValueKind::Return(ret) => {
                        if let Some(ret_val) = ret.value() {
                            self.check_use(ret_val, &mut live_uses, &live_defs);
                        }
                    }
                    ValueKind::Load(load) => {
                        self.check_use(load.src(), &mut live_uses, &live_defs);
                    }
                    ValueKind::Store(store) => {
                        self.check_use(store.value(), &mut live_uses, &live_defs);
                        self.check_use(store.dest(), &mut live_uses, &live_defs);
                    }
                    ValueKind::Branch(br) => {
                        self.check_use(br.cond(), &mut live_uses, &live_defs);
                        for &arg in br.true_args() {
                            self.check_use(arg, &mut live_uses, &live_defs);
                        }
                        for &arg in br.false_args() {
                            self.check_use(arg, &mut live_uses, &live_defs);
                        }
                    }
                    ValueKind::Jump(jmp) => {
                        for &arg in jmp.args() {
                            self.check_use(arg, &mut live_uses, &live_defs);
                        }
                    }
                    ValueKind::Call(call) => {
                        for &arg in call.args() {
                            self.check_use(arg, &mut live_uses, &live_defs);
                        }
                    }
                    ValueKind::GetElemPtr(gep) => {
                        self.check_use(gep.src(), &mut live_uses, &live_defs);
                        self.check_use(gep.index(), &mut live_uses, &live_defs);
                    }
                    ValueKind::GetPtr(gp) => {
                        self.check_use(gp.src(), &mut live_uses, &live_defs);
                        self.check_use(gp.index(), &mut live_uses, &live_defs);
                    }
                    _ => {}
                }

                // Add defined value to defs
                if !val_data.ty().is_unit() {
                    live_defs.insert(inst);
                }
            }
            use_def.insert(bb, (live_uses, live_defs));
        }
    }

    /// Calculate Live In and Live Out sets
    fn live_in_out(
        &mut self,
        bbs: &Vec<BasicBlock>,
        use_def: &HashMap<BasicBlock, (HashSet<Value>, HashSet<Value>)>,
        live_in: &mut HashMap<BasicBlock, HashSet<Value>>,
        live_out: &mut HashMap<BasicBlock, HashSet<Value>>,
    ) {
        let mut changed = true;
        while changed {
            changed = false;
            for &bb in bbs.iter().rev() {
                let mut new_out = HashSet::new();
                for succ in self.get_successors(bb) {
                    if let Some(succ_in) = live_in.get(&succ) {
                        new_out.extend(succ_in.iter().cloned());
                    }
                }

                let (uses, defs) = use_def.get(&bb).unwrap();
                let mut new_in = new_out.clone();
                // in[B] = use[B] ∪ (out[B] - def[B])
                new_in.retain(|v| !defs.contains(v));
                new_in.extend(uses.iter().cloned());

                let mut update = |map: &mut HashMap<BasicBlock, HashSet<Value>>,
                                  new_val: HashSet<Value>| {
                    if let Some(old_val) = map.get(&bb) {
                        if *old_val != new_val {
                            map.insert(bb, new_val);
                            changed = true;
                        }
                    } else {
                        map.insert(bb, new_val);
                        changed = true;
                    }
                };
                update(live_in, new_in);
                update(live_out, new_out);
            }
        }
    }

    /// Compute live intervals from live in/out sets
    fn live_intervals(
        &mut self,
        bbs: &Vec<BasicBlock>,
        bb_nodes: &HashMap<BasicBlock, &BasicBlockNode>,
        live_out: &HashMap<BasicBlock, HashSet<Value>>,
    ) {
        // Default intervals for all values
        for (val, &id) in self.inst_ids.iter() {
            if !self.is_variable(*val) {
                continue;
            }
            self.intervals.insert(
                *val,
                LiveInterval {
                    value: *val,
                    start: id,
                    end: id,
                    reg: None,
                    cross_call: false,
                    reg_hint: None,
                    coalesce_hint: None,
                },
            );
        }

        // Extend intervals based on liveness
        for &bb in bbs.iter().rev() {
            let node = bb_nodes.get(&bb).unwrap();
            let block_out = live_out.get(&bb).cloned().unwrap_or_default();

            // Ensure all block params overlap at block entry to avoid sharing a reg.
            let params = self.func.dfg().bb(bb).params();
            if !params.is_empty() {
                let mut max_id = 0;
                for &param in params.iter() {
                    if let Some(&id) = self.inst_ids.get(&param) {
                        if id > max_id {
                            max_id = id;
                        }
                    }
                }
                let entry_end = max_id + 1;
                for &param in params.iter() {
                    self.extend_interval(param, entry_end);
                }
            }

            let insts: Vec<_> = node.insts().keys().cloned().collect();
            if insts.is_empty() {
                continue;
            }
            let block_end = self.inst_ids[insts.last().unwrap()] + 1;
            for &val in &block_out {
                self.extend_interval(val, block_end);
            }

            let mut current_live = block_out;
            for &inst in insts.iter().rev() {
                let inst_id = self.inst_ids[&inst];
                let val_data = self.func.dfg().value(inst);

                if !val_data.ty().is_unit() {
                    current_live.remove(&inst);
                }
                self.process_operands(inst, &mut current_live, inst_id);
            }
        }
    }

    /// Mark live intervals that cross function calls
    fn mark_cross_call(&mut self) {
        for interval in self.intervals.values_mut() {
            for &call_id in self.call_inst_ids.iter() {
                if interval.start < call_id && call_id < interval.end {
                    interval.cross_call = true;
                    break;
                }
            }
        }
    }

    /// Compute register hints based on calling conventions
    fn compute_hints(&mut self) {
        // ABI hints for function calls and returns
        for (&inst, _) in self.inst_ids.iter() {
            let val_data = self.func.dfg().value(inst);

            match val_data.kind() {
                ValueKind::Call(call) => {
                    for (i, &arg) in call.args().iter().enumerate() {
                        if i < 8 {
                            let target_reg = 10 + i as i32; // a0-a7
                            if let Some(interval) = self.intervals.get_mut(&arg) {
                                interval.reg_hint = Some(target_reg);
                            }
                        }
                    }
                    if !val_data.ty().is_unit() {
                        if let Some(interval) = self.intervals.get_mut(&inst) {
                            interval.reg_hint = Some(10); // a0
                        }
                    }
                }
                ValueKind::Return(ret) => {
                    if let Some(ret_val) = ret.value() {
                        if let Some(interval) = self.intervals.get_mut(&ret_val) {
                            interval.reg_hint = Some(10); // a0
                        }
                    }
                }
                _ => {}
            }
        }

        // Coalesce hints for block arguments (Phi nodes elimination)
        self.compute_coalesce_hints();
    }

    /// Compute coalesce hints for register coalescing optimization
    /// When Jump target(arg) or Branch bb(arg), try to assign arg and param the same register
    fn compute_coalesce_hints(&mut self) {
        let bbs: Vec<_> = self
            .func
            .layout()
            .bbs()
            .iter()
            .map(|(_bb, node)| node)
            .collect();

        for node in bbs {
            if let Some(&terminator) = node.insts().keys().last() {
                let term_data = self.func.dfg().value(terminator);

                match term_data.kind() {
                    ValueKind::Jump(jmp) => {
                        let target_bb = jmp.target();
                        let args = jmp.args();
                        let params: Vec<_> = self
                            .func
                            .dfg()
                            .bb(target_bb)
                            .params()
                            .iter()
                            .copied()
                            .collect();

                        // Try to coalesce each arg-param pair
                        for (arg, param) in args.iter().zip(params.iter()) {
                            self.try_coalesce(*arg, *param);
                        }
                    }
                    ValueKind::Branch(br) => {
                        // Handle true branch
                        let true_bb = br.true_bb();
                        let true_args = br.true_args();
                        let true_params: Vec<_> = self
                            .func
                            .dfg()
                            .bb(true_bb)
                            .params()
                            .iter()
                            .copied()
                            .collect();

                        for (arg, param) in true_args.iter().zip(true_params.iter()) {
                            self.try_coalesce(*arg, *param);
                        }

                        // Handle false branch
                        let false_bb = br.false_bb();
                        let false_args = br.false_args();
                        let false_params: Vec<_> = self
                            .func
                            .dfg()
                            .bb(false_bb)
                            .params()
                            .iter()
                            .copied()
                            .collect();

                        for (arg, param) in false_args.iter().zip(false_params.iter()) {
                            self.try_coalesce(*arg, *param);
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    /// Try to coalesce two values if beneficial
    /// Sets coalesce hints to encourage LSRA to assign the same register
    fn try_coalesce(&mut self, arg: Value, param: Value) {
        if !self.is_variable(arg) || !self.is_variable(param) {
            return;
        }

        let arg_interval = match self.intervals.get(&arg) {
            Some(interval) => interval.clone(),
            None => return,
        };
        let param_interval = match self.intervals.get(&param) {
            Some(interval) => interval.clone(),
            None => return,
        };

        // Only coalesce when intervals do not overlap.
        let no_overlap =
            arg_interval.end <= param_interval.start || param_interval.end <= arg_interval.start;
        if !no_overlap {
            return;
        }

        // Set bidirectional coalesce hints
        // LSRA will try to use these hints if possible (i.e., if no conflicts)
        if let Some(interval) = self.intervals.get_mut(&arg) {
            interval.coalesce_hint = Some(param);
        }
        if let Some(interval) = self.intervals.get_mut(&param) {
            interval.coalesce_hint = Some(arg);
        }
    }

    /// Helper to check and add to uses set
    fn check_use(&self, val: Value, uses: &mut HashSet<Value>, defs: &HashSet<Value>) {
        if self.is_variable(val) && !defs.contains(&val) {
            uses.insert(val);
        }
    }

    // Check if value is a "variable" (inst result) vs constant
    fn is_variable(&self, val: Value) -> bool {
        if !self.func.dfg().values().contains_key(&val) {
            return false;
        }
        !matches!(
            self.func.dfg().value(val).kind(),
            ValueKind::Integer(_)
                | ValueKind::GlobalAlloc(_)
                | ValueKind::ZeroInit(_)
                | ValueKind::Alloc(_)
        )
    }

    /// Get successors of a basic block
    fn get_successors(&self, bb: BasicBlock) -> Vec<BasicBlock> {
        let node = self
            .func
            .layout()
            .bbs()
            .iter()
            .find(|(b, _)| **b == bb)
            .unwrap()
            .1;
        if let Some(&term) = node.insts().keys().last() {
            match self.func.dfg().value(term).kind() {
                ValueKind::Branch(br) => vec![br.true_bb(), br.false_bb()],
                ValueKind::Jump(jmp) => vec![jmp.target()],
                _ => vec![],
            }
        } else {
            vec![]
        }
    }

    /// Extend the live interval of a value to at least 'end'
    fn extend_interval(&mut self, val: Value, end: u32) {
        if let Some(interval) = self.intervals.get_mut(&val) {
            if end > interval.end {
                interval.end = end;
            }
        }
    }

    /// Process operands of an instruction to update liveness
    fn process_operands(&mut self, inst: Value, live: &mut HashSet<Value>, inst_id: u32) {
        let val_data = self.func.dfg().value(inst);
        let mut ops = Vec::new();
        match val_data.kind() {
            ValueKind::Binary(bin) => {
                ops.push(bin.lhs());
                ops.push(bin.rhs());
            }
            ValueKind::Return(ret) => {
                if let Some(ret_val) = ret.value() {
                    ops.push(ret_val);
                }
            }
            ValueKind::Load(load) => {
                ops.push(load.src());
            }
            ValueKind::Store(store) => {
                ops.push(store.value());
                ops.push(store.dest());
            }
            ValueKind::Branch(br) => {
                ops.push(br.cond());
                ops.extend(br.true_args());
                ops.extend(br.false_args());
            }
            ValueKind::Jump(jmp) => {
                ops.extend(jmp.args());
            }
            ValueKind::Call(call) => {
                ops.extend(call.args());
            }
            ValueKind::GetElemPtr(gep) => {
                ops.push(gep.src());
                ops.push(gep.index());
            }
            ValueKind::GetPtr(gp) => {
                ops.push(gp.src());
                ops.push(gp.index());
            }
            _ => {}
        }

        for op in ops {
            if self.is_variable(op) {
                self.extend_interval(op, inst_id);
                live.insert(op);
            }
        }
    }
}
