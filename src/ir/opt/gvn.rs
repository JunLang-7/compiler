use super::dom::DomInfo;
use koopa::ir::builder::ValueBuilder;
use koopa::ir::{BasicBlock, BinaryOp, FunctionData, Value, ValueKind};
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

/// Expression for value numbering
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Expr {
    Binary(BinaryOp, Value, Value),
    GetElemPtr(Value, Value),
    GetPtr(Value, Value),
    Phi(BasicBlock, Vec<(BasicBlock, Value)>),
}

/// Global value numbering optimization pass
pub struct GlobalValueNumbering<'a> {
    func: &'a mut FunctionData,
    dom: DomInfo,
    /// expression -> value map
    value_map: HashMap<Expr, Value>,
    /// old value -> new value map
    replacements: HashMap<Value, Value>,
    /// block -> predecessors map (for phi construction)
    preds: HashMap<BasicBlock, Vec<BasicBlock>>,
    bb_order: HashMap<BasicBlock, usize>,
}

impl<'a> GlobalValueNumbering<'a> {
    /// Create a new GVN pass for the given function
    pub fn new(func: &'a mut FunctionData) -> Self {
        let dom = DomInfo::new(func);
        let mut preds: HashMap<BasicBlock, Vec<BasicBlock>> = HashMap::new();
        let mut bb_order: HashMap<BasicBlock, usize> = HashMap::new();
        let mut idx = 0;
        for (&bb, node) in func.layout().bbs() {
            bb_order.insert(bb, idx);
            idx += 1;
            // ensure every block is in the map even if it has no preds
            preds.entry(bb).or_default();
            if let Some(&term) = node.insts().keys().last() {
                let val_data = func.dfg().value(term);
                match val_data.kind() {
                    ValueKind::Branch(br) => {
                        preds.entry(br.true_bb()).or_default().push(bb);
                        preds.entry(br.false_bb()).or_default().push(bb);
                    }
                    ValueKind::Jump(jmp) => {
                        preds.entry(jmp.target()).or_default().push(bb);
                    }
                    _ => {}
                }
            }
        }
        Self {
            func,
            dom,
            value_map: HashMap::new(),
            replacements: HashMap::new(),
            preds,
            bb_order,
        }
    }

    /// Run the GVN pass and return whether any changes were made
    pub fn run(&mut self) -> bool {
        let entry = self.func.layout().entry_bb().unwrap();
        self.run_on_block(entry);
        self.apply_replacements()
    }

    /// Run GVN on a basic block and its dominated blocks
    fn run_on_block(&mut self, bb: BasicBlock) {
        let mut scope_keys = Vec::new();

        // Handle phi node first
        let params = self.func.dfg().bb(bb).params().to_vec();
        for (id, &param) in params.iter().enumerate() {
            let mut incoming = Vec::new();
            if let Some(preds) = self.preds.get(&bb) {
                for &pred in preds {
                    let arg_val = self.get_jump_arg(pred, bb, id);
                    // Use the latest replacement for the argument value
                    incoming.push((pred, self.get_latest_value(arg_val)));
                }
            }

            if incoming.is_empty() {
                continue;
            }

            // Trival phi elimination
            // if all incoming values are the same, replace the phi with that value
            let first_val = incoming[0].1;
            if incoming.iter().all(|&(_, v)| v == first_val) {
                self.replacements.insert(param, first_val);
                continue; // no need to add the phi to the value map
            }

            // Create the phi expression and add to the value map
            incoming.sort_by_key(|&(p, _)| self.bb_order.get(&p).cloned().unwrap_or(usize::MAX));
            let key = Expr::Phi(bb, incoming);

            if let Some(&existing) = self.value_map.get(&key) {
                // Found an existing phi, replace with the existing value
                self.replacements.insert(param, existing);
            } else {
                // New phi, add to the map
                self.value_map.insert(key.clone(), param);
                scope_keys.push(key);
            }
        }

        // Process instructions in the current block
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
            let key = self.get_expr_key(inst);
            if let Some(key) = key {
                if let Some(&existing) = self.value_map.get(&key) {
                    // Found an existing expression, replace with the existing value
                    self.replacements.insert(inst, existing);
                } else {
                    // New expression, add to the map
                    self.value_map.insert(key.clone(), inst);
                    scope_keys.push(key);
                }
            }
        }

        // Recursively process dominated blocks
        if let Some(children) = self.dom.tree.get(&bb) {
            let children = children.clone();
            for child in children {
                self.run_on_block(child);
            }
        }

        // Remove the expressions defined in this block from the map to maintain correct scoping
        for key in scope_keys {
            self.value_map.remove(&key);
        }
    }

    /// Apply the recorded replacements and return whether any changes were made
    fn apply_replacements(&mut self) -> bool {
        if self.replacements.is_empty() {
            return false;
        }

        // Replace all uses of the old values with the new values
        let to_replace: Vec<(Value, Value)> = self
            .replacements
            .iter()
            .map(|(&old, &new)| (old, self.get_latest_value(new)))
            .collect();

        for (old, new) in to_replace {
            self.replace_all_uses_with(old, new);
        }

        // Collect instructions that have been replaced for potential removal
        let replaced: HashSet<Value> = self.replacements.keys().cloned().collect();
        let mut insts_by_bb: HashMap<BasicBlock, Vec<Value>> = HashMap::new();
        for (&bb, node) in self.func.layout().bbs() {
            let insts: Vec<Value> = node
                .insts()
                .keys()
                .filter(|inst| replaced.contains(inst))
                .cloned()
                .collect();
            if !insts.is_empty() {
                insts_by_bb.insert(bb, insts);
            }
        }

        // Remove instructions that have been replaced
        let mut removed: HashSet<Value> = HashSet::new();
        let mut progress = true;
        while progress {
            progress = false;
            for (bb, insts) in &insts_by_bb {
                for &inst in insts {
                    if removed.contains(&inst) {
                        continue;
                    }
                    if self.func.dfg().value(inst).used_by().is_empty() {
                        self.func.layout_mut().bb_mut(*bb).insts_mut().remove(&inst);
                        self.func.dfg_mut().remove_value(inst);
                        removed.insert(inst);
                        progress = true;
                    }
                }
            }
        }

        true
    }

    /// Get the expression key for a value if it is an instruction we care about
    fn get_expr_key(&self, val: Value) -> Option<Expr> {
        let val_data = self.func.dfg().value(val);
        match val_data.kind() {
            ValueKind::Binary(bin) => {
                let mut lhs = self.get_latest_value(bin.lhs());
                let mut rhs = self.get_latest_value(bin.rhs());
                match bin.op() {
                    BinaryOp::Add
                    | BinaryOp::Mul
                    | BinaryOp::And
                    | BinaryOp::Or
                    | BinaryOp::Xor
                    | BinaryOp::Eq
                    | BinaryOp::NotEq => {
                        // Commutative operations: order the operands to canonicalize
                        if self.value_sort_key(lhs) > self.value_sort_key(rhs) {
                            std::mem::swap(&mut lhs, &mut rhs);
                        }
                    }
                    _ => {}
                }
                Some(Expr::Binary(bin.op(), lhs, rhs))
            }
            ValueKind::GetElemPtr(gep) => {
                let base = self.get_latest_value(gep.src());
                let index = self.get_latest_value(gep.index());
                Some(Expr::GetElemPtr(base, index))
            }
            ValueKind::GetPtr(getptr) => {
                let base = self.get_latest_value(getptr.src());
                let offset = self.get_latest_value(getptr.index());
                Some(Expr::GetPtr(base, offset))
            }
            _ => None,
        }
    }

    /// Get the latest replacement for a value, following the chain of replacements
    fn get_latest_value(&self, val: Value) -> Value {
        let mut curr = val;
        while let Some(&next) = self.replacements.get(&curr) {
            curr = next;
        }
        curr
    }

    /// Build a deterministic sort key for values (used for commutative ops).
    fn value_sort_key(&self, val: Value) -> (u64, String) {
        let mut hasher = DefaultHasher::new();
        val.hash(&mut hasher);
        let hash = hasher.finish();
        let dbg = format!("{:?}", val);
        (hash, dbg)
    }

    /// Replace all uses of old_val with new_val (RAUW)
    fn replace_all_uses_with(&mut self, old_val: Value, new_val: Value) {
        if old_val == new_val {
            return;
        }

        let users: Vec<Value> = self
            .func
            .dfg()
            .value(old_val)
            .used_by()
            .iter()
            .cloned()
            .collect();

        for user in users {
            let mut data = self.func.dfg().value(user).clone();
            Self::subst_value_in_kind(data.kind_mut(), old_val, new_val);
            self.func.dfg_mut().replace_value_with(user).raw(data);
        }
    }

    /// Replace old with new in ValueKind
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
            ValueKind::Return(ret) => {
                if let Some(val) = ret.value_mut() {
                    r(val);
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
            _ => {}
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
}
