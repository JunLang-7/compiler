/// Dead Code Elimination Pass
use std::collections::{HashMap, HashSet, VecDeque};
use koopa::ir::{BasicBlock, FunctionData, TypeKind, Value, ValueKind};


pub struct DeadCodeElimination<'a> {
    pub func: &'a mut FunctionData,
}

impl<'a> DeadCodeElimination<'a> {
    /// Create a new Dead Code Elimination Pass
    pub fn new(func: &'a mut FunctionData) -> Self {
        Self { func }
    }

    /// Run the Dead Code Elimination Pass
    pub fn run(&mut self) {
        // Initialize map from alloc to store instructions
        let (alloc_to_stores, mut worklists, mut marked) = self.init();
        // propagate liveness through related instructions
        self.mark(&alloc_to_stores, &mut worklists, &mut marked);
        // remove unmarked instructions
        self.sweep(&marked);
    }

    /// Initialize the data structures for DCE
    fn init(&self) -> (
        HashMap<Value, Vec<Value>>,
        VecDeque<Value>,
        HashSet<Value>,
    ) {
        let mut alloc_to_stores: HashMap<Value, Vec<Value>> = HashMap::new();
        let mut worklists: VecDeque<Value> = VecDeque::new();
        let mut marked: HashSet<Value> = HashSet::new();
        let mut escaping_allocs: HashSet<Value> = HashSet::new();

        for (_, node) in self.func.layout().bbs() {
            for &inst in node.insts().keys() {
                let val_data = self.func.dfg().value(inst);
                if let ValueKind::Call(call) = val_data.kind() {
                    for &arg in call.args() {
                        if let Some(base_alloc) = self.get_base_alloc(arg) {
                            escaping_allocs.insert(base_alloc);
                        }
                    }
                }
            }
        }

        for (_, node) in self.func.layout().bbs() {
            for &inst in node.insts().keys() {
                let val_data = self.func.dfg().value(inst);
                match val_data.kind() {
                    // Control flow instructions are always live
                    ValueKind::Return(_) | ValueKind::Branch(_) | ValueKind::Jump(_) => {
                        marked.insert(inst);
                        worklists.push_back(inst);
                    }
                    // Call instructions are treated as live conservatively
                    ValueKind::Call(_) => {
                        marked.insert(inst);
                        worklists.push_back(inst);
                    }
                    // Store instructions: map alloc to store
                    ValueKind::Store(store) => {
                        let dest = store.dest();
                        if self.is_global_or_param_ptr(dest) {
                            marked.insert(inst);
                            worklists.push_back(inst);
                        } else {
                            if let Some(base_alloc) = self.get_base_alloc(dest) {
                                if escaping_allocs.contains(&base_alloc) {
                                    marked.insert(inst);
                                    worklists.push_back(inst);
                                } else {
                                    alloc_to_stores
                                        .entry(base_alloc)
                                        .or_insert_with(Vec::new)
                                        .push(inst);
                                }
                            }
                        }
                    }
                    _ => { /* other instructions are handled later */}
                }
            }
        }
        (alloc_to_stores, worklists, marked)
    }

    /// Mark reachable instructions starting from worklist
    fn mark(
        &self,
        alloc_to_stores: &HashMap<Value, Vec<Value>>,
        worklists: &mut VecDeque<Value>,
        marked: &mut HashSet<Value>,
    ) {
        while let Some(inst) = worklists.pop_front() {
            let val_data = self.func.dfg().value(inst);
            let operands = match val_data.kind() {
                ValueKind::Binary(bin) => vec![bin.lhs(), bin.rhs()],
                ValueKind::Return(ret) => ret.value().into_iter().collect(),
                ValueKind::Load(load) => vec![load.src()],
                ValueKind::Store(store) => vec![store.value(), store.dest()],
                ValueKind::Branch(br) => vec![br.cond()],
                ValueKind::Call(call) => call.args().to_vec(),
                ValueKind::GetElemPtr(gep) => vec![gep.src(), gep.index()],
                ValueKind::GetPtr(gp) => vec![gp.src(), gp.index()],
                _ => vec![],
            };
            // For each operand of the instruction
            for op in operands {
                // If operand is an instruction and not marked yet
                if self.func.dfg().values().contains_key(&op) && !marked.contains(&op) {
                    marked.insert(op);
                    worklists.push_back(op);
                }
            }

            // Handle Load -> alloc -> Store chain
            if let ValueKind::Load(load) = val_data.kind() {
                let src = load.src();
                if let Some(base_alloc) = self.get_base_alloc(src) {
                    // Activate all stores related to this alloc
                    if let Some(stores) = alloc_to_stores.get(&base_alloc) {
                        for &store_inst in stores {
                            if !marked.contains(&store_inst) {
                                marked.insert(store_inst);
                                worklists.push_back(store_inst);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Remove unmarked instructions from the function
    fn sweep(&mut self, marked: &HashSet<Value>) {
        let mut insts_by_bb: HashMap<BasicBlock, Vec<Value>> = HashMap::new();
        let mut dead_set: HashSet<Value> = HashSet::new();
        for (&bb, node) in self.func.layout().bbs() {
            // Collect instructions that are not marked
            let dead: Vec<Value> = node
                .insts()
                .keys()
                .filter(|inst| !marked.contains(inst))
                .cloned()
                .collect();
            if !dead.is_empty() {
                for inst in &dead {
                    dead_set.insert(*inst);
                }
                insts_by_bb.insert(bb, dead);
            }
        }

        // Remove dead instructions from function in a use-safe order
        let mut removed: HashSet<Value> = HashSet::new();
        let mut progress = true;
        while progress {
            progress = false;
            for (bb, insts) in insts_by_bb.iter() {
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
    }

    fn is_global_or_param_ptr(&self, val: Value) -> bool {
        if self.func.dfg().values().contains_key(&val) {
            let kind = &self.func.dfg().value(val).kind();
            match kind {
                ValueKind::GlobalAlloc(_) => true,
                ValueKind::Load(load) => self.is_pointer_alloc(load.src()),
                ValueKind::GetPtr(gp) => self.is_global_or_param_ptr(gp.src()),
                ValueKind::GetElemPtr(gep) => self.is_global_or_param_ptr(gep.src()),
                ValueKind::Alloc(_) => false,
                _ => false,
            }
        } else {
            true
        }
    }

    /// Check if the value is an allocation of pointer type
    fn is_pointer_alloc(&self, val: Value) -> bool {
        if !self.func.dfg().values().contains_key(&val) {
            return false;
        }
        match self.func.dfg().value(val).kind() {
            ValueKind::Alloc(_) => match self.func.dfg().value(val).ty().kind() {
                TypeKind::Pointer(inner) => matches!(inner.kind(), TypeKind::Pointer(_)),
                _ => false,
            },
            _ => false,
        }
    }

    /// Recursively get the base allocation for a given value
    fn get_base_alloc(&self, val: Value) -> Option<Value> {
        if !self.func.dfg().values().contains_key(&val) {
            return None;
        }
        match self.func.dfg().value(val).kind() {
            ValueKind::Alloc(_) => Some(val),
            ValueKind::GetPtr(gp) => self.get_base_alloc(gp.src()),
            ValueKind::GetElemPtr(gep) => self.get_base_alloc(gep.src()),
            _ => None,
        }
    }
}