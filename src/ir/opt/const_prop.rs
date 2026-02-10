/// A simple constant propagation pass for Koopa IR.
use koopa::ir::{
    BinaryOp, FunctionData, Value, ValueKind,
    builder::{LocalInstBuilder, ValueBuilder},
};
use std::collections::{HashMap, VecDeque};

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
}

impl<'a> ConstantPropagation<'a> {
    /// Create a constant propagation instance
    pub fn new(func: &'a mut FunctionData) -> Self {
        Self {
            func,
            values: HashMap::new(),
            uses: HashMap::new(),
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
        for (_, node) in self.func.layout().bbs() {
            for &inst in node.insts().keys() {
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
                for op in operands {
                    self.uses.entry(op).or_default().push(inst);
                }
            }
        }
    }

    /// Initialize the lattice values for all values in the function
    fn init(&mut self) {
        let mut worklist = VecDeque::new();

        // Initialize all values
        // we only care about values that are "Top" initially.
        // Constants are "Const". Params are "Bottom".
        let value_keys: Vec<Value> = self.func.dfg().values().keys().cloned().collect();
        for val in value_keys {
            let val_data = self.func.dfg().value(val);
            let lat = match val_data.kind() {
                ValueKind::Integer(int) => LatticeVal::Const(int.value()),
                ValueKind::FuncArgRef(_) => LatticeVal::Bottom,
                ValueKind::BlockArgRef(_) => LatticeVal::Bottom, // Phis & Block args
                ValueKind::Undef(_) => LatticeVal::Top,
                ValueKind::Binary(_) => LatticeVal::Top,
                // Result of Load/Call/Alloc/GetPtr/GetElemPtr is unknown at compile time (pointer or memory)
                ValueKind::Load(_)
                | ValueKind::Call(_)
                | ValueKind::Alloc(_)
                | ValueKind::GlobalAlloc(_)
                | ValueKind::GetElemPtr(_)
                | ValueKind::GetPtr(_) => LatticeVal::Bottom,
                // Instructions that don't produce values (Store, Branch, Jump, Return)
                // effectively don't have a lattice value, treat them as Bottom to be safe.
                _ => LatticeVal::Bottom,
            };
            self.values.insert(val, lat);

            // Add Top values to worklist
            if let ValueKind::Binary(_) = val_data.kind() {
                worklist.push_back(val);
            }
        }

        self.process_worklist(worklist);
    }

    /// Perform the constant propagation analysis
    fn propagate(&mut self) {
        let bb_insts: Vec<Vec<Value>> = self
            .func
            .layout()
            .bbs()
            .nodes()
            .map(|bb| bb.insts().keys().cloned().collect())
            .collect();

        let mut changed = true;
        while changed {
            changed = false;
            let mut worklist = VecDeque::new();

            // Simple local memory const propagation within each basic block.
            for insts in &bb_insts {
                let mut mem_consts: HashMap<Value, Option<i32>> = HashMap::new();
                for &inst in insts {
                    let val_data = self.func.dfg().value(inst);
                    match val_data.kind() {
                        ValueKind::Store(store) => {
                            if let Some(alloc) = self.get_direct_alloc(store.dest()) {
                                let val_lat = self.values.get(&store.value()).copied();
                                let new_const = match val_lat {
                                    Some(LatticeVal::Const(c)) => Some(c),
                                    _ => None,
                                };
                                mem_consts.insert(alloc, new_const);
                            }
                        }
                        ValueKind::Load(load) => {
                            if let Some(alloc) = self.get_direct_alloc(load.src()) {
                                if let Some(Some(c)) = mem_consts.get(&alloc) {
                                    changed |= self.mark_load_const(inst, *c, &mut worklist);
                                }
                            }
                        }
                        ValueKind::Call(call) => {
                            // Calls may modify memory through pointers; conservatively clear.
                            if !call.args().is_empty() {
                                mem_consts.clear();
                            }
                        }
                        _ => {}
                    }
                }
            }

            changed |= self.process_worklist(worklist);
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
        let bb_insts: Vec<Value> = self
            .func
            .layout()
            .bbs()
            .nodes()
            .flat_map(|bb| bb.insts().keys().cloned())
            .collect();
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
                ) {
                    let const_val = self.get_or_create_const(&mut existing_inst, c);
                    replacements.insert(val, const_val);
                }
            }
        }
        // perform the replacements in the IR
        for inst in bb_insts {
            changed |= self.rewrite_inst_op(inst, &replacements, &mut existing_inst);
        }
        changed
    }

    /// Process the worklist of values that may have changed lattice values
    /// and propagate changes to their users
    fn process_worklist(&mut self, mut worklist: VecDeque<Value>) -> bool {
        let mut changed = false;
        while let Some(inst) = worklist.pop_front() {
            let val_data = self.func.dfg().value(inst);
            // We only propagate for Binary instructions in this simple CP
            match val_data.kind() {
                ValueKind::Binary(bin) => {
                    let lhs_lat = *self.values.get(&bin.lhs()).unwrap_or(&LatticeVal::Bottom);
                    let rhs_lat = *self.values.get(&bin.rhs()).unwrap_or(&LatticeVal::Bottom);
                    let old_lat = *self.values.get(&inst).unwrap_or(&LatticeVal::Bottom);
                    let new_lat = self.eval_binary(bin.op(), lhs_lat, rhs_lat);

                    if old_lat != new_lat {
                        self.values.insert(inst, new_lat);
                        changed = true;
                        // Add users to worklist
                        if let Some(users) = self.uses.get(&inst) {
                            for &user in users {
                                worklist.push_back(user);
                            }
                        }
                    }
                }
                _ => {}
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

    /// Helper to check if a value is a direct alloc (not through pointers)
    fn get_direct_alloc(&self, val: Value) -> Option<Value> {
        if !self.func.dfg().values().contains_key(&val) {
            return None;
        }
        match self.func.dfg().value(val).kind() {
            ValueKind::Alloc(_) => Some(val),
            _ => None,
        }
    }

    /// Mark a load instruction as having a constant value and update the worklist accordingly
    fn mark_load_const(
        &mut self,
        load_inst: Value,
        c: i32,
        worklist: &mut VecDeque<Value>,
    ) -> bool {
        let old_lat = *self.values.get(&load_inst).unwrap_or(&LatticeVal::Bottom);
        let new_lat = LatticeVal::Const(c);
        if old_lat != new_lat {
            self.values.insert(load_inst, new_lat);
            if let Some(users) = self.uses.get(&load_inst) {
                for &user in users {
                    worklist.push_back(user);
                }
            }
            return true;
        }
        false
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
}
