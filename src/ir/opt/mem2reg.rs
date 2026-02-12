use super::dom::DomInfo;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, FunctionData, TypeKind, Value, ValueKind};
use std::collections::{HashMap, HashSet};

pub struct Mem2Reg<'a> {
    func: &'a mut FunctionData,
    /// Dominator Tree
    dom: DomInfo,
    /// Record which basic blocks define (Store) each Alloc instruction
    alloc_defs: HashMap<Value, HashSet<BasicBlock>>,
    /// Record the mapping from (BasicBlock, Alloc)
    /// to the corresponding BlockArg Value (Phi).
    /// Map<BasicBlock, Map<AllocValue, BlockArgValue>>
    phi_map: HashMap<BasicBlock, HashMap<Value, Value>>,
    /// Alloc list corresponding to parms of each BasicBlock
    bb_param_allocs: HashMap<BasicBlock, Vec<Value>>,
    /// Varable stack used in variable renaming：AllocValue -> Stack<CurrentValue>
    var_stacks: HashMap<Value, Vec<Value>>,
}

impl<'a> Mem2Reg<'a> {
    pub fn new(func: &'a mut FunctionData) -> Self {
        let dom = DomInfo::new(func);
        Self {
            func,
            dom,
            alloc_defs: HashMap::new(),
            phi_map: HashMap::new(),
            bb_param_allocs: HashMap::new(),
            var_stacks: HashMap::new(),
        }
    }

    pub fn run(&mut self) -> bool {
        if !self.analyze_allocs() {
            return false; // No variable to optimize
        }

        self.insert_phis();
        self.rename_vars();
        self.clean_up()
    }

    /// Analyze the function to find all Alloc instructions and their defining blocks (Store).
    fn analyze_allocs(&mut self) -> bool {
        let bbs: Vec<(BasicBlock, Vec<Value>)> = self
            .func
            .layout()
            .bbs()
            .iter()
            .map(|(&bb, node)| {
                let insts: Vec<Value> = node.insts().keys().cloned().collect();
                (bb, insts)
            })
            .collect();

        for (bb, insts) in bbs {
            for inst in insts {
                let val_data = self.func.dfg().value(inst);
                match val_data.kind() {
                    // 记录 Alloc 变量
                    ValueKind::Alloc(_) => {
                        // 只有分配基本整数类型的 alloc 才能被提升 (i32)
                        let ty = val_data.ty();
                        if let TypeKind::Pointer(base) = ty.kind() {
                            if base.is_i32() {
                                self.alloc_defs.insert(inst, HashSet::new());
                                self.var_stacks.insert(inst, Vec::new());
                            }
                        }
                    }
                    // 记录 Store 定义
                    ValueKind::Store(store) => {
                        let dest = store.dest();
                        if let Some(defs) = self.alloc_defs.get_mut(&dest) {
                            defs.insert(bb);
                        }
                    }
                    _ => {}
                }
            }
        }
        !self.alloc_defs.is_empty()
    }

    /// Insert phi nodes (basic block parameters) at the dominance frontier of each Alloc's defining blocks.
    fn insert_phis(&mut self) {
        // Phase 1: 确定所有需要插入 phi 的 (alloc, bb) 对
        let mut phi_placements: HashMap<BasicBlock, Vec<Value>> = HashMap::new();

        for (&alloc, def_blocks) in &self.alloc_defs {
            let mut worklist: Vec<BasicBlock> = def_blocks.iter().cloned().collect();
            let mut visited = HashSet::new();

            let mut i = 0;
            while i < worklist.len() {
                let bb = worklist[i];
                i += 1;

                if let Some(frontier) = self.dom.dom_front.get(&bb) {
                    for &df_bb in frontier {
                        if !visited.contains(&df_bb) {
                            visited.insert(df_bb);
                            phi_placements.entry(df_bb).or_default().push(alloc);
                            worklist.push(df_bb);
                        }
                    }
                }
            }
        }

        // Phase 2: 为每个需要 phi 的 BB 创建基本块参数
        for (&bb, allocs) in &phi_placements {
            for &alloc in allocs {
                // 获取 alloc 的基类型 (i32)
                let ty = self.func.dfg().value(alloc).ty().clone();
                let arg_ty = match ty.kind() {
                    TypeKind::Pointer(base) => base.clone(),
                    _ => unreachable!(),
                };

                // 通过创建临时 BB 获取 BlockArgRef Value
                let temp_bb = self
                    .func
                    .dfg_mut()
                    .new_bb()
                    .basic_block_with_params(None, vec![arg_ty]);
                let block_arg = self.func.dfg().bb(temp_bb).params()[0];

                // 修正 BlockArgRef 的 index 为目标 BB 中的正确位置
                let correct_index = self.func.dfg().bb(bb).params().len();
                let mut data = self.func.dfg().value(block_arg).clone();
                if let ValueKind::BlockArgRef(arg) = data.kind_mut() {
                    *arg.index_mut() = correct_index;
                }
                self.func.dfg_mut().replace_value_with(block_arg).raw(data);

                // 将参数转移到目标 BB
                self.func.dfg_mut().bb_mut(bb).params_mut().push(block_arg);

                // 清理临时 BB
                self.func.dfg_mut().bb_mut(temp_bb).params_mut().clear();
                self.func.dfg_mut().remove_bb(temp_bb);

                // 记录映射
                self.phi_map.entry(bb).or_default().insert(alloc, block_arg);
                self.bb_param_allocs.entry(bb).or_default().push(alloc);
            }
        }
    }

    /// Rename variables: Traverse the dominator tree, replace loads with current values, and update stores to change the current value.
    fn rename_vars(&mut self) {
        let entry = self.func.layout().entry_bb().unwrap();
        self.rename_recursive(entry);
    }

    /// Recursive helper for variable renaming.
    fn rename_recursive(&mut self, bb: BasicBlock) {
        // 记录每个 Alloc 在本块中压栈次数，回溯时 pop
        let mut push_counts: HashMap<Value, usize> = HashMap::new();

        // 处理本块中的 BlockArg 定义 (Phi)
        if let Some(phis) = self.phi_map.get(&bb) {
            for (&alloc, &phi_val) in phis {
                self.var_stacks.get_mut(&alloc).unwrap().push(phi_val);
                *push_counts.entry(alloc).or_default() += 1;
            }
        }

        // 遍历并处理指令
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
            let val_data = self.func.dfg().value(inst).clone();
            match val_data.kind() {
                ValueKind::Load(load) => {
                    let src = load.src();
                    if self.var_stacks.contains_key(&src) {
                        // 这是一个针对 promoted alloc 的 Load
                        let new_val = self.var_stacks.get(&src).and_then(|s| s.last().cloned());
                        match new_val {
                            Some(new_val) => {
                                // RAUW: 将 inst 的所有使用替换为 new_val
                                self.replace_all_uses_with(inst, new_val);
                            }
                            None => {
                                // 使用了未初始化的变量，替换为 0
                                let zero = self.func.dfg_mut().new_value().integer(0);
                                self.replace_all_uses_with(inst, zero);
                            }
                        }
                    }
                }
                ValueKind::Store(store) => {
                    let dest = store.dest();
                    if self.var_stacks.contains_key(&dest) {
                        // 这是一个针对 promoted alloc 的 Store
                        // 将 Store 的值压入栈
                        let val = store.value();
                        self.var_stacks.get_mut(&dest).unwrap().push(val);
                        *push_counts.entry(dest).or_default() += 1;
                    }
                }
                _ => {}
            }
        }

        // 填充后继块的基本块参数
        let term_inst = self
            .func
            .layout()
            .bbs()
            .node(&bb)
            .unwrap()
            .insts()
            .back_key()
            .cloned();

        if let Some(term_inst) = term_inst {
            let term_data = self.func.dfg().value(term_inst).clone();

            match term_data.kind() {
                ValueKind::Jump(jmp) => {
                    let target = jmp.target();
                    let args = self.collect_jump_args(target);
                    if !args.is_empty() {
                        // 替换 Jump 为带参数版本
                        self.func
                            .dfg_mut()
                            .replace_value_with(term_inst)
                            .jump_with_args(target, args);
                    }
                }
                ValueKind::Branch(br) => {
                    let cond = br.cond();
                    let true_bb = br.true_bb();
                    let false_bb = br.false_bb();

                    let true_args = self.collect_jump_args(true_bb);
                    let false_args = self.collect_jump_args(false_bb);

                    if !true_args.is_empty() || !false_args.is_empty() {
                        if true_bb == false_bb {
                            // Koopa 不允许同一目标的 branch 带参数，转换为 jump
                            self.func
                                .dfg_mut()
                                .replace_value_with(term_inst)
                                .jump_with_args(true_bb, true_args);
                        } else {
                            self.func
                                .dfg_mut()
                                .replace_value_with(term_inst)
                                .branch_with_args(cond, true_bb, false_bb, true_args, false_args);
                        }
                    }
                }
                _ => {}
            }
        }

        // D. 递归访问支配树子节点
        let children = self.dom.tree.get(&bb).cloned().unwrap_or_default();
        for child in children {
            self.rename_recursive(child);
        }

        // E. 回溯：恢复栈状态 (Pop)
        for (alloc, count) in push_counts {
            let stack = self.var_stacks.get_mut(&alloc).unwrap();
            for _ in 0..count {
                stack.pop();
            }
        }
    }

    /// Clean up: Remove all Alloc, Load, and Store instructions related to promoted variables.
    fn clean_up(&mut self) -> bool {
        let mut changed = false;

        let bbs: Vec<BasicBlock> = self.func.layout().bbs().keys().cloned().collect();

        for bb in bbs {
            let mut to_remove = Vec::new();
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
                let val_data = self.func.dfg().value(inst);
                match val_data.kind() {
                    ValueKind::Alloc(_) => {
                        if self.alloc_defs.contains_key(&inst) {
                            to_remove.push(inst);
                        }
                    }
                    ValueKind::Store(store) => {
                        if self.alloc_defs.contains_key(&store.dest()) {
                            to_remove.push(inst);
                        }
                    }
                    ValueKind::Load(load) => {
                        if self.alloc_defs.contains_key(&load.src()) {
                            to_remove.push(inst);
                        }
                    }
                    _ => {}
                }
            }

            for inst in to_remove {
                // 从 Layout 中移除指令
                self.func.layout_mut().bb_mut(bb).insts_mut().remove(&inst);
                // 从 DFG 中移除 Value（需确保没有其他用户）
                if self.func.dfg().value(inst).used_by().is_empty() {
                    self.func.dfg_mut().remove_value(inst);
                }
                changed = true;
            }
        }

        changed
    }

    /// Helper function: Collect the actual arguments from the current stacks based on the target block's parameter mapping.
    fn collect_jump_args(&mut self, target_bb: BasicBlock) -> Vec<Value> {
        let mut args = Vec::new();

        if let Some(allocs) = self.bb_param_allocs.get(&target_bb).cloned() {
            for alloc in allocs {
                if let Some(stack) = self.var_stacks.get(&alloc) {
                    if let Some(&val) = stack.last() {
                        args.push(val);
                    } else {
                        // 未初始化的变量，默认为 0
                        let zero = self.func.dfg_mut().new_value().integer(0);
                        args.push(zero);
                    }
                }
            }
        }

        args
    }

    /// Helper function: Replace all uses of old_val with new_val (RAUW)
    /// Use clone + kind_mut + raw to avoid builder's argument checking assertions
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
}
