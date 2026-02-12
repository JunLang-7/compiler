use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, FunctionData, Value, ValueKind};
use std::collections::HashMap;

/// Convert SSA style(Basic block args) back to non-SSA style(alloc/store/load).
/// should be used after optimization and before codegen.
pub fn destruct_ssa(func: &mut FunctionData) {
    // Step 1: 收集所有带参数的基本块
    let bb_params: Vec<(BasicBlock, Vec<Value>)> = func
        .dfg()
        .bbs()
        .iter()
        .filter_map(|(&bb, bb_data)| {
            let params = bb_data.params().to_vec();
            if params.is_empty() {
                None
            } else {
                Some((bb, params))
            }
        })
        .collect();

    if bb_params.is_empty() {
        return;
    }

    let entry = func.layout().entry_bb().unwrap();

    // Step 2: 为每个 block param 创建 alloc（放在 entry 块开头）
    // param_value -> alloc_value
    let mut param_to_alloc: HashMap<Value, Value> = HashMap::new();

    for (_, params) in &bb_params {
        for &param in params {
            let param_ty = func.dfg().value(param).ty().clone();
            let alloc = func.dfg_mut().new_value().alloc(param_ty);
            func.layout_mut()
                .bb_mut(entry)
                .insts_mut()
                .push_key_front(alloc)
                .unwrap();
            param_to_alloc.insert(param, alloc);
        }
    }

    // Step 3: 在每个带参数的 BB 开头插入 load，并 RAUW param -> load
    for &(bb, ref params) in &bb_params {
        let mut load_pairs: Vec<(Value, Value)> = Vec::new();

        for &param in params {
            let alloc = param_to_alloc[&param];
            let load_val = func.dfg_mut().new_value().load(alloc);
            load_pairs.push((param, load_val));
        }

        // 逆序 push_front 保证 load 顺序正确
        for (_, load) in load_pairs.iter().rev() {
            func.layout_mut()
                .bb_mut(bb)
                .insts_mut()
                .push_key_front(*load)
                .unwrap();
        }

        // RAUW: 将 param 的所有使用替换为 load
        for (param, load) in &load_pairs {
            replace_all_uses_with(func, *param, *load);
        }
    }

    // Step 4: 保存 bb->params 映射（下一步要清除 params）
    let bb_param_map: HashMap<BasicBlock, Vec<Value>> = bb_params.iter().cloned().collect();

    // Step 5: 清除所有 BB 的 params（这样后续 jump()/branch() builder 不会报错）
    for &(bb, ref params) in &bb_params {
        func.dfg_mut().bb_mut(bb).params_mut().clear();
        // 移除 BlockArgRef values
        for &param in params {
            if func.dfg().value(param).used_by().is_empty() {
                func.dfg_mut().remove_value(param);
            }
        }
    }

    // Step 6: 处理所有带参数的 terminator
    let all_bbs: Vec<BasicBlock> = func.layout().bbs().keys().cloned().collect();

    for bb in all_bbs {
        let term = func
            .layout()
            .bbs()
            .node(&bb)
            .unwrap()
            .insts()
            .back_key()
            .cloned();
        let Some(term) = term else { continue };

        let term_data = func.dfg().value(term).clone();

        match term_data.kind() {
            ValueKind::Jump(jmp) if !jmp.args().is_empty() => {
                let target = jmp.target();
                let args = jmp.args().to_vec();
                let target_params = &bb_param_map[&target];

                // 创建 stores
                let mut stores = Vec::new();
                for (i, &arg) in args.iter().enumerate() {
                    let alloc = param_to_alloc[&target_params[i]];
                    let store = func.dfg_mut().new_value().store(arg, alloc);
                    stores.push(store);
                }

                // 从 layout 移除 terminator
                func.layout_mut().bb_mut(bb).insts_mut().remove(&term);

                // 插入 stores
                func.layout_mut().bb_mut(bb).insts_mut().extend(stores);

                // 用 raw() 清除 jump args（绕过 builder 断言）
                let mut data = func.dfg().value(term).clone();
                if let ValueKind::Jump(jmp) = data.kind_mut() {
                    jmp.args_mut().clear();
                }
                func.dfg_mut().replace_value_with(term).raw(data);

                // 重新插入 terminator
                func.layout_mut()
                    .bb_mut(bb)
                    .insts_mut()
                    .push_key_back(term)
                    .unwrap();
            }
            ValueKind::Branch(br) => {
                let true_args = br.true_args().to_vec();
                let false_args = br.false_args().to_vec();

                if true_args.is_empty() && false_args.is_empty() {
                    continue;
                }

                let true_bb_target = br.true_bb();
                let false_bb_target = br.false_bb();

                // Branch 带参数需要边分裂（edge splitting）
                // 因为 true/false 路径的 stores 不能同时执行

                let new_true_target = if !true_args.is_empty() {
                    let target_params = &bb_param_map[&true_bb_target];
                    let split_bb = func
                        .dfg_mut()
                        .new_bb()
                        .basic_block(Some("%_split_t".into()));
                    func.layout_mut().bbs_mut().push_key_back(split_bb).unwrap();

                    for (i, &arg) in true_args.iter().enumerate() {
                        let alloc = param_to_alloc[&target_params[i]];
                        let store = func.dfg_mut().new_value().store(arg, alloc);
                        func.layout_mut()
                            .bb_mut(split_bb)
                            .insts_mut()
                            .push_key_back(store)
                            .unwrap();
                    }
                    let jmp = func.dfg_mut().new_value().jump(true_bb_target);
                    func.layout_mut()
                        .bb_mut(split_bb)
                        .insts_mut()
                        .push_key_back(jmp)
                        .unwrap();

                    split_bb
                } else {
                    true_bb_target
                };

                let new_false_target = if !false_args.is_empty() {
                    let target_params = &bb_param_map[&false_bb_target];
                    let split_bb = func
                        .dfg_mut()
                        .new_bb()
                        .basic_block(Some("%_split_f".into()));
                    func.layout_mut().bbs_mut().push_key_back(split_bb).unwrap();

                    for (i, &arg) in false_args.iter().enumerate() {
                        let alloc = param_to_alloc[&target_params[i]];
                        let store = func.dfg_mut().new_value().store(arg, alloc);
                        func.layout_mut()
                            .bb_mut(split_bb)
                            .insts_mut()
                            .push_key_back(store)
                            .unwrap();
                    }
                    let jmp = func.dfg_mut().new_value().jump(false_bb_target);
                    func.layout_mut()
                        .bb_mut(split_bb)
                        .insts_mut()
                        .push_key_back(jmp)
                        .unwrap();

                    split_bb
                } else {
                    false_bb_target
                };

                // 修改 branch：更新目标并清除参数
                let mut data = func.dfg().value(term).clone();
                if let ValueKind::Branch(br) = data.kind_mut() {
                    *br.true_bb_mut() = new_true_target;
                    *br.false_bb_mut() = new_false_target;
                    br.true_args_mut().clear();
                    br.false_args_mut().clear();
                }
                func.dfg_mut().replace_value_with(term).raw(data);
            }
            _ => {}
        }
    }
}

/// Replace all use of `old_val` into `new_val`
fn replace_all_uses_with(func: &mut FunctionData, old_val: Value, new_val: Value) {
    if old_val == new_val {
        return;
    }

    let users: Vec<Value> = func
        .dfg()
        .value(old_val)
        .used_by()
        .iter()
        .cloned()
        .collect();

    for user in users {
        let mut data = func.dfg().value(user).clone();
        subst_value_in_kind(data.kind_mut(), old_val, new_val);
        func.dfg_mut().replace_value_with(user).raw(data);
    }
}

/// Replace `old` into `new` in `ValueKind`.
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
