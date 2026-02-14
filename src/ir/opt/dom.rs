use std::collections::{HashMap, HashSet};

use koopa::ir::{BasicBlock, FunctionData, ValueKind};

/// Dominitor tree
pub struct DomInfo {
    /// block -> immediate dominator
    pub idom: HashMap<BasicBlock, BasicBlock>,
    /// block -> dominator tree
    pub tree: HashMap<BasicBlock, Vec<BasicBlock>>,
    /// block -> dominance frontier
    pub dom_front: HashMap<BasicBlock, Vec<BasicBlock>>,
}

impl DomInfo {
    /// Create a dominator tree
    pub fn new(func: &FunctionData) -> Self {
        let mut dom = Self {
            idom: HashMap::new(),
            tree: HashMap::new(),
            dom_front: HashMap::new(),
        };
        dom.analyze(func);
        dom
    }

    fn analyze(&mut self, func: &FunctionData) {
        // build prev order for CFG
        // compute reverse post-order as well
        let (preds, rpo) = self.build_cfg_info(func);

        // block -> RPO map
        let rpo_map: HashMap<BasicBlock, usize> =
            rpo.iter().enumerate().map(|(i, &bb)| (bb, i)).collect();

        self.compute_idom(func, &preds, &rpo, &rpo_map);

        // contruct dominitor tree
        for (&bb, &parent) in &self.idom {
            if bb != parent {
                self.tree.entry(parent).or_default().push(bb);
            }
        }

        self.compute_dom_frontier(&preds);
    }

    fn build_cfg_info(
        &mut self,
        func: &FunctionData,
    ) -> (HashMap<BasicBlock, Vec<BasicBlock>>, Vec<BasicBlock>) {
        let mut preds: HashMap<BasicBlock, Vec<BasicBlock>> = HashMap::new();
        let mut successors: HashMap<BasicBlock, Vec<BasicBlock>> = HashMap::new();

        for (&bb, node) in func.layout().bbs() {
            // make sure every block are in the map even with no pred
            preds.entry(bb).or_default();

            let mut succs = Vec::new();
            if let Some(last) = node.insts().keys().last() {
                let val_data = func.dfg().value(*last);
                match val_data.kind() {
                    ValueKind::Branch(br) => {
                        succs.push(br.true_bb());
                        succs.push(br.false_bb());
                    }
                    ValueKind::Jump(jmp) => {
                        succs.push(jmp.target());
                    }
                    ValueKind::Return(_) => {}
                    _ => {
                        // Koopa IR 规范：基本块必须以 Terminator 结尾。
                        panic!("The last instruction should be terminator!");
                    }
                }
            }

            for &succ in &succs {
                preds.entry(succ).or_default().push(bb);
            }
            successors.insert(bb, succs);
        }

        // Compute Reverse post-order
        let mut rpo = Vec::new();
        let mut visited = HashSet::new();
        if let Some(entry) = func.layout().entry_bb() {
            self.post_order_dfs(entry, &successors, &mut visited, &mut rpo);
        }
        rpo.reverse();

        (preds, rpo)
    }

    fn post_order_dfs(
        &mut self,
        curr: BasicBlock,
        successors: &HashMap<BasicBlock, Vec<BasicBlock>>,
        visited: &mut HashSet<BasicBlock>,
        result: &mut Vec<BasicBlock>,
    ) {
        visited.insert(curr);
        if let Some(curr_succs) = successors.get(&curr) {
            for &succ in curr_succs {
                if !visited.contains(&succ) {
                    self.post_order_dfs(succ, successors, visited, result);
                }
            }
        }
        result.push(curr);
    }

    fn compute_idom(
        &mut self,
        func: &FunctionData,
        preds: &HashMap<BasicBlock, Vec<BasicBlock>>,
        rpo: &[BasicBlock],
        rpo_map: &HashMap<BasicBlock, usize>,
    ) {
        let entry = func.layout().entry_bb().unwrap();
        // set entry's idom as itself (dummy node)
        self.idom.insert(entry, entry);

        let mut changed = true;
        while changed {
            changed = false;
            // skip entry
            for &bb in rpo.iter().skip(1) {
                let bb_preds = preds.get(&bb).unwrap();
                // find the first pred with idom as candidate
                let mut new_idom = *bb_preds
                    .iter()
                    .find(|&&p| self.idom.contains_key(&p))
                    .expect("Unreachable block found or CFG broken");

                for &p in bb_preds {
                    if p != new_idom && self.idom.contains_key(&p) {
                        new_idom = self.intersect(new_idom, p, rpo_map);
                    }
                }

                if self.idom.get(&bb) != Some(&new_idom) {
                    self.idom.insert(bb, new_idom);
                    changed = true;
                }
            }
        }
    }

    /// Find the LCA of two block in Dominitor Tree
    /// Using RPO Index: dominator has a smaller index than subdominator
    fn intersect(
        &self,
        mut b1: BasicBlock,
        mut b2: BasicBlock,
        rpo_map: &HashMap<BasicBlock, usize>,
    ) -> BasicBlock {
        while b1 != b2 {
            while rpo_map[&b1] > rpo_map[&b2] {
                b1 = self.idom[&b1];
            }
            while rpo_map[&b1] < rpo_map[&b2] {
                b2 = self.idom[&b2];
            }
        }
        b1
    }

    fn compute_dom_frontier(&mut self, preds: &HashMap<BasicBlock, Vec<BasicBlock>>) {
        // DF(n) = { w | n dom pred(w) but n not strictly dom w }
        for (&bb, bb_preds) in preds {
            // Unreachable blocks are not in idom; skip them.
            let Some(&idom_bb) = self.idom.get(&bb) else {
                continue;
            };

            if bb_preds.len() >= 2 {
                for &p in bb_preds {
                    if !self.idom.contains_key(&p) {
                        continue;
                    }
                    let mut runner = p;
                    // runner climbs up to the idom(bb)
                    while runner != idom_bb {
                        self.dom_front.entry(runner).or_default().push(bb);
                        if let Some(&parent) = self.idom.get(&runner) {
                            if parent == runner {
                                break;
                            }
                            runner = parent;
                        } else {
                            break;
                        }
                    }
                }
            }
        }
    }
}
