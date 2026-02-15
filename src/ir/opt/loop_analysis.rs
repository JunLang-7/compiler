/// Loop analysis for natural loop detection in the control flow graph.
use super::dom::DomInfo;
use koopa::ir::{BasicBlock, FunctionData, ValueKind};
use std::collections::{HashMap, HashSet};

/// LoopInfo represents a natural loop in the control flow graph,
/// identified by its header block and the blocks that form the loop body.
#[derive(Debug, Clone)]
pub struct LoopInfo {
    /// The header block of the loop, which dominates all blocks in the loop and is the target of back edges
    pub header: BasicBlock,
    /// Preheader is the block that leads into the loop header,
    /// used for placing loop-invariant code
    pub preheader: Option<BasicBlock>,
    /// Basic blocks that form the loop body, including the header
    pub blocks: HashSet<BasicBlock>,
    /// Latches are blocks that jump back to the loop header,
    /// used for identifying loop back edges
    pub latches: HashSet<BasicBlock>,
}

/// LoopAnalysis performs natural loop detection on a function's control flow graph,
/// using the dominator tree to identify back edges and loop headers.
pub struct LoopAnalysis<'a> {
    func: &'a FunctionData,
    dom: &'a DomInfo,
    /// Detected loops in the function, each represented by its LoopInfo
    pub loops: Vec<LoopInfo>,
    /// Predecessor map for all basic blocks,
    /// used for reverse CFG traversal during loop block collection
    preds: HashMap<BasicBlock, Vec<BasicBlock>>,
}

impl<'a> LoopAnalysis<'a> {
    /// Create a new LoopAnalysis for the given function and its dominator information
    pub fn new(func: &'a FunctionData, dom: &'a DomInfo) -> Self {
        let mut analyzer = Self {
            func,
            dom,
            loops: Vec::new(),
            preds: HashMap::new(),
        };
        analyzer.build_preds();
        analyzer.analyze();
        analyzer
    }

    /// Build the predecessor map for all basic blocks in the function
    fn build_preds(&mut self) {
        for (&bb, node) in self.func.layout().bbs() {
            self.preds.entry(bb).or_default();
            if let Some(&term) = node.insts().back_key() {
                let term_data = self.func.dfg().value(term);
                match term_data.kind() {
                    ValueKind::Jump(jmp) => {
                        self.preds.entry(jmp.target()).or_default().push(bb);
                    }
                    ValueKind::Branch(br) => {
                        self.preds.entry(br.true_bb()).or_default().push(bb);
                        self.preds.entry(br.false_bb()).or_default().push(bb);
                    }
                    _ => {}
                }
            }
        }
    }

    /// Analyze the function to identify natural loops based on back edges in the CFG
    fn analyze(&mut self) {
        // Identify back edges in the CFG using the dominator tree
        let mut back_edges = Vec::new();

        for (&bb, _) in self.func.layout().bbs() {
            if let Some(succs) = self.get_successors(bb) {
                for succ in succs {
                    if self.dominates(succ, bb) {
                        back_edges.push((bb, succ)); // bb is latch, succ is header
                    }
                }
            }
        }

        // Group back edges by their loop headers
        let mut loop_by_header: HashMap<BasicBlock, HashSet<BasicBlock>> = HashMap::new();
        for (latch, header) in back_edges {
            loop_by_header.entry(header).or_default().insert(latch);
        }

        // construct closure of blocks for each loop header
        for (header, latches) in loop_by_header {
            let mut blocks = HashSet::new();
            blocks.insert(header);

            // Perform a reverse DFS from the latches to find all blocks in the loop
            for &latch in &latches {
                let mut stack = vec![latch];
                while let Some(node) = stack.pop() {
                    if blocks.insert(node) {
                        if let Some(node_preds) = self.preds.get(&node) {
                            for &pred in node_preds {
                                if pred != header {
                                    stack.push(pred);
                                }
                            }
                        }
                    }
                }
            }

            // Identify preheader: a block that is not in the loop but has an edge to the header
            let mut outside_preds = Vec::new();
            if let Some(header_preds) = self.preds.get(&header) {
                for &pred in header_preds {
                    if !blocks.contains(&pred) {
                        outside_preds.push(pred);
                    }
                }
            }

            let preheader = if outside_preds.len() == 1 {
                // If there's exactly one outside predecessor, it's the preheader
                Some(outside_preds[0])
            } else {
                // In more complex cases, we might need to create a new preheader block,
                // but for now we just note the absence of a unique preheader
                None
            };

            self.loops.push(LoopInfo {
                header,
                preheader,
                blocks,
                latches,
            });
        }
    }

    /// Helper function to judge whether a dominates b
    fn dominates(&self, a: BasicBlock, b: BasicBlock) -> bool {
        let mut curr = b;
        while curr != a {
            if let Some(&parent) = self.dom.idom.get(&curr) {
                if parent == curr {
                    return false; // reached the root without finding a
                }
                curr = parent;
            } else {
                return false; // no dominator info, treat as not dominating
            }
        }
        true
    }

    /// Get the successors of a basic block, if it has a terminator instruction
    fn get_successors(&self, bb: BasicBlock) -> Option<Vec<BasicBlock>> {
        let node = self.func.layout().bbs().node(&bb)?;
        let term = node.insts().back_key()?;
        let term_data = self.func.dfg().value(*term);
        match term_data.kind() {
            ValueKind::Jump(jmp) => Some(vec![jmp.target()]),
            ValueKind::Branch(br) => Some(vec![br.true_bb(), br.false_bb()]),
            _ => None,
        }
    }
}
