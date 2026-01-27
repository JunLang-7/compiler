use koopa::ir::{BasicBlock, FunctionData, Value, ValueKind};
use std::collections::HashMap;

/// enumerate for symbol table
#[derive(Clone, Copy)]
pub enum Symbol {
    Const(i32),
    Var(Value),
}

/// Context for Koopa IR generation
pub struct GenContext<'a> {
    pub func_data: &'a mut FunctionData,
    pub scopes: Vec<HashMap<String, Symbol>>,
    pub loop_stack: Vec<(BasicBlock, BasicBlock)>, // (continue_bb, break_bb)
    pub if_counter: usize,
    pub while_counter: usize,
    pub land_counter: usize,
    pub lor_counter: usize,
}

impl<'a> GenContext<'a> {
    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Exit the current scope
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    /// Insert a symbol into the current scope
    pub fn insert_symbol(&mut self, ident: String, symbol: Symbol) {
        self.scopes.last_mut().unwrap().insert(ident, symbol);
    }

    /// Lookup a symbol in the scopes
    pub fn lookup_symbol(&self, ident: &str) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(ident) {
                return Some(*symbol);
            }
        }
        None
    }

    /// Enter a loop with given continue and break basic blocks
    pub fn enter_loop(&mut self, entry: BasicBlock, end: BasicBlock) {
        self.loop_stack.push((entry, end));
    }

    /// Exit the current loop
    pub fn exit_loop(&mut self) {
        self.loop_stack.pop();
    }

    /// Get the continue and break basic blocks of the current loop
    pub fn current_loop(&self) -> Option<(BasicBlock, BasicBlock)> {
        self.loop_stack.last().cloned()
    }

    /// Check if a basic block is terminated
    pub fn is_bb_terminated(&mut self, bb: BasicBlock) -> bool {
        for (b, node) in self.func_data.layout().bbs() {
            if *b == bb {
                if let Some(last_inst) = node.insts().back_key() {
                    let last = self.func_data.dfg().value(*last_inst);
                    match last.kind() {
                        ValueKind::Return(_) | ValueKind::Branch(_) | ValueKind::Jump(_) => {
                            return true;
                        }
                        _ => return false,
                    }
                } else {
                    return false;
                }
            }
        }
        false
    }
}
