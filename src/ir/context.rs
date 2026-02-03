use koopa::ir::{BasicBlock, Function, FunctionData, Program, Value, ValueKind};
use std::collections::HashMap;

/// enumerate for symbol table
#[derive(Clone, Copy, Debug)]
pub enum Symbol {
    Const(i32),
    Var(Value),
    Func(Function),
    Array(Value),
}

/// Context for Koopa IR generation
pub struct GenContext<'a> {
    pub program: &'a mut Program,
    pub current_func: Option<Function>,
    pub scopes: Vec<HashMap<String, Symbol>>,
    pub loop_stack: Vec<(BasicBlock, BasicBlock)>, // (continue_bb, break_bb)
    pub if_counter: usize,
    pub while_counter: usize,
    pub land_counter: usize,
    pub lor_counter: usize,
}

impl<'a> GenContext<'a> {
    /// Create a new GenContext
    pub fn new(program: &'a mut Program) -> Self {
        Self {
            program,
            current_func: None,
            scopes: vec![HashMap::new()],
            loop_stack: vec![],
            if_counter: 0,
            while_counter: 0,
            land_counter: 0,
            lor_counter: 0,
        }
    }

    /// Get mutable reference to the current function data
    pub fn func_mut(&mut self) -> &mut FunctionData {
        self.program
            .func_mut(self.current_func.expect("No current function"))
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Exit the current scope
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    /// Return the symbol table of current scope
    pub fn symbol_table(&mut self) -> &mut HashMap<String, Symbol> {
        self.scopes.last_mut().unwrap()
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
        let func_data = self.func_mut(); // Store mutable reference
        for (b, node) in func_data.layout().bbs() {
            if *b == bb {
                if let Some(last_inst) = node.insts().back_key() {
                    let last = func_data.dfg().value(*last_inst); // Use stored reference
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

    /// Lookup a function by its identifier
    pub fn lookup_function(&self, ident: &str) -> Option<Function> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(ident) {
                if let Symbol::Func(func) = *symbol {
                    return Some(func);
                }
            }
        }
        None
    }
}
