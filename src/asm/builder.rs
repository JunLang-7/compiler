use super::calc_type_size;
use super::reg_alloc::{LinearScan, LivenessAnalysis, Location};
use super::riscv::*;
use koopa::ir::{
    BasicBlock, BinaryOp, FunctionData, Program, Type, TypeKind, Value, ValueKind, values::*,
};
use std::{
    collections::{HashMap, HashSet},
    io::Result,
};

/// Represents the source of a value for parallel move purposes
#[derive(Clone)]
enum CopySrc {
    Loc(Location),
    Imm(i32),
    Global(String),
}

/// Key for tracking locations in parallel move resolution (registers and stack slots)
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum LocKey {
    Reg(i32),
    Stack(i32),
}

/// Represents a single move operation for parallel move resolution,
/// including source/destination and their keys for cycle detection
#[derive(Clone)]
struct CopyMove {
    dst: Location,
    dst_key: LocKey,
    src: CopySrc,
    src_key: Option<LocKey>,
}

/// Asm generator for a single function
pub struct RiscvFuncBuilder<'a> {
    program: &'a Program,
    func_data: &'a FunctionData,
    // inst -> allocation (Reg or Spilled/Stack)
    allocation: HashMap<Value, Location>,
    stack_size: i32,
    has_call: bool,
    used_callee_saved_regs: HashSet<i32>,
    riscv_func: RiscvFunc,
    current_insts: Vec<Inst>,
    // BB -> unique label (handles Koopa's duplicate BB names)
    bb_labels: HashMap<BasicBlock, String>,
    extra_blocks: Vec<RiscvBlock>,
    edge_block_counter: u32,
}

impl<'a> RiscvFuncBuilder<'a> {
    /// Create a new AsmGen instance
    pub fn new(program: &'a Program, func_data: &'a FunctionData) -> Self {
        // Run Liveness Analysis
        let mut liveness = LivenessAnalysis::new(func_data);
        liveness.analyze();
        let mut intervals: Vec<_> = liveness.intervals.values().cloned().collect();

        // Run LSRA
        let mut lsra = LinearScan::new();
        let mut allocation = lsra.run(&mut intervals);

        // Collect used callee-saved regs
        let mut used_callee_saved_regs = HashSet::new();
        for loc in allocation.values() {
            if let Location::Reg(r) = loc {
                if Reg::is_callee_saved(&Reg::from_index(*r)) {
                    used_callee_saved_regs.insert(*r);
                }
            }
        }

        let (stack_size, has_call) =
            Self::analyze_stack_frame(func_data, &mut allocation, &used_callee_saved_regs);

        let riscv_func = RiscvFunc {
            name: String::new(),
            blocks: Vec::new(),
            stack_size,
        };
        let current_insts = Vec::new();
        let extra_blocks = Vec::new();

        Self {
            program,
            func_data,
            allocation,
            stack_size,
            has_call,
            used_callee_saved_regs,
            riscv_func,
            current_insts,
            bb_labels: HashMap::new(),
            extra_blocks,
            edge_block_counter: 0,
        }
    }

    /// Generate asm code for the function
    pub fn generate(mut self) -> Result<RiscvFunc> {
        self.riscv_func.name = self.func_data.name().trim_start_matches('@').to_string();

        // Build unique BB label map (Koopa DFG may store duplicate names)
        let func_name = self.func_data.name().trim_start_matches('@').to_string();
        let mut name_counts: HashMap<String, usize> = HashMap::new();
        for &bb in self.func_data.layout().bbs().keys() {
            let bb_data = self.func_data.dfg().bb(bb);
            let raw_name = if let Some(bb_name) = bb_data.name() {
                bb_name.trim_start_matches('%').to_string()
            } else {
                "unknown".to_string()
            };
            let count = name_counts.entry(raw_name.clone()).or_insert(0);
            let label = if *count == 0 {
                format!("{}_{}", func_name, raw_name)
            } else {
                format!("{}_{}_{}", func_name, raw_name, count)
            };
            *count += 1;
            self.bb_labels.insert(bb, label);
        }
        // prologue
        self.generate_prologue()?;
        if !self.current_insts.is_empty() {
            let insts = std::mem::take(&mut self.current_insts);
            self.riscv_func.blocks.push(RiscvBlock {
                label: String::new(),
                insts,
            });
        }

        for (&bb, node) in self.func_data.layout().bbs() {
            let label = self.get_bb_label(bb);
            self.current_insts.clear();
            for &inst in node.insts().keys() {
                let value_data = self.func_data.dfg().value(inst);
                match value_data.kind() {
                    ValueKind::Integer(_) => {}
                    ValueKind::Alloc(_) => {} // Handled by stack map setup
                    ValueKind::Return(ret) => self.generate_return(ret)?,
                    ValueKind::Binary(bin) => self.generate_binary(inst, bin)?,
                    ValueKind::Load(load) => self.generate_load(inst, load)?,
                    ValueKind::Store(store) => self.generate_store(store)?,
                    ValueKind::Branch(branch) => self.generate_branch(branch)?,
                    ValueKind::Jump(jmp) => self.generate_jump(jmp)?,
                    ValueKind::Call(call) => self.generate_call(inst, call)?,
                    ValueKind::GetElemPtr(gep) => self.generate_get_elem_ptr(inst, gep)?,
                    ValueKind::GetPtr(gp) => self.generate_get_ptr(inst, gp)?,
                    _ => unreachable!(),
                }
            }
            let insts = std::mem::take(&mut self.current_insts);
            self.riscv_func.blocks.push(RiscvBlock { label, insts });
        }
        // trailing edge blocks for branches/jumps with args
        self.riscv_func
            .blocks
            .extend(std::mem::take(&mut self.extra_blocks));
        Ok(self.riscv_func)
    }

    /// Analyze stack frame layout for the function
    fn analyze_stack_frame(
        func_data: &FunctionData,
        allocation: &mut HashMap<Value, Location>,
        used_callee_saved: &HashSet<i32>,
    ) -> (i32, bool) {
        let mut has_call = false;
        let mut max_call_args = 0;

        // 1. Call overflow args
        for (_, node) in func_data.layout().bbs() {
            for &inst in node.insts().keys() {
                let val_data = func_data.dfg().value(inst);
                if let ValueKind::Call(call) = val_data.kind() {
                    has_call = true;
                    max_call_args = max_call_args.max(call.args().len() as i32);
                }
            }
        }

        let overflow_size = if max_call_args > 8 {
            (max_call_args - 8) * 4
        } else {
            0
        };
        let mut offset = overflow_size; // Start of local variables & spills

        // 2. Identify LSRA spills to shift them later
        // LSRA results are already in `allocation`.
        let lsra_spilled_values: Vec<Value> = allocation
            .iter()
            .filter_map(|(v, loc)| matches!(loc, Location::Stack(_)).then(|| *v))
            .collect();

        // Calculate size required by LSRA spills
        let max_lsra_slot = allocation
            .values()
            .filter_map(|loc| match loc {
                Location::Stack(slot) => Some(*slot),
                _ => None,
            })
            .max()
            .unwrap_or(-4); // if -4, size is 0

        let lsra_spill_size = max_lsra_slot + 4; // 0->4, 4->8...

        // Shift LSRA spills to be AFTER Allocs? Or BEFORE?
        // Let's put Allocs FIRST (at `offset`), then LSRA spills.

        // 3. Allocate Locals (Alloc instructions)
        for (_, node) in func_data.layout().bbs() {
            for &inst in node.insts().keys() {
                let val_data = func_data.dfg().value(inst);
                if !val_data.ty().is_unit() {
                    match val_data.kind() {
                        ValueKind::Alloc(_) => {
                            let base_ty = match val_data.ty().kind() {
                                TypeKind::Pointer(t) => t,
                                _ => panic!("Alloc must be pointer type"),
                            };
                            let size = calc_type_size(base_ty) as i32;
                            allocation.insert(inst, Location::Stack(offset));
                            offset += size;
                        }
                        _ => {}
                    }
                }
            }
        }

        // 4. Now shift LSRA spills
        // They were at 0, 4, 8... relative to "start of spills"
        for val in lsra_spilled_values {
            if let Location::Stack(slot_idx) = allocation[&val] {
                let new_offset = offset + slot_idx;
                allocation.insert(val, Location::Stack(new_offset));
            }
        }

        offset += lsra_spill_size;

        // 5. Saved Regs (Callee saved)

        // Args 0-7
        for (i, &parm) in func_data.params().iter().enumerate() {
            if i < 8 {
                allocation.insert(parm, Location::Stack(offset));
                offset += 4;
            }
        }

        // 3. Callee Saved Registers Space
        offset += (used_callee_saved.len() as i32) * 4;

        // 4. RA Space
        if has_call {
            offset += 4;
        }

        // 5. Alignment
        let stack_size = (offset + 15) & !15;

        // 6. Caller Stack Frame (Args >= 8)
        for (i, &parm) in func_data.params().iter().enumerate() {
            if i >= 8 {
                let parm_offset = stack_size + ((i as i32 - 8) * 4);
                allocation.insert(parm, Location::Stack(parm_offset));
            }
        }

        (stack_size, has_call)
    }

    /// Generate asm code for the function prologue
    fn generate_prologue(&mut self) -> Result<()> {
        if self.stack_size > 0 {
            if self.stack_size < -2048 || self.stack_size > 2047 {
                self.push_inst(Inst::Li(Reg::T0, -self.stack_size));
                self.push_inst(Inst::Add(Reg::Sp, Reg::Sp, Reg::T0));
            } else {
                self.push_inst(Inst::Addi(Reg::Sp, Reg::Sp, -self.stack_size));
            }
            // Save RA
            if self.has_call {
                self.safe_sw(Reg::Ra, self.stack_size - 4)?;
            }

            // Save Callee Saved Regs
            // Location = stack_size - 4 (if has_call) - 4 * (i + 1)
            let mut callee_save_offset = self.stack_size - 4;
            if self.has_call {
                callee_save_offset -= 4;
            }
            let mut saved_regs: Vec<i32> = self.used_callee_saved_regs.iter().cloned().collect();
            saved_regs.sort();
            for idx in saved_regs {
                let reg = Reg::from_index(idx);
                self.safe_sw(reg, callee_save_offset)?;
                callee_save_offset -= 4;
            }
        }

        // Save parameters to stack (original behavior)
        for (i, &param) in self.func_data.params().iter().enumerate() {
            if i < 8 {
                if let Some(Location::Stack(offset)) = self.allocation.get(&param) {
                    let a = Reg::from_index(10 + i as i32);
                    self.push_inst(Inst::Sw(a, Reg::Sp, *offset));
                }
            }
        }

        // Move Allocations for Parameters
        for (i, &param) in self.func_data.params().iter().enumerate() {
            if i < 8 {
                if let Some(Location::Reg(r)) = self.allocation.get(&param) {
                    let r = Reg::from_index(*r);
                    let a = Reg::from_index(10 + i as i32);
                    if r != a {
                        self.push_inst(Inst::Mv(r, a));
                    }
                }
            }
        }

        Ok(())
    }

    /// Generate asm code for the function epilogue
    fn generate_epilogue(&mut self) -> Result<()> {
        // Restore Callee Saved Regs
        if self.stack_size > 0 {
            let mut callee_save_offset = self.stack_size - 4;
            if self.has_call {
                callee_save_offset -= 4;
            }
            let mut saved_regs: Vec<i32> = self.used_callee_saved_regs.iter().cloned().collect();
            saved_regs.sort();
            for idx in saved_regs {
                let reg = Reg::from_index(idx);
                self.safe_lw(reg, callee_save_offset)?;
                callee_save_offset -= 4;
            }

            if self.has_call {
                self.safe_lw(Reg::Ra, self.stack_size - 4)?;
            }
            if self.stack_size < -2048 || self.stack_size > 2047 {
                self.push_inst(Inst::Li(Reg::T0, self.stack_size));
                self.push_inst(Inst::Add(Reg::Sp, Reg::Sp, Reg::T0));
            } else {
                self.push_inst(Inst::Addi(Reg::Sp, Reg::Sp, self.stack_size));
            }
        }
        self.push_inst(Inst::Ret);
        Ok(())
    }

    /// Generate asm code for a return operation
    fn generate_return(&mut self, ret: &Return) -> Result<()> {
        if let Some(ret_val) = ret.value() {
            self.load_to_reg(ret_val, Reg::A0)?;
        }
        // epilogue
        self.generate_epilogue()?;
        Ok(())
    }

    /// Generate asm code for a binary operation
    fn generate_binary(&mut self, inst: Value, bin: &Binary) -> Result<()> {
        // Load operands
        self.load_to_reg(bin.lhs(), Reg::T0)?;
        self.load_to_reg(bin.rhs(), Reg::T1)?;

        match bin.op() {
            BinaryOp::Add => self.push_inst(Inst::Add(Reg::T0, Reg::T0, Reg::T1)),
            BinaryOp::Sub => self.push_inst(Inst::Sub(Reg::T0, Reg::T0, Reg::T1)),
            BinaryOp::Mul => self.push_inst(Inst::Mul(Reg::T0, Reg::T0, Reg::T1)),
            BinaryOp::Div => self.push_inst(Inst::Div(Reg::T0, Reg::T0, Reg::T1)),
            BinaryOp::Mod => self.push_inst(Inst::Rem(Reg::T0, Reg::T0, Reg::T1)),
            BinaryOp::And => self.push_inst(Inst::And(Reg::T0, Reg::T0, Reg::T1)),
            BinaryOp::Or => self.push_inst(Inst::Or(Reg::T0, Reg::T0, Reg::T1)),
            BinaryOp::Xor => self.push_inst(Inst::Xor(Reg::T0, Reg::T0, Reg::T1)),
            BinaryOp::Shl => self.push_inst(Inst::Sll(Reg::T0, Reg::T0, Reg::T1)),
            BinaryOp::Shr => self.push_inst(Inst::Srl(Reg::T0, Reg::T0, Reg::T1)),
            BinaryOp::Sar => self.push_inst(Inst::Sra(Reg::T0, Reg::T0, Reg::T1)),
            BinaryOp::Eq => {
                self.push_inst(Inst::Xor(Reg::T0, Reg::T0, Reg::T1));
                self.push_inst(Inst::Seqz(Reg::T0, Reg::T0));
            }
            BinaryOp::NotEq => {
                self.push_inst(Inst::Xor(Reg::T0, Reg::T0, Reg::T1));
                self.push_inst(Inst::Snez(Reg::T0, Reg::T0));
            }
            BinaryOp::Lt => self.push_inst(Inst::Slt(Reg::T0, Reg::T0, Reg::T1)),
            BinaryOp::Gt => self.push_inst(Inst::Sgt(Reg::T0, Reg::T0, Reg::T1)),
            BinaryOp::Le => {
                self.push_inst(Inst::Sgt(Reg::T0, Reg::T0, Reg::T1));
                self.push_inst(Inst::Seqz(Reg::T0, Reg::T0));
            }
            BinaryOp::Ge => {
                self.push_inst(Inst::Slt(Reg::T0, Reg::T0, Reg::T1));
                self.push_inst(Inst::Seqz(Reg::T0, Reg::T0));
            }
        }

        self.store_from_reg(inst, Reg::T0)?;
        Ok(())
    }

    /// Generate asm code for a load operation
    fn generate_load(&mut self, inst: Value, load: &Load) -> Result<()> {
        self.load_to_reg(load.src(), Reg::T0)?;
        self.push_inst(Inst::Lw(Reg::T0, Reg::T0, 0));
        self.store_from_reg(inst, Reg::T0)?;
        Ok(())
    }

    /// Generate asm code for a store operation
    fn generate_store(&mut self, store: &Store) -> Result<()> {
        self.load_to_reg(store.value(), Reg::T0)?;
        self.load_to_reg(store.dest(), Reg::T1)?;
        self.push_inst(Inst::Sw(Reg::T0, Reg::T1, 0));
        Ok(())
    }

    /// Generate asm code for a branch operation
    fn generate_branch(&mut self, branch: &Branch) -> Result<()> {
        self.load_to_reg(branch.cond(), Reg::T0)?;
        let true_bb = branch.true_bb();
        let false_bb = branch.false_bb();
        let true_args = branch.true_args().to_vec();
        let false_args = branch.false_args().to_vec();

        let true_label = self.get_bb_label(true_bb);
        let false_label = self.get_bb_label(false_bb);

        let true_target_label = if true_args.is_empty() {
            true_label.clone()
        } else {
            let edge_label = self.new_edge_label("br_t");
            let edge_block = self.build_edge_block(edge_label.clone(), true_bb, true_args)?;
            self.extra_blocks.push(edge_block);
            edge_label
        };

        let false_target_label = if false_args.is_empty() {
            false_label.clone()
        } else {
            let edge_label = self.new_edge_label("br_f");
            let edge_block = self.build_edge_block(edge_label.clone(), false_bb, false_args)?;
            self.extra_blocks.push(edge_block);
            edge_label
        };

        self.push_inst(Inst::Bnez(Reg::T0, true_target_label));
        self.push_inst(Inst::J(false_target_label));
        Ok(())
    }

    /// Generate asm code for a jump operation
    fn generate_jump(&mut self, jmp: &Jump) -> Result<()> {
        let target = jmp.target();
        let args = jmp.args().to_vec();
        if !args.is_empty() {
            self.emit_block_arg_moves(target, &args)?;
        }
        let label = self.get_bb_label(target);
        self.push_inst(Inst::J(label));
        Ok(())
    }

    /// Generate asm code for a call operation
    fn generate_call(&mut self, inst: Value, call: &Call) -> Result<()> {
        for (i, &arg) in call.args().iter().enumerate() {
            if i < 8 {
                let dst = Reg::from_index(10 + i as i32);
                self.load_to_reg(arg, dst)?;
            } else {
                self.load_to_reg(arg, Reg::T0)?;
                let offset = (i as i32 - 8) * 4;
                self.push_inst(Inst::Sw(Reg::T0, Reg::Sp, offset));
            }
        }
        let callee = self.program.func(call.callee());
        let name = callee.name().trim_start_matches('@');
        self.push_inst(Inst::Call(name.to_string()));

        if !self.func_data.dfg().value(inst).ty().is_unit() {
            self.store_from_reg(inst, Reg::A0)?;
        }
        Ok(())
    }

    /// Generate asm code for a get element pointer operation
    fn generate_get_elem_ptr(&mut self, inst: Value, gep: &GetElemPtr) -> Result<()> {
        self.load_to_reg(gep.src(), Reg::T0)?;
        self.load_to_reg(gep.index(), Reg::T1)?;
        let src_ty = self.value_ty(gep.src());
        let arr_ty = match src_ty.kind() {
            TypeKind::Pointer(base) => base,
            _ => panic!("GetElemPtr src must be a pointer"),
        };

        let elem_size = match arr_ty.kind() {
            TypeKind::Array(base, _) => calc_type_size(base),
            _ => panic!("GetElemPtr src must point to an array"),
        };

        if elem_size == 4 {
            self.push_inst(Inst::Slli(Reg::T1, Reg::T1, 2));
        } else {
            self.push_inst(Inst::Li(Reg::T2, elem_size as i32));
            self.push_inst(Inst::Mul(Reg::T1, Reg::T1, Reg::T2));
        }

        self.push_inst(Inst::Add(Reg::T0, Reg::T0, Reg::T1));
        self.store_from_reg(inst, Reg::T0)?;
        Ok(())
    }

    /// Generate asm code for a get pointer operation
    fn generate_get_ptr(&mut self, inst: Value, gp: &GetPtr) -> Result<()> {
        self.load_to_reg(gp.src(), Reg::T0)?;
        self.load_to_reg(gp.index(), Reg::T1)?;
        let src_ty = self.value_ty(gp.src());
        let ptr_ty = match src_ty.kind() {
            TypeKind::Pointer(base) => base,
            _ => panic!("GetPtr src must be a pointer"),
        };
        let elem_size = calc_type_size(ptr_ty);

        if elem_size == 4 {
            self.push_inst(Inst::Slli(Reg::T1, Reg::T1, 2));
        } else {
            self.push_inst(Inst::Li(Reg::T2, elem_size as i32));
            self.push_inst(Inst::Mul(Reg::T1, Reg::T1, Reg::T2));
        }

        self.push_inst(Inst::Add(Reg::T0, Reg::T0, Reg::T1));
        self.store_from_reg(inst, Reg::T0)?;
        Ok(())
    }

    /// Load a value into a register
    fn load_to_reg(&mut self, val: Value, reg: Reg) -> Result<()> {
        if !self.allocation.contains_key(&val) {
            if let Some(value_data) = self.func_data.dfg().values().get(&val) {
                match value_data.kind() {
                    ValueKind::Integer(i) => {
                        self.push_inst(Inst::Li(reg, i.value()));
                        return Ok(());
                    }
                    _ => {}
                }
            }

            let global_data = self.program.borrow_value(val);
            match global_data.kind() {
                ValueKind::GlobalAlloc(_) => {
                    let name = global_data
                        .name()
                        .as_deref()
                        .unwrap()
                        .trim_start_matches('@');
                    self.push_inst(Inst::La(reg, name.to_string()));
                }
                _ => panic!("Unknown value source {:?} {:?}", val, global_data.kind()),
            }
            return Ok(());
        }
        let loc = self.allocation[&val];
        match loc {
            // If value is allocated to a register, move it.
            Location::Reg(r) => {
                let r = Reg::from_index(r);
                if r != reg {
                    self.push_inst(Inst::Mv(reg, r));
                }
            }

            Location::Stack(offset) => {
                // Alloc 在栈里的 Location 代表它是"分配在哪"
                // 普通 Spill 在栈里的 Location 代表"值存在哪"
                let val_kind = self.func_data.dfg().value(val).kind();
                if let ValueKind::Alloc(_) = val_kind {
                    // Alloc: Load address (addi)
                    if offset < -2048 || offset > 2047 {
                        self.push_inst(Inst::Li(reg, offset));
                        self.push_inst(Inst::Add(reg, reg, Reg::Sp));
                    } else {
                        self.push_inst(Inst::Addi(reg, Reg::Sp, offset));
                    }
                } else {
                    // Spill / Args: load vals (lw)
                    self.safe_lw(reg, offset)?;
                }
            }
        }
        Ok(())
    }

    /// Store a register value into a value
    fn store_from_reg(&mut self, val: Value, reg: Reg) -> Result<()> {
        if !self.allocation.contains_key(&val) {
            // Global stores not handled here typically?
            let global_data = self.program.borrow_value(val);
            if let ValueKind::GlobalAlloc(_) = global_data.kind() {
                let name = global_data
                    .name()
                    .as_deref()
                    .unwrap()
                    .trim_start_matches('@');
                self.push_inst(Inst::La(Reg::T2, name.to_string()));
                self.push_inst(Inst::Sw(reg, Reg::T2, 0));
            }
            return Ok(());
        }
        let loc = self.allocation[&val];
        match loc {
            Location::Reg(r) => {
                let r = Reg::from_index(r);
                if r != reg {
                    self.push_inst(Inst::Mv(r, reg));
                }
            }
            Location::Stack(offset) => {
                self.safe_sw(reg, offset)?;
            }
        }
        Ok(())
    }

    /// Get the type of a value
    fn value_ty(&self, val: Value) -> Type {
        if self.func_data.dfg().values().contains_key(&val) {
            self.func_data.dfg().value(val).ty().clone()
        } else {
            self.program.borrow_value(val).ty().clone()
        }
    }

    /// Get the label of a basic block
    fn get_bb_label(&self, bb: BasicBlock) -> String {
        self.bb_labels[&bb].clone()
    }

    /// Helper to push an instruction to current block
    fn push_inst(&mut self, inst: Inst) {
        self.current_insts.push(inst);
    }

    /// Generate a unique edge block label
    fn new_edge_label(&mut self, prefix: &str) -> String {
        let id = self.edge_block_counter;
        self.edge_block_counter += 1;
        format!("{}_{}_{}", self.riscv_func.name, prefix, id)
    }

    /// Build an edge block for branches/jumps with arguments
    fn build_edge_block(
        &mut self,
        label: String,
        target_bb: BasicBlock,
        args: Vec<Value>,
    ) -> Result<RiscvBlock> {
        let mut saved = Vec::new();
        // Save current insts and start fresh for edge block
        std::mem::swap(&mut saved, &mut self.current_insts);
        self.current_insts.clear();

        self.emit_block_arg_moves(target_bb, &args)?;
        let target_label = self.get_bb_label(target_bb);
        self.push_inst(Inst::J(target_label));

        let mut insts = Vec::new();
        // Restore original insts for caller block
        std::mem::swap(&mut insts, &mut self.current_insts);
        std::mem::swap(&mut saved, &mut self.current_insts);
        Ok(RiscvBlock { label, insts })
    }

    /// Emit moves to set up block arguments before a branch/jump
    fn emit_block_arg_moves(&mut self, target_bb: BasicBlock, args: &[Value]) -> Result<()> {
        let params = self.func_data.dfg().bb(target_bb).params();
        if params.len() != args.len() {
            panic!("Mismatched block arg count");
        }

        let mut moves = Vec::new();
        for (&param, &arg) in params.iter().zip(args.iter()) {
            // dst must be a register or stack slot allocated for the block param
            let Some(&dst) = self.allocation.get(&param) else {
                panic!("Missing allocation for block param");
            };
            let dst_key = Self::loc_key(dst);
            let src = self.copy_src_of_value(arg);
            let src_key = match &src {
                CopySrc::Loc(loc) => Some(Self::loc_key(*loc)),
                _ => None,
            };
            // avoid redundant moves (src and dst are same reg/slot)
            if src_key == Some(dst_key) {
                continue;
            }
            moves.push(CopyMove {
                dst,
                dst_key,
                src,
                src_key,
            });
        }

        self.emit_parallel_moves(moves)
    }

    /// Emit moves for a set of parallel copies, handling cycles
    fn emit_parallel_moves(&mut self, mut moves: Vec<CopyMove>) -> Result<()> {
        while !moves.is_empty() {
            let mut src_keys = HashSet::new();
            for m in moves.iter() {
                if let Some(k) = m.src_key {
                    src_keys.insert(k);
                }
            }

            // cycle-free: exists a move whose dst is not src of any other move
            if let Some(idx) = moves.iter().position(|m| !src_keys.contains(&m.dst_key)) {
                let m = moves.remove(idx);
                self.emit_single_move(&m, Reg::T1)?;
                continue;
            }

            // read first to `t0`
            let first = moves.remove(0);
            let Some(mut hole) = first.src_key else {
                self.emit_single_move(&first, Reg::T1)?;
                continue;
            };
            self.read_copy_src_into(&first.src, Reg::T0)?;

            // `hole` is now the "empty slot" we need to fill,
            // starting with dst of `first`
            let dst0_key = first.dst_key;
            let dst0 = first.dst;

            // cycle: follow the chain until we get back to dst0
            while hole != dst0_key {
                let idx = moves
                    .iter()
                    .position(|m| m.dst_key == hole)
                    .expect("Broken parallel move cycle");
                let m = moves.remove(idx);
                self.emit_single_move(&m, Reg::T1)?;
                hole = m.src_key.expect("Broken parallel move cycle");
            }

            self.write_reg_to_loc(Reg::T0, dst0)?;
        }
        Ok(())
    }

    /// Emit a single move operation, handling different source/destination types
    fn emit_single_move(&mut self, m: &CopyMove, tmp: Reg) -> Result<()> {
        match (&m.src, m.dst) {
            (CopySrc::Loc(Location::Reg(rs)), Location::Reg(rd)) => {
                let rs = Reg::from_index(*rs);
                let rd = Reg::from_index(rd);
                if rs != rd {
                    self.push_inst(Inst::Mv(rd, rs));
                }
                Ok(())
            }
            (CopySrc::Loc(Location::Reg(rs)), Location::Stack(off)) => {
                self.safe_sw(Reg::from_index(*rs), off)
            }
            (CopySrc::Loc(Location::Stack(off)), Location::Reg(rd)) => {
                self.safe_lw(Reg::from_index(rd), *off)
            }
            (CopySrc::Loc(Location::Stack(src_off)), Location::Stack(dst_off)) => {
                self.safe_lw(tmp, *src_off)?;
                self.safe_sw(tmp, dst_off)
            }
            (CopySrc::Imm(v), Location::Reg(rd)) => {
                self.push_inst(Inst::Li(Reg::from_index(rd), *v));
                Ok(())
            }
            (CopySrc::Global(name), Location::Reg(rd)) => {
                self.push_inst(Inst::La(Reg::from_index(rd), name.clone()));
                Ok(())
            }
            (CopySrc::Imm(_), Location::Stack(_)) | (CopySrc::Global(_), Location::Stack(_)) => {
                self.read_copy_src_into(&m.src, tmp)?;
                self.write_reg_to_loc(tmp, m.dst)
            }
        }
    }

    /// Read a CopySrc into a register, handling different source types
    fn read_copy_src_into(&mut self, src: &CopySrc, rd: Reg) -> Result<()> {
        match src {
            CopySrc::Loc(Location::Reg(r)) => {
                let rs = Reg::from_index(*r);
                if rs != rd {
                    self.push_inst(Inst::Mv(rd, rs));
                }
                Ok(())
            }
            CopySrc::Loc(Location::Stack(offset)) => self.safe_lw(rd, *offset),
            CopySrc::Imm(v) => {
                self.push_inst(Inst::Li(rd, *v));
                Ok(())
            }
            CopySrc::Global(name) => {
                self.push_inst(Inst::La(rd, name.clone()));
                Ok(())
            }
        }
    }

    /// Write a register value to a location, handling different destination types
    fn write_reg_to_loc(&mut self, rs: Reg, dst: Location) -> Result<()> {
        match dst {
            Location::Reg(r) => {
                let rd = Reg::from_index(r);
                if rd != rs {
                    self.push_inst(Inst::Mv(rd, rs));
                }
                Ok(())
            }
            Location::Stack(offset) => self.safe_sw(rs, offset),
        }
    }

    /// Get the `CopySrc` of the given `val`
    fn copy_src_of_value(&self, val: Value) -> CopySrc {
        if let Some(loc) = self.allocation.get(&val) {
            return CopySrc::Loc(*loc);
        }
        if let Some(value_data) = self.func_data.dfg().values().get(&val) {
            if let ValueKind::Integer(i) = value_data.kind() {
                return CopySrc::Imm(i.value());
            }
        }
        let global_data = self.program.borrow_value(val);
        match global_data.kind() {
            ValueKind::GlobalAlloc(_) => {
                let name = global_data
                    .name()
                    .as_deref()
                    .unwrap()
                    .trim_start_matches('@')
                    .to_string();
                CopySrc::Global(name)
            }
            _ => panic!("Unknown value source {:?} {:?}", val, global_data.kind()),
        }
    }

    /// Convert a Location to a LocKey for parallel move tracking
    fn loc_key(loc: Location) -> LocKey {
        match loc {
            Location::Reg(r) => LocKey::Reg(r),
            Location::Stack(o) => LocKey::Stack(o),
        }
    }

    /// Safely load a value from stack into register with offset checking
    fn safe_lw(&mut self, rd: Reg, offset: i32) -> Result<()> {
        if offset < -2048 || offset > 2047 {
            self.push_inst(Inst::Li(Reg::T2, offset));
            self.push_inst(Inst::Add(Reg::T2, Reg::T2, Reg::Sp));
            self.push_inst(Inst::Lw(rd, Reg::T2, 0));
        } else {
            self.push_inst(Inst::Lw(rd, Reg::Sp, offset));
        }
        Ok(())
    }

    /// Safely store a register value into stack with offset checking
    fn safe_sw(&mut self, src: Reg, offset: i32) -> Result<()> {
        if offset < -2048 || offset > 2047 {
            self.push_inst(Inst::Li(Reg::T2, offset));
            self.push_inst(Inst::Add(Reg::T2, Reg::T2, Reg::Sp));
            self.push_inst(Inst::Sw(src, Reg::T2, 0));
        } else {
            self.push_inst(Inst::Sw(src, Reg::Sp, offset));
        }
        Ok(())
    }
}
