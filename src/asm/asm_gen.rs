use super::calc_type_size;
use super::interval::Location;
use super::liveness::LivenessAnalysis;
use super::lsra::LinearScan;
use koopa::ir::{
    BasicBlock, BinaryOp, FunctionData, Program, Type, TypeKind, Value, ValueKind, values::*,
};
use std::{
    collections::{HashMap, HashSet},
    io::{Result, Write},
};

/// Asm generator
pub struct AsmGen<'a> {
    writer: &'a mut dyn Write,
    program: &'a Program,
    func_data: &'a FunctionData,
    // inst -> allocation (Reg or Spilled/Stack)
    allocation: HashMap<Value, Location>,
    stack_size: i32,
    has_call: bool,
    used_callee_saved_regs: HashSet<i32>,
}

impl<'a> AsmGen<'a> {
    /// Create a new AsmGen instance
    pub fn new(
        writer: &'a mut dyn Write,
        program: &'a Program,
        func_data: &'a FunctionData,
    ) -> Self {
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
                if AsmGen::is_callee_saved(*r) {
                    used_callee_saved_regs.insert(*r);
                }
            }
        }

        let (stack_size, has_call) =
            Self::analyze_stack_frame(func_data, &mut allocation, &used_callee_saved_regs);

        Self {
            writer,
            program,
            func_data,
            allocation,
            stack_size,
            has_call,
            used_callee_saved_regs,
        }
    }

    fn is_callee_saved(r: i32) -> bool {
        // s0-s11 are callee saved (8, 9, 18-27)
        (r == 8) || (r == 9) || (r >= 18 && r <= 27)
    }

    fn get_reg_name(reg: i32) -> &'static str {
        match reg {
            0 => "x0",
            1 => "ra",
            2 => "sp",
            3 => "gp",
            4 => "tp",
            5 => "t0",
            6 => "t1",
            7 => "t2",
            8 => "s0",
            9 => "s1",
            10 => "a0",
            11 => "a1",
            12 => "a2",
            13 => "a3",
            14 => "a4",
            15 => "a5",
            16 => "a6",
            17 => "a7",
            18 => "s2",
            19 => "s3",
            20 => "s4",
            21 => "s5",
            22 => "s6",
            23 => "s7",
            24 => "s8",
            25 => "s9",
            26 => "s10",
            27 => "s11",
            28 => "t3",
            29 => "t4",
            30 => "t5",
            31 => "t6",
            _ => panic!("Invalid register {}", reg),
        }
    }

    /// Generate asm code for the function
    pub fn generate(&mut self) -> Result<()> {
        // prologue
        self.generate_prologue()?;

        for (&bb, node) in self.func_data.layout().bbs() {
            let label = self.get_bb_label(bb);
            writeln!(self.writer, "{}:", label)?;
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
        }
        Ok(())
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
                writeln!(self.writer, "\tli t0, -{}", self.stack_size)?;
                writeln!(self.writer, "\tadd sp, sp, t0")?;
            } else {
                writeln!(self.writer, "\taddi sp, sp, -{}", self.stack_size)?;
            }
            // Save RA
            if self.has_call {
                self.safe_sw("ra", self.stack_size - 4)?;
            }

            // Save Callee Saved Regs
            // Location = stack_size - 4 (if has_call) - 4 * (i + 1)
            let mut callee_save_offset = self.stack_size - 4;
            if self.has_call {
                callee_save_offset -= 4;
            }
            let mut saved_regs: Vec<i32> = self.used_callee_saved_regs.iter().cloned().collect();
            saved_regs.sort();
            for reg in saved_regs {
                let name = Self::get_reg_name(reg);
                self.safe_sw(name, callee_save_offset)?;
                callee_save_offset -= 4;
            }
        }

        // Save parameters to stack (original behavior)
        for (i, &param) in self.func_data.params().iter().enumerate() {
            if i < 8 {
                if let Some(Location::Stack(offset)) = self.allocation.get(&param) {
                    writeln!(self.writer, "\tsw a{}, {}(sp)", i, offset)?;
                }
            }
        }

        // Move Allocations for Parameters
        for (i, &param) in self.func_data.params().iter().enumerate() {
            if i < 8 {
                if let Some(Location::Reg(r)) = self.allocation.get(&param) {
                    let r_name = Self::get_reg_name(*r);
                    let a_name = format!("a{}", i);
                    if r_name != a_name {
                        writeln!(self.writer, "\tmv {}, {}", r_name, a_name)?;
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
            for reg in saved_regs {
                let name = Self::get_reg_name(reg);
                self.safe_lw(name, callee_save_offset)?;
                callee_save_offset -= 4;
            }

            if self.has_call {
                self.safe_lw("ra", self.stack_size - 4)?;
            }
            if self.stack_size < -2048 || self.stack_size > 2047 {
                writeln!(self.writer, "\tli t0, {}", self.stack_size)?;
                writeln!(self.writer, "\tadd sp, sp, t0")?;
            } else {
                writeln!(self.writer, "\taddi sp, sp, {}", self.stack_size)?;
            }
        }
        writeln!(self.writer, "\tret")?;
        writeln!(self.writer, "")?;
        Ok(())
    }

    /// Generate asm code for a return operation
    fn generate_return(&mut self, ret: &Return) -> Result<()> {
        if let Some(ret_val) = ret.value() {
            self.load_to_reg(ret_val, "a0")?;
        }
        // epilogue
        self.generate_epilogue()?;
        Ok(())
    }

    /// Generate asm code for a binary operation
    fn generate_binary(&mut self, inst: Value, bin: &Binary) -> Result<()> {
        // Load operands
        self.load_to_reg(bin.lhs(), "t0")?;
        self.load_to_reg(bin.rhs(), "t1")?;

        match bin.op() {
            BinaryOp::Add => writeln!(self.writer, "\tadd t0, t0, t1")?,
            BinaryOp::Sub => writeln!(self.writer, "\tsub t0, t0, t1")?,
            BinaryOp::Mul => writeln!(self.writer, "\tmul t0, t0, t1")?,
            BinaryOp::Div => writeln!(self.writer, "\tdiv t0, t0, t1")?,
            BinaryOp::Mod => writeln!(self.writer, "\trem t0, t0, t1")?,
            BinaryOp::And => writeln!(self.writer, "\tand t0, t0, t1")?,
            BinaryOp::Or => writeln!(self.writer, "\tor t0, t0, t1")?,
            BinaryOp::Xor => writeln!(self.writer, "\txor t0, t0, t1")?,
            BinaryOp::Shl => writeln!(self.writer, "\tsll t0, t0, t1")?,
            BinaryOp::Shr => writeln!(self.writer, "\tsrl t0, t0, t1")?,
            BinaryOp::Sar => writeln!(self.writer, "\tsra t0, t0, t1")?,
            BinaryOp::Eq => {
                writeln!(self.writer, "\txor t0, t0, t1")?;
                writeln!(self.writer, "\tseqz t0, t0")?;
            }
            BinaryOp::NotEq => {
                writeln!(self.writer, "\txor t0, t0, t1")?;
                writeln!(self.writer, "\tsnez t0, t0")?;
            }
            BinaryOp::Lt => writeln!(self.writer, "\tslt t0, t0, t1")?,
            BinaryOp::Gt => writeln!(self.writer, "\tsgt t0, t0, t1")?,
            BinaryOp::Le => {
                writeln!(self.writer, "\tsgt t0, t0, t1")?;
                writeln!(self.writer, "\tseqz t0, t0")?;
            }
            BinaryOp::Ge => {
                writeln!(self.writer, "\tslt t0, t0, t1")?;
                writeln!(self.writer, "\tseqz t0, t0")?;
            }
        }

        self.store_from_reg(inst, "t0")?;
        Ok(())
    }

    /// Generate asm code for a load operation
    fn generate_load(&mut self, inst: Value, load: &Load) -> Result<()> {
        self.load_to_reg(load.src(), "t0")?;
        writeln!(self.writer, "\tlw t0, 0(t0)")?;
        self.store_from_reg(inst, "t0")?;
        Ok(())
    }

    /// Generate asm code for a store operation
    fn generate_store(&mut self, store: &Store) -> Result<()> {
        self.load_to_reg(store.value(), "t0")?;
        self.load_to_reg(store.dest(), "t1")?;
        writeln!(self.writer, "\tsw t0, 0(t1)")?;
        Ok(())
    }

    /// Generate asm code for a branch operation
    fn generate_branch(&mut self, branch: &Branch) -> Result<()> {
        self.load_to_reg(branch.cond(), "t0")?;
        let true_label = self.get_bb_label(branch.true_bb());
        let false_label = self.get_bb_label(branch.false_bb());
        writeln!(self.writer, "\tbnez t0, {}", true_label)?;
        writeln!(self.writer, "\tj {}", false_label)?;
        Ok(())
    }

    /// Generate asm code for a jump operation
    fn generate_jump(&mut self, jmp: &Jump) -> Result<()> {
        let label = self.get_bb_label(jmp.target());
        writeln!(self.writer, "\tj {}", label)?;
        Ok(())
    }

    /// Generate asm code for a call operation
    fn generate_call(&mut self, inst: Value, call: &Call) -> Result<()> {
        for (i, &arg) in call.args().iter().enumerate() {
            if i < 8 {
                self.load_to_reg(arg, &format!("a{}", i))?;
            } else {
                self.load_to_reg(arg, "t0")?;
                writeln!(self.writer, "\tsw t0, {}(sp)", (i - 8) * 4)?;
            }
        }
        let callee = self.program.func(call.callee());
        let name = callee.name().trim_start_matches('@');
        writeln!(self.writer, "\tcall {}", name)?;

        if !self.func_data.dfg().value(inst).ty().is_unit() {
            self.store_from_reg(inst, "a0")?;
        }
        Ok(())
    }

    /// Generate asm code for a get element pointer operation
    fn generate_get_elem_ptr(&mut self, inst: Value, gep: &GetElemPtr) -> Result<()> {
        self.load_to_reg(gep.src(), "t0")?;
        self.load_to_reg(gep.index(), "t1")?;
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
            writeln!(self.writer, "\tslli t1, t1, 2")?;
        } else {
            writeln!(self.writer, "\tli t2, {}", elem_size)?;
            writeln!(self.writer, "\tmul t1, t1, t2")?;
        }

        writeln!(self.writer, "\tadd t0, t0, t1")?;
        self.store_from_reg(inst, "t0")?;
        Ok(())
    }

    /// Generate asm code for a get pointer operation
    fn generate_get_ptr(&mut self, inst: Value, gp: &GetPtr) -> Result<()> {
        self.load_to_reg(gp.src(), "t0")?;
        self.load_to_reg(gp.index(), "t1")?;
        let src_ty = self.value_ty(gp.src());
        let ptr_ty = match src_ty.kind() {
            TypeKind::Pointer(base) => base,
            _ => panic!("GetPtr src must be a pointer"),
        };
        let elem_size = calc_type_size(ptr_ty);

        if elem_size == 4 {
            writeln!(self.writer, "\tslli t1, t1, 2")?;
        } else {
            writeln!(self.writer, "\tli t2, {}", elem_size)?;
            writeln!(self.writer, "\tmul t1, t1, t2")?;
        }

        writeln!(self.writer, "\tadd t0, t0, t1")?;
        self.store_from_reg(inst, "t0")?;
        Ok(())
    }

    /// Load a value into a register
    fn load_to_reg(&mut self, val: Value, reg: &str) -> Result<()> {
        if !self.allocation.contains_key(&val) {
            if let Some(value_data) = self.func_data.dfg().values().get(&val) {
                match value_data.kind() {
                    ValueKind::Integer(i) => {
                        writeln!(self.writer, "\tli {}, {}", reg, i.value())?;
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
                    writeln!(self.writer, "\tla {}, {}", reg, name)?;
                }
                _ => panic!("Unknown value source {:?} {:?}", val, global_data.kind()),
            }
            return Ok(());
        }
        let loc = self.allocation[&val];
        match loc {
            // If value is allocated to a register, move it.
            Location::Reg(r) => {
                let r_name = Self::get_reg_name(r);
                if r_name != reg {
                    writeln!(self.writer, "\tmv {}, {}", reg, r_name)?;
                }
            }

            Location::Stack(offset) => {
                // Alloc 在栈里的 Location 代表它是"分配在哪"
                // 普通 Spill 在栈里的 Location 代表"值存在哪"
                let val_kind = self.func_data.dfg().value(val).kind();
                if let ValueKind::Alloc(_) = val_kind {
                    // Alloc: Load address (addi)
                    if offset < -2048 || offset > 2047 {
                        writeln!(self.writer, "\tli {}, {}", reg, offset)?;
                        writeln!(self.writer, "\tadd {}, {}, sp", reg, reg)?;
                    } else {
                        writeln!(self.writer, "\taddi {}, sp, {}", reg, offset)?;
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
    fn store_from_reg(&mut self, val: Value, reg: &str) -> Result<()> {
        if !self.allocation.contains_key(&val) {
            // Global stores not handled here typically?
            let global_data = self.program.borrow_value(val);
            if let ValueKind::GlobalAlloc(_) = global_data.kind() {
                let name = global_data
                    .name()
                    .as_deref()
                    .unwrap()
                    .trim_start_matches('@');
                writeln!(self.writer, "\tla t2, {}", name)?;
                writeln!(self.writer, "\tsw {}, 0(t2)", reg)?;
            }
        }
        let loc = self.allocation[&val];
        match loc {
            Location::Reg(r) => {
                let r_name = Self::get_reg_name(r);
                if r_name != reg {
                    writeln!(self.writer, "\tmv {}, {}", r_name, reg)?;
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
        let bb_data = self.func_data.dfg().bb(bb);
        if let Some(bb_name) = bb_data.name() {
            let func_name = self.func_data.name().trim_start_matches('@');
            format!("{}_{}", func_name, bb_name.trim_start_matches("%"))
        } else {
            "unknown".to_string()
        }
    }

    /// Safely load a value from stack into register with offset checking
    fn safe_lw(&mut self, src: &str, offset: i32) -> Result<()> {
        if offset < -2048 || offset > 2047 {
            writeln!(self.writer, "\tli t2, {}", offset)?;
            writeln!(self.writer, "\tadd t2, t2, sp")?;
            writeln!(self.writer, "\tlw {}, 0(t2)", src)?;
        } else {
            writeln!(self.writer, "\tlw {}, {}(sp)", src, offset)?;
        }
        Ok(())
    }

    /// Safely store a register value into stack with offset checking
    fn safe_sw(&mut self, src: &str, offset: i32) -> Result<()> {
        if offset < -2048 || offset > 2047 {
            writeln!(self.writer, "\tli t2, {}", offset)?;
            writeln!(self.writer, "\tadd t2, t2, sp")?;
            writeln!(self.writer, "\tsw {}, 0(t2)", src)?;
        } else {
            writeln!(self.writer, "\tsw {}, {}(sp)", src, offset)?;
        }
        Ok(())
    }
}
