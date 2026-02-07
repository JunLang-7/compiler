use super::{LiveInterval, Location};
use koopa::ir::Value;
use std::collections::HashMap;

/// Linear Scan Register Allocator
pub struct LinearScan {
    callee_saved_regs: Vec<i32>,
    caller_saved_regs: Vec<i32>,
    spill_slot_count: i32,
}

impl LinearScan {
    /// Create a new Linear Scan Register Allocator
    pub fn new() -> Self {
        // allocatable regs are callee-saved regs: s0-s11
        let callee = vec![8, 9, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27];
        // TODO: we can use caller-saved regs: t3-t6 as well
        let caller = vec![28, 29, 30, 31];
        Self {
            callee_saved_regs: callee,
            caller_saved_regs: caller,
            spill_slot_count: 0,
        }
    }

    /// Run the Linear Scan Register Allocation Algorithm
    pub fn run(&mut self, list: &mut Vec<LiveInterval>) -> HashMap<Value, Location> {
        // pool of free regs
        let mut free_regs = [
            self.callee_saved_regs.clone(),
            self.caller_saved_regs.clone(),
        ]
        .concat();
        free_regs.reverse();
        let mut active = Vec::new();
        let mut allocation = HashMap::new();

        list.sort_by_key(|i| i.start);

        for i in list.iter() {
            self.expire_old_intervals(i, &mut active, &mut free_regs);
            let candidate = self.select_register(&free_regs, i);
            if let Some(reg) = candidate {
                free_regs.retain(|&r| r != reg);
                let mut interval = i.clone();
                interval.reg = Some(reg);
                allocation.insert(interval.value, Location::Reg(reg));
                active.push(interval);
            } else {
                self.spill_at_interval(i, &mut active, &mut allocation);
            }
        }

        allocation
    }

    /// Expire the old intervals
    fn expire_old_intervals(
        &self,
        i: &LiveInterval,
        active: &mut Vec<LiveInterval>,
        free_regs: &mut Vec<i32>,
    ) {
        active.retain(|j| {
            if j.end < i.start {
                free_regs.push(j.reg.unwrap());
                false // remove from activw
            } else {
                true
            }
        });
    }

    /// Spill logic when no registers are free
    fn spill_at_interval(
        &mut self,
        i: &LiveInterval,
        active: &mut Vec<LiveInterval>,
        allocation: &mut HashMap<Value, Location>,
    ) {
        let (max_idx, spill) = active
            .iter()
            .enumerate()
            .max_by_key(|(_, interval)| interval.end)
            .unwrap();
        if spill.end > i.end {
            let reg = spill.reg.unwrap();
            let slot = self.alloc_spill_slot();
            allocation.insert(spill.value, Location::Stack(slot));
            active.remove(max_idx);
            let mut interval = i.clone();
            interval.reg = Some(reg);
            allocation.insert(interval.value, Location::Reg(reg));
            active.push(interval);
        } else {
            let slot = self.alloc_spill_slot();
            allocation.insert(i.value, Location::Stack(slot));
        }
    }

    /// select a free register from pool of free registers
    fn select_register(&self, free_regs: &[i32], i: &LiveInterval) -> Option<i32> {
        // try reg hint first
        if let Some(hint) = i.reg_hint {
            if free_regs.contains(&hint) {
                let is_caller_saved = self.caller_saved_regs.contains(&hint);
                let valid = if i.cross_call { !is_caller_saved } else { true };
                if valid {
                    return Some(hint);
                }
            }
        }

        if i.cross_call {
            // must use callee-saved regs
            for &reg in free_regs.iter().rev() {
                if self.callee_saved_regs.contains(&reg) {
                    return Some(reg);
                }
            }
        } else {
            // prefer caller-saved regs
            for &reg in free_regs.iter().rev() {
                if self.caller_saved_regs.contains(&reg) {
                    return Some(reg);
                }
            }
            // fallback to callee-saved regs
            for &reg in free_regs.iter().rev() {
                if self.callee_saved_regs.contains(&reg) {
                    return Some(reg);
                }
            }
        }
        None
    }

    /// alloc a slot index
    fn alloc_spill_slot(&mut self) -> i32 {
        let slot = self.spill_slot_count;
        self.spill_slot_count += 4;
        slot
    }
}
