/// Register Allocation Module, using Linear Scan Register Allocation
mod liveness;
mod lsra;

use koopa::ir::Value;
/// Live Interval for a Value
#[derive(Debug, Clone)]
pub struct LiveInterval {
    pub value: Value,
    pub start: u32,
    pub end: u32,
    pub reg: Option<i32>,
    /// Whether this interval crosses a function call
    pub cross_call: bool,
    /// ABI-based register hint
    pub reg_hint: Option<i32>,
}

/// Location of a value after register allocation
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Location {
    Reg(i32),
    Stack(i32),
}

pub use liveness::LivenessAnalysis;
pub use lsra::LinearScan;
