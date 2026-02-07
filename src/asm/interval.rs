use koopa::ir::Value;

/// Live Interval for a Value
#[derive(Debug, Clone)]
pub struct LiveInterval {
    pub value: Value,
    pub start: u32,
    pub end: u32,
    pub reg: Option<i32>,
    pub cross_call: bool,
}

/// Location of a value after register allocation
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Location {
    Reg(i32),
    Stack(i32),
}
