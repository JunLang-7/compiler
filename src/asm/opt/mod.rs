mod cfg;
mod peephole;

pub use cfg::simplify_control_flow;
pub use peephole::peephole_optimize;
