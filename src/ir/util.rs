use super::{Eval, GenContext};
use crate::ast::{ConstExp, ConstInitVal, Exp, InitVal};
use crate::ir::generate_exp;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, Type, TypeKind, Value};

/// Calculate array type from base type and dimensions
pub fn get_array_type(ctx: &mut GenContext, base_ty: Type, dims: &[ConstExp]) -> Type {
    let mut ty = base_ty;
    for dim in dims.iter().rev() {
        let len = dim.exp.evaluate(ctx.symbol_table());
        ty = Type::get_array(ty, len as usize);
    }
    ty
}

/// Generate pointer calculation for array access
pub fn generate_ptr(
    ctx: &mut GenContext,
    bb: &mut BasicBlock,
    base: Value,
    indices: &[Exp],
    is_ptr: bool, // true if base is a pointer (function param), false if array (alloc)
) -> Value {
    let mut ptr = base;
    for (i, index) in indices.iter().enumerate() {
        let idx_val = generate_exp(ctx, bb, index);

        if i == 0 && is_ptr {
            // First dimension of a pointer parameter: use getptr
            let getptr = ctx.func_mut().dfg_mut().new_value().get_ptr(ptr, idx_val);
            ctx.func_mut()
                .layout_mut()
                .bb_mut(*bb)
                .insts_mut()
                .push_key_back(getptr)
                .expect("failed to add getptr instruction");
            ptr = getptr;
        } else {
            // Array access: use getelemptr
            let getelemptr = ctx
                .func_mut()
                .dfg_mut()
                .new_value()
                .get_elem_ptr(ptr, idx_val);
            ctx.func_mut()
                .layout_mut()
                .bb_mut(*bb)
                .insts_mut()
                .push_key_back(getelemptr)
                .expect("failed to add getelemptr instruction");
            ptr = getelemptr;
        }
    }
    ptr
}

/// Struct to help flatten multi-dimensional arrays
pub struct ArrayFlattener {
    /// strides for each dimension
    pub strides: Vec<usize>,
    /// Total capacity of the array
    pub total_capacity: usize,
}

impl ArrayFlattener {
    /// Create a new ArrayFlattener given the dimensions
    pub fn new(dims: &[i32]) -> Self {
        let mut strides = Vec::new();
        let mut current_size = 1;

        for &dim in dims.iter().rev() {
            current_size *= dim as usize;
            strides.push(current_size);
        }
        // Remove the last stride as it's not needed
        if !strides.is_empty() {
            strides.pop();
        }
        // Reverse to maintain the correct order
        strides.reverse();
        Self {
            strides,
            total_capacity: current_size,
        }
    }
}

/// Flatten the given initialization values for variables
pub fn flatten_var(ctx: &mut GenContext, init: &InitVal, dims: &[i32]) -> Vec<Exp> {
    let flattener = ArrayFlattener::new(dims);
    let mut result = Vec::new();
    flatten_var_helper(ctx, init, &flattener, &mut result);
    while result.len() < flattener.total_capacity {
        result.push(Exp::number(0));
    }
    result
}

fn flatten_var_helper(
    ctx: &mut GenContext,
    init: &InitVal,
    flattener: &ArrayFlattener,
    result: &mut Vec<Exp>,
) {
    match init {
        InitVal::Exp(exp) => {
            result.push(exp.clone());
        }
        InitVal::List(list) => {
            let current_pos = result.len();
            let mut aligned_stride = 0;
            // try to find the max stride that can divide current_pos
            for &stride in &flattener.strides {
                if current_pos % stride == 0 {
                    aligned_stride = stride;
                    break;
                }
            }

            for item in list {
                flatten_var_helper(ctx, item, flattener, result);
            }

            // Padding with zeros if necessary
            if aligned_stride > 0 {
                let target_len =
                    (result.len() + aligned_stride - 1) / aligned_stride * aligned_stride;
                while result.len() < target_len && result.len() < flattener.total_capacity {
                    result.push(Exp::number(0));
                }
            }
        }
    }
}

/// Flatten the given initialization values for constants
pub fn flatten_const(ctx: &mut GenContext, init: &ConstInitVal, dims: &[i32]) -> Vec<i32> {
    let flattener = ArrayFlattener::new(dims);
    let mut result = Vec::new();
    flatten_const_helper(ctx, init, &flattener, &mut result);
    while result.len() < flattener.total_capacity {
        result.push(0);
    }
    result
}

fn flatten_const_helper(
    ctx: &mut GenContext,
    init: &ConstInitVal,
    flattener: &ArrayFlattener,
    result: &mut Vec<i32>,
) {
    match init {
        ConstInitVal::Exp(const_exp) => {
            let val = const_exp.exp.evaluate(ctx.symbol_table());
            result.push(val);
        }
        ConstInitVal::List(list) => {
            let current_pos = result.len();
            let mut aligned_stride = 0;
            // try to find the max stride that can divide current_pos
            for &stride in &flattener.strides {
                if current_pos % stride == 0 {
                    aligned_stride = stride;
                    break;
                }
            }

            for item in list {
                flatten_const_helper(ctx, item, flattener, result);
            }

            // Padding with zeros if necessary
            if aligned_stride > 0 {
                let target_len =
                    (result.len() + aligned_stride - 1) / aligned_stride * aligned_stride;
                while result.len() < target_len && result.len() < flattener.total_capacity {
                    result.push(0);
                }
            }
        }
    }
}

/// Generate initialization value for global variable
pub fn generate_global_init_val(ctx: &mut GenContext, init: &InitVal, ty: &Type) -> Value {
    let dims = get_type_dims(ty);
    let flat_exps = flatten_var(ctx, init, &dims);
    let mut flat_vals = Vec::new();
    for exp in flat_exps {
        flat_vals.push(exp.evaluate(ctx.symbol_table()));
    }
    build_aggregate_value(ctx, &flat_vals, &dims)
}

/// Generate initialization value for global constant
pub fn generate_global_const_init_val(
    ctx: &mut GenContext,
    init: &ConstInitVal,
    ty: &Type,
) -> Value {
    let dims = get_type_dims(ty);
    let flat_vals = flatten_const(ctx, init, &dims);
    build_aggregate_value(ctx, &flat_vals, &dims)
}

fn get_type_dims(mut ty: &Type) -> Vec<i32> {
    let mut dims = Vec::new();
    while let TypeKind::Array(base, len) = ty.kind() {
        dims.push(*len as i32);
        ty = base;
    }
    dims
}

/// Build aggregate value from flattened values and dimensions
pub fn build_aggregate_value(ctx: &mut GenContext, flat_vals: &[i32], dims: &[i32]) -> Value {
    if dims.is_empty() {
        return ctx.program.new_value().integer(flat_vals[0]);
    }

    let cur_dim = dims[0] as usize;
    let sub_dims = &dims[1..];
    let chunk_size = flat_vals.len() / cur_dim;
    let mut elems = Vec::new();

    for i in 0..cur_dim {
        let start = i * chunk_size;
        let end = start + chunk_size;
        elems.push(build_aggregate_value(ctx, &flat_vals[start..end], sub_dims));
    }
    ctx.program.new_value().aggregate(elems)
}

/// Generate zero initialization for global variable
pub fn generate_global_zero_init(ctx: &mut GenContext, ty: &Type) -> Value {
    ctx.program.new_value().zero_init(ty.clone())
}
