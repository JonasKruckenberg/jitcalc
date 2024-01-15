use crate::jit::ExprDefinitions;
use cranelift_codegen::ir::{types, AbiParam, Signature};
use cranelift_codegen::isa::CallConv;

fn create_signature(num_params: usize) -> Signature {
    let mut sig = Signature::new(CallConv::Fast);
    sig.returns.push(AbiParam::new(types::F64));
    for _ in 0..num_params {
        sig.params.push(AbiParam::new(types::F64))
    }
    sig
}

pub fn declare_builtins(exprs_defs: &mut ExprDefinitions) {
    let id = exprs_defs.declare_expr("sin", create_signature(1));
    exprs_defs.define_builtin(id, f64::sin as *const u8);

    let id = exprs_defs.declare_expr("asin", create_signature(1));
    exprs_defs.define_builtin(id, f64::asin as *const u8);

    let id = exprs_defs.declare_expr("cos", create_signature(1));
    exprs_defs.define_builtin(id, f64::cos as *const u8);

    let id = exprs_defs.declare_expr("acos", create_signature(1));
    exprs_defs.define_builtin(id, f64::acos as *const u8);

    let id = exprs_defs.declare_expr("tan", create_signature(1));
    exprs_defs.define_builtin(id, f64::tan as *const u8);

    let id = exprs_defs.declare_expr("atan", create_signature(1));
    exprs_defs.define_builtin(id, f64::atan as *const u8);

    let id = exprs_defs.declare_expr("trunc", create_signature(1));
    exprs_defs.define_builtin(id, f64::trunc as *const u8);

    let id = exprs_defs.declare_expr("log", create_signature(2));
    exprs_defs.define_builtin(id, f64::log as *const u8);

    let id = exprs_defs.declare_expr("log2", create_signature(1));
    exprs_defs.define_builtin(id, f64::log2 as *const u8);

    let id = exprs_defs.declare_expr("log10", create_signature(1));
    exprs_defs.define_builtin(id, f64::log10 as *const u8);
}
