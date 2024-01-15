use crate::compiled_expr::CompiledExpr;
use crate::lexer::Lexer;
use crate::parser::{Opcode, Operation, OperationData, Parser};
use cranelift::frontend::FunctionBuilderContext;
use cranelift::prelude::FunctionBuilder;
use cranelift_codegen::control::ControlPlane;
use cranelift_codegen::dominator_tree::DominatorTree;
use cranelift_codegen::flowgraph::ControlFlowGraph;
use cranelift_codegen::ir::{
    types, AbiParam, ExtFuncData, ExternalName, Function, InstBuilder, Signature, UserExternalName,
    UserFuncName, Value,
};
use cranelift_codegen::isa::{CallConv, OwnedTargetIsa};
use cranelift_codegen::settings::Configurable;
use std::collections::BTreeMap;

pub struct Compiler {
    isa: OwnedTargetIsa,
    func_ctx: FunctionBuilderContext,
    expr_defs: ExprDefinitions,
}

impl Compiler {
    pub fn new_for_host() -> Self {
        let isa_builder = cranelift::codegen::isa::lookup(target_lexicon::HOST).unwrap();
        let mut b = cranelift::codegen::settings::builder();
        b.set("opt_level", "speed_and_size").unwrap();

        let isa = isa_builder
            .finish(cranelift::codegen::settings::Flags::new(b))
            .unwrap();

        Self {
            isa,
            func_ctx: FunctionBuilderContext::new(),
            expr_defs: ExprDefinitions::new(),
        }
    }

    pub fn assign_expr(&mut self, ident: &str, expr: CompiledExpr) -> ExprId {
        let id = self.expr_defs.declare_expr(ident, expr.signature.clone());
        self.expr_defs.define_expr(id, expr);
        id
    }

    pub fn compile(&mut self, expr: &str) -> anyhow::Result<CompiledExpr> {
        let lexer = Lexer::new(expr);
        let mut parser = Parser::from_tokens(lexer);
        let root = parser.parse()?;

        parser.optimize_tree(&self.expr_defs, root);

        let func = self.translate(parser, root)?;

        let cfg = ControlFlowGraph::with_function(&func);
        let domtree = DominatorTree::with_function(&func, &cfg);
        let mut ctrl_plane = ControlPlane::default();

        let compiled_expr = self
            .isa
            .compile_function(&func, &domtree, false, &mut ctrl_plane)?;

        let mut expr = CompiledExpr::new(func.signature.clone(), compiled_expr.code_buffer());

        for reloc in compiled_expr.buffer.relocs() {
            expr.perform_reloc(reloc, &self.expr_defs, &func);
        }

        expr.make_executable();

        Ok(expr)
    }

    fn translate(&mut self, parser: Parser, root: Operation) -> anyhow::Result<Function> {
        let (operations, strings, vars) = parser.into_parts();

        let mut sig = Signature::new(CallConv::Fast);

        for _ in &vars {
            sig.params.push(AbiParam::new(types::F64));
        }
        sig.returns.push(AbiParam::new(types::F64));

        let mut func = Function::with_name_signature(
            UserFuncName::User(UserExternalName {
                namespace: 0,
                index: 0, // TODO change this
            }),
            sig,
        );

        let mut builder = FunctionBuilder::new(&mut func, &mut self.func_ctx);

        let entry_block = builder.create_block();
        builder.seal_block(entry_block);
        builder.switch_to_block(entry_block);
        builder.append_block_params_for_function_params(entry_block);

        let retval = translate_op(&mut builder, &self.expr_defs, &strings, &operations, root);
        builder.ins().return_(&[retval]);

        builder.finalize();

        Ok(func)
    }
}

fn translate_op(
    builder: &mut FunctionBuilder,
    expr_defs: &ExprDefinitions,
    strings: &[&str],
    ops: &[OperationData],
    op: Operation,
) -> Value {
    match &ops[op.0] {
        OperationData::Constant(val) => builder.ins().f64const(*val),
        OperationData::Variable(idx) => {
            builder.block_params(builder.current_block().unwrap())[*idx]
        }
        OperationData::Binary { opcode, lhs, rhs } => {
            let lhs = translate_op(builder, expr_defs, strings, ops, *lhs);
            let rhs = translate_op(builder, expr_defs, strings, ops, *rhs);

            match opcode {
                Opcode::Add => builder.ins().fadd(lhs, rhs),
                Opcode::Sub => builder.ins().fsub(lhs, rhs),
                Opcode::Mul => builder.ins().fmul(lhs, rhs),
                Opcode::Div => builder.ins().fdiv(lhs, rhs),
            }
        }
        OperationData::Call { ident, args } => {
            let exprid = expr_defs.lookup_name(strings[*ident]).unwrap();

            let nameref = builder
                .func
                .declare_imported_user_function(UserExternalName {
                    namespace: 0,
                    index: exprid.0,
                });

            let sigref = builder.import_signature(expr_defs.get_expr_signature(exprid).clone());

            let funcref = builder.import_function(ExtFuncData {
                name: ExternalName::User(nameref),
                signature: sigref,
                colocated: false,
            });

            let args = args
                .iter()
                .map(|arg| translate_op(builder, expr_defs, strings, ops, *arg))
                .collect::<Vec<_>>();

            let inst = builder.ins().call(funcref, &args);
            builder.func.dfg.first_result(inst)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprId(pub(crate) u32);

pub struct ExprDefinitions {
    /// Maps human readable names to expression IDs
    names: BTreeMap<String, ExprId>,
    signatures: BTreeMap<ExprId, Signature>,
    compiled_exprs: BTreeMap<ExprId, CompiledExpr>,
}

impl ExprDefinitions {
    pub fn new() -> Self {
        Self {
            names: Default::default(),
            signatures: Default::default(),
            compiled_exprs: Default::default(),
        }
    }

    pub fn declare_expr(&mut self, name: &str, sig: Signature) -> ExprId {
        assert!(
            !self.names.contains_key(name),
            "attempted to redeclare expression"
        );

        let id = ExprId(self.signatures.len() as u32);
        self.names.insert(name.to_string(), id);
        self.signatures.insert(id, sig);
        id
    }

    pub fn define_expr(&mut self, id: ExprId, expr: CompiledExpr) {
        assert!(
            !self.compiled_exprs.contains_key(&id),
            "attempted to redefine expression"
        );
        self.compiled_exprs.insert(id, expr);
    }

    pub fn get_expr_signature(&self, id: ExprId) -> &Signature {
        self.signatures.get(&id).unwrap()
    }

    pub fn get_expr_address(&self, id: ExprId) -> *const u8 {
        let expr = self.compiled_exprs.get(&id).unwrap();
        expr.mmap.as_ptr()
    }

    pub fn lookup_name(&self, name: &str) -> Option<ExprId> {
        self.names.get(name).map(|n| *n)
    }
}
