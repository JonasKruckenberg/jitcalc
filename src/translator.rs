use crate::jit::{ExprDefinitions, ExprId};
use crate::lexer::{Lexer, Token};
use alloc::collections::BTreeMap;
use alloc::vec;
use alloc::vec::Vec;
use core::cmp::Ordering;
use core::iter::Peekable;
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_codegen::ir::{
    types, AbiParam, ExtFuncData, ExternalName, Function, Inst, InstBuilder, InstructionData,
    Opcode, Signature, UserExternalName, UserFuncName, Value,
};
use cranelift_codegen::isa::CallConv;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    LParen,
    Call(ExprId),
}

impl Operator {
    fn precedence(&self) -> usize {
        match self {
            Operator::Call(_) => 4,
            Operator::Mul | Operator::Div => 3,
            Operator::Add | Operator::Sub => 2,
            Operator::LParen => 0,
        }
    }
}

impl PartialOrd for Operator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.precedence().partial_cmp(&other.precedence())
    }
}

pub struct Translator<'a> {
    tokens: Peekable<Lexer<'a>>,
    value_stack: Vec<Value>,
    op_stack: Vec<Operator>,
    params: BTreeMap<&'a str, Value>,

    expr_defs: &'a ExprDefinitions,
}

impl<'a> Translator<'a> {
    pub fn from_tokens(tokens: Lexer<'a>, expr_defs: &'a ExprDefinitions) -> Self {
        Self {
            tokens: tokens.peekable(),
            value_stack: vec![],
            op_stack: vec![],
            params: Default::default(),
            expr_defs,
        }
    }

    pub fn num_params(&self) -> usize {
        self.params.len()
    }

    pub fn translate(&mut self, ctx: &mut FunctionBuilderContext) -> anyhow::Result<Function> {
        let mut func = Function::with_name_signature(
            UserFuncName::User(UserExternalName {
                namespace: 0,
                index: 0,
            }),
            Signature::new(CallConv::Fast),
        );

        let mut builder = FunctionBuilder::new(&mut func, ctx);

        let entry_block = builder.create_block();
        builder.seal_block(entry_block);
        builder.switch_to_block(entry_block);
        builder.append_block_params_for_function_params(entry_block);

        while let Some(token) = self.tokens.next() {
            self.process_token(&mut builder, token?);
        }

        while let Some(op) = self.op_stack.pop() {
            self.process_op(&mut builder, op);
        }

        assert!(self.op_stack.is_empty());
        assert_eq!(self.value_stack.len(), 1);

        let retval = self.value_stack.pop().unwrap();
        let retval_ty = builder.func.dfg.value_type(retval);
        builder.ins().return_(&[retval]);

        builder
            .func
            .signature
            .returns
            .push(AbiParam::new(retval_ty));

        builder.finalize();

        Ok(func)
    }

    fn peek_op(&self) -> Option<Operator> {
        self.op_stack.last().map(|op| *op)
    }

    fn process_op(&mut self, builder: &mut FunctionBuilder, op: Operator) {
        let val = if let Operator::Call(exprid) = op {
            let reff = builder
                .func
                .declare_imported_user_function(UserExternalName {
                    namespace: 0,
                    index: exprid.as_bits(),
                });
            let signature =
                builder.import_signature(self.expr_defs.get_expr_signature(exprid).clone());

            let funcref = builder.import_function(ExtFuncData {
                name: ExternalName::User(reff),
                signature,
                colocated: false,
            });

            let num_args = self.expr_defs.get_expr_signature(exprid).params.len();
            let args = self
                .value_stack
                .split_off(self.value_stack.len() - num_args);

            let inst = builder.ins().call(funcref, &args);
            builder.func.dfg.first_result(inst)
        } else {
            let rhs = self.value_stack.pop().unwrap();
            let lhs = self.value_stack.pop().unwrap();

            if let (Some((lhs, i1)), Some((rhs, i2))) =
                (maybe_imm(builder, lhs), maybe_imm(builder, rhs))
            {
                builder.func.layout.remove_inst(i1);
                builder.func.layout.remove_inst(i2);

                match op {
                    Operator::Add => builder.ins().f64const(lhs + rhs),
                    Operator::Sub => builder.ins().f64const(lhs - rhs),
                    Operator::Mul => builder.ins().f64const(lhs * rhs),
                    Operator::Div => builder.ins().f64const(lhs / rhs),
                    Operator::Call(_) | Operator::LParen => {
                        unreachable!()
                    }
                }
            } else {
                match op {
                    Operator::Add => builder.ins().fadd(lhs, rhs),
                    Operator::Sub => builder.ins().fsub(lhs, rhs),
                    Operator::Mul => builder.ins().fmul(lhs, rhs),
                    Operator::Div => builder.ins().fdiv(lhs, rhs),
                    Operator::Call(exprid) => {
                        let reff = builder
                            .func
                            .declare_imported_user_function(UserExternalName {
                                namespace: 0,
                                index: exprid.as_bits(),
                            });
                        let signature = builder
                            .import_signature(self.expr_defs.get_expr_signature(exprid).clone());

                        let funcref = builder.import_function(ExtFuncData {
                            name: ExternalName::User(reff),
                            signature,
                            colocated: false,
                        });

                        let num_args = self.expr_defs.get_expr_signature(exprid).params.len();
                        let args = self
                            .value_stack
                            .split_off(self.value_stack.len() - num_args);

                        let inst = builder.ins().call(funcref, &args);
                        builder.func.dfg.first_result(inst)
                    }
                    Operator::LParen => {
                        unreachable!()
                    }
                }
            }
        };

        self.value_stack.push(val);
    }

    fn process_token(&mut self, builder: &mut FunctionBuilder, token: Token<'a>) {
        match token {
            Token::Number(val) => {
                let val = builder.ins().f64const(val);
                self.value_stack.push(val);
            }
            Token::Ident(ident) if self.expr_defs.lookup_name(ident).is_some() => {
                let exprid = self.expr_defs.lookup_name(ident).unwrap();

                self.op_stack.push(Operator::Call(exprid));
            }
            Token::Ident(ident) => {
                let val = *self.params.entry(ident).or_insert_with(|| {
                    builder
                        .func
                        .signature
                        .params
                        .push(AbiParam::new(types::F64));

                    builder.append_block_param(builder.current_block().unwrap(), types::F64)
                });

                self.value_stack.push(val);
            }
            Token::LParen => self.op_stack.push(Operator::LParen),
            Token::RParen => {
                while let Some(op) = self.op_stack.pop() {
                    if Operator::LParen == op {
                        break;
                    } else {
                        self.process_op(builder, op);
                    }
                }
            }
            _ => {
                let op = match token {
                    Token::Plus => Operator::Add,
                    Token::Minus => Operator::Sub,
                    Token::Star => Operator::Mul,
                    Token::Slash => Operator::Div,
                    _ => unreachable!(),
                };

                if self.peek_op().map(|other| op < other).unwrap_or_default() {
                    let op = self.op_stack.pop().unwrap();
                    self.process_op(builder, op);
                }

                self.op_stack.push(op);
            }
        }
    }
}

fn maybe_imm(builder: &mut FunctionBuilder, value: Value) -> Option<(f64, Inst)> {
    let def = builder.func.dfg.value_def(value);

    let Some(inst) = def.inst() else {
        return None;
    };
    let data = builder.func.dfg.insts[inst];

    if let InstructionData::UnaryIeee64 {
        opcode: Opcode::F64const,
        imm,
    } = data
    {
        Some((imm.as_f64(), def.unwrap_inst()))
    } else {
        None
    }
}
