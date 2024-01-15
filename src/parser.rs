use crate::jit::ExprDefinitions;
use crate::lexer::{Lexer, Token};
use std::collections::BTreeMap;
use std::iter::Peekable;

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy)]
pub struct Operation(pub(crate) usize);

#[derive(Debug, Clone)]
pub enum OperationData {
    Constant(f64),
    Binary {
        opcode: Opcode,
        lhs: Operation,
        rhs: Operation,
    },
    Variable(usize),
    Call {
        ident: usize,
        args: Vec<Operation>,
    },
}

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
    operations: Vec<OperationData>,
    strings: Vec<&'a str>,
    vars: BTreeMap<&'a str, usize>,
}

impl<'a> Parser<'a> {
    pub fn from_tokens(tokens: Lexer<'a>) -> Self {
        Self {
            tokens: tokens.peekable(),
            operations: vec![],
            strings: vec![],
            vars: Default::default(),
        }
    }

    pub fn parse(&mut self) -> anyhow::Result<Operation> {
        self.parse_subadd()
    }

    pub fn into_parts(self) -> (Vec<OperationData>, Vec<&'a str>, BTreeMap<&'a str, usize>) {
        (self.operations, self.strings, self.vars)
    }

    pub fn optimize_tree(&mut self, expr_defs: &ExprDefinitions, op: Operation) {
        match self.operations[op.0] {
            OperationData::Variable(idx) if expr_defs.lookup_name(self.strings[idx]).is_some() => {
                self.vars.remove(self.strings[idx]);
                self.operations[op.0] = OperationData::Call {
                    ident: idx,
                    args: vec![],
                }
            }
            OperationData::Binary { opcode, lhs, rhs } => {
                self.optimize_tree(expr_defs, lhs);
                self.optimize_tree(expr_defs, rhs);

                let lhs = &self.operations[lhs.0];
                let rhs = &self.operations[rhs.0];

                if let (OperationData::Constant(lhs), OperationData::Constant(rhs)) = (lhs, rhs) {
                    let val = match opcode {
                        Opcode::Add => lhs + rhs,
                        Opcode::Sub => lhs - rhs,
                        Opcode::Mul => lhs * rhs,
                        Opcode::Div => lhs / rhs,
                    };
                    self.operations[op.0] = OperationData::Constant(val)
                }
            }
            _ => {}
        }
    }

    fn make_operation(&mut self, data: OperationData) -> Operation {
        let op = Operation(self.operations.len());
        self.operations.push(data);
        op
    }

    fn parse_subadd(&mut self) -> anyhow::Result<Operation> {
        let mut lhs = self.parse_call()?;

        while let Some(Ok(Token::Plus | Token::Minus)) = self.tokens.peek() {
            let token = self.tokens.next().unwrap()?;

            let opcode = match token {
                Token::Plus => Opcode::Add,
                Token::Minus => Opcode::Sub,
                _ => unreachable!(),
            };

            let data = OperationData::Binary {
                opcode,
                lhs,
                rhs: self.parse_call()?,
            };

            lhs = self.make_operation(data);
        }

        Ok(lhs)
    }

    fn parse_call(&mut self) -> anyhow::Result<Operation> {
        let lhs = self.parse_basic()?;
        let mut lhs = self.parse_muldiv(lhs)?;

        if let Some(Ok(Token::LParen)) = self.tokens.peek() {
            self.tokens.next().unwrap()?;

            let mut args = vec![];

            if !matches!(self.tokens.peek(), Some(Ok(Token::RParen))) {
                args.push(self.parse_subadd()?);

                while let Some(Ok(Token::Comma)) = self.tokens.peek() {
                    self.tokens.next().unwrap()?;
                    let op = self.parse_subadd()?;
                    args.push(op);
                }
            }

            assert_eq!(self.tokens.next().unwrap()?, Token::RParen);

            let OperationData::Variable(ident) = self.operations[lhs.0] else {
                return Err(anyhow::anyhow!("expected identifier"));
            };

            self.vars.remove(self.strings[ident]);

            let data = OperationData::Call { ident, args };
            lhs = self.make_operation(data);
        }

        lhs = self.parse_muldiv(lhs)?;

        Ok(lhs)
    }

    fn parse_muldiv(&mut self, mut lhs: Operation) -> anyhow::Result<Operation> {
        while let Some(Ok(Token::Star | Token::Slash)) = self.tokens.peek() {
            let token = self.tokens.next().unwrap()?;

            let opcode = match token {
                Token::Star => Opcode::Mul,
                Token::Slash => Opcode::Div,
                _ => unreachable!(),
            };

            let data = OperationData::Binary {
                opcode,
                lhs,
                rhs: self.parse_basic()?,
            };

            lhs = self.make_operation(data);
        }

        Ok(lhs)
    }

    fn parse_basic(&mut self) -> anyhow::Result<Operation> {
        let token = self.tokens.next().unwrap()?;

        match token {
            Token::Number(val) => Ok(self.make_operation(OperationData::Constant(val))),
            Token::Ident(ident) => {
                let id = *self.vars.entry(ident).or_insert_with(|| {
                    let id = self.strings.len();
                    self.strings.push(ident);
                    id
                });
                Ok(self.make_operation(OperationData::Variable(id)))
            }
            Token::LParen => {
                let op = self.parse_subadd()?;
                let token = self.tokens.next().unwrap()?;
                assert_eq!(token, Token::RParen, "expected ')'");
                Ok(op)
            }
            c => unreachable!("didn't expect char {:?}", c),
        }
    }

    pub fn debug_print(&self, op: Operation) {
        self.debug_print_inner(op);
        println!();
    }

    fn debug_print_inner(&self, op: Operation) {
        match &self.operations[op.0] {
            OperationData::Constant(val) => print!("{}", val),
            OperationData::Variable(ident) => print!("${}", self.strings[*ident]),
            OperationData::Call { ident, args } => {
                print!("${}(", self.strings[*ident]);
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        print!(", ");
                    }
                    self.debug_print_inner(*arg);
                }
                print!(")");
            }
            OperationData::Binary { opcode, lhs, rhs } => {
                print!("(");
                self.debug_print_inner(*lhs);
                match opcode {
                    Opcode::Add => print!(" + "),
                    Opcode::Sub => print!(" - "),
                    Opcode::Mul => print!(" * "),
                    Opcode::Div => print!(" / "),
                }
                self.debug_print_inner(*rhs);
                print!(")");
            }
        }
    }
}
