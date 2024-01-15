#![cfg_attr(not(any(feature = "std", test)), no_std)]

extern crate alloc;
extern crate core;

mod builtins;
mod compiled_expr;
mod jit;
mod lexer;
mod mmap;
mod parser;

pub use compiled_expr::CompiledExpr;
pub use jit::Compiler;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn lex() {
        let mut jit = Compiler::new_for_host();

        let expr = jit.compile("a + b - (4.2 * 0) / 2").unwrap();
        assert_eq!(expr.eval(&[1.0, 2.0]), 3.0);
    }

    #[test]
    fn reff() {
        let mut jit = Compiler::new_for_host();

        let a = jit.compile("2 + 3").unwrap();
        jit.assign_expr("a", a);

        let b = jit.compile("a * 6").unwrap();
        assert_eq!(b.eval(&[]), 30.0);
    }

    #[test]
    fn args() {
        let mut jit = Compiler::new_for_host();

        let plus_one = jit.compile("a + 1").unwrap();
        jit.assign_expr("plus_one", plus_one);

        let b = jit.compile("plus_one(3) * 6").unwrap();
        assert_eq!(b.eval(&[]), 24.0);
    }

    #[test]
    fn builtins() {
        let mut jit = Compiler::new_for_host();

        let expr = jit.compile("sin(1)").unwrap();
        assert_eq!(expr.eval(&[]), 0.8414709848078965);

        let expr = jit.compile("cos(1)").unwrap();
        assert_eq!(expr.eval(&[]), 0.5403023058681398);

        let expr = jit.compile("tan(1)").unwrap();
        assert_eq!(expr.eval(&[]), 1.557407724654902);
    }
}
