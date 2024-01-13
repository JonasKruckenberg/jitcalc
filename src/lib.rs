#![cfg_attr(not(any(feature = "std", test)), no_std)]

extern crate alloc;

mod jit;
mod lexer;
mod mmap;
mod translator;

pub use crate::jit::{CompiledExpr, Compiler};

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple() {
        let mut jit = Compiler::new_for_host();

        let expr = jit.compile("3 + 7");
        assert_eq!(expr.call(&[]), 10.0);

        let expr = jit.compile("a + b");
        assert_eq!(expr.call(&[1.0, 2.0]), 3.0);
    }

    #[test]
    fn parentheses() {
        let mut jit = Compiler::new_for_host();

        let expr = jit.compile("3 - (7 + 6)");
        assert_eq!(expr.call(&[]), -10.0);

        let expr = jit.compile("a * (b - 6)");
        assert_eq!(expr.call(&[3.0, 8.0]), 6.0);
    }
}
