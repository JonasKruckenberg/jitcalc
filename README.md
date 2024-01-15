# JitCalc

JitCalc is an arithmetic expression evaluator, you can give it an expression and it will compute!
On the surface this might seem trivial and boring, however JitCalc has an ass up its sleeve: **All expressions are compiled into machine code**

This is a relatively simple toy project that aims to demonstrate how to parse & compile a language using [`cranelift`](https://crates.io/crates/cranelift).

## Usage

JitCalc is currently just a Rust crate, add it to your dependencies like so

```toml
[dependencies]
jitcalc = { git = "https://github.com/JonasKruckenberg/jitcalc" }
```

and then use it in your Rust code:

```rust
use jitcalc::Compiler;

fn main() {
    let compiler = Compiler::new_for_host();

    let expr = compiler.compile("a + b");
    
    let result = expr.call(&[10.0, 5.0]);
    
    assert_eq!(result, 15.0);
}
```

## Supported Grammar

JitCalc supports a simple grammar in the shape that you probably expect from arithmetic expressions:

- Numbers: With or without decimal point `1` or `1.0`
- Variables: One or more alphanumeric characters. Variables must not begin with a number.
- Addition: `+`
- Subtraction: `-`
- Multiplication: `*`
- Division: `/`
- Parentheses: `(`, `)`

Here are a couple example expressions:
- `4 * (6 - 5)`
- `a + b`
- `(7 + 9) / v1`

## Assignments

You can also assign identifiers to expressions and then use them from within other expressions:

```rust
use jitcalc::Compiler;

fn main() {
    let mut jit = Compiler::new_for_host();

    let a = jit.compile("2 + 3").unwrap();
    jit.assign_expr("a", a);

    let b = jit.compile("a * 6").unwrap();
    assert_eq!(b.eval(&[]), 30.0);
}
```

Expressions that have **no** free variables can be used just like identifiers, while expressions that do require arguments be passed in parentheses:


```rust
use jitcalc::Compiler;

fn main() {
    let mut jit = Compiler::new_for_host();

    let a = jit.compile("a + 1").unwrap();
    jit.assign_expr("plus_one", a);

    let b = jit.compile("plus_one(3) * 6").unwrap();
    assert_eq!(b.eval(&[]), 24.0);
}
```

## Builtin Functions

JitCalc has builtin supports for a number of functions:

- `min(x, y)`
- `max(x, y)`
- `abs(x)`
- `ceil(x)`
- `floor(x)`
- `sin(x)`
- `asin(x)`
- `cos(x)`
- `acos(x)`
- `tan(x)`
- `atan(x)`
- `log(x, base)`
- `log2(x)`
- `log10(x)`

## Contributing

There are many things not yet implemented, such as functions e.g. `sin`/`tan`/`sqrt` etc.
I would also love to build a REPL using this and be able to reference expressions.

If you have any ideas for this project: PRs are welcome!

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>