use criterion::{black_box, criterion_group, criterion_main, Criterion};
use jitcalc::Compiler;

fn eval_simple(c: &mut Compiler) {
    let expr = c.compile("a * b * c * d * e");
    expr.call(&[1.0, 2.0, 3.0, 4.0, 5.0]);
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut compiler = Compiler::new_for_host();
    c.bench_function("jitcalc simple", |b| {
        b.iter(|| eval_simple(black_box(&mut compiler)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
