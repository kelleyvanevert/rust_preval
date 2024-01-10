use crate::{
    parse::parse_doc,
    stdlib::implement_stdlib,
    types::{Args, Ctx, StackVal},
};

mod parse;
mod runtime;
mod signature_map;
mod stdlib;
mod types;

fn main() {
    let mut ctx = Ctx::new();
    implement_stdlib(&mut ctx.env);

    println!("ops = {:?}", ctx.env.ops);

    let r = ctx.call(
        "+",
        Args::from_iter([StackVal::Float(2.0), StackVal::Float(3.0)]),
    );
    println!(" -> {:?}", r);

    let r = ctx.call("+", Args::from_iter([StackVal::Int(2), StackVal::Int(3)]));
    println!(" -> {:?}", r);

    let r = ctx.call(
        "+",
        Args::from_iter([StackVal::Int(2), StackVal::Float(3.0)]),
    );
    println!(" -> {:?}", r);

    let r = ctx.call(
        "+",
        Args::from_iter([StackVal::Float(2.0), StackVal::Int(3)]),
    );
    println!(" -> {:?}", r);

    let f = parse_doc("{ i = 3; while i { print i; i = i - 1 } }").unwrap();

    println!("Executing doc...");
    println!(" -> {}", f.eval(&mut ctx));
}
