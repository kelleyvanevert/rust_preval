use crate::{
    parse::parse_doc,
    types::{Args, Ctx, StackVal},
};

mod parse;
mod signature_map;
mod stdlib;
mod types;

fn main() {
    let mut ctx = Ctx::with_stdlib();

    let r = ctx.call_by_name(
        "+",
        Args::from_iter([StackVal::Float(2.0), StackVal::Float(3.0)]),
    );
    println!(" -> {:?}", r);

    let r = ctx.call_by_name("+", Args::from_iter([StackVal::Int(2), StackVal::Int(3)]));
    println!(" -> {:?}", r);

    let r = ctx.call_by_name(
        "+",
        Args::from_iter([StackVal::Int(2), StackVal::Float(3.0)]),
    );
    println!(" -> {:?}", r);

    let r = ctx.call_by_name(
        "+",
        Args::from_iter([StackVal::Float(2.0), StackVal::Int(3)]),
    );
    println!(" -> {:?}", r);

    let f = parse_doc(
        "{
            fn hi(a: int, b: float) {
                a + b + 100000
            };

            fn hi(a: float, b: float) {
                a + b - 100000
            };

            i = 3;

            while i != 0 {
                print(i);
                print(hi(i, 4));
                i = i - 1;
            };

            42
        }",
    )
    .unwrap();

    println!("Executing doc...");
    println!(" -> {}", f.eval(&mut ctx));
}
