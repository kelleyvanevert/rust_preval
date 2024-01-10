use crate::types::*;

pub fn implement_stdlib(env: &mut Env) {
    env.add_global_fn_sig(
        "print",
        Params::from_iter([Ty::Any]),
        FnBody::from(|_: &mut _, args: Args| {
            println!("{}", args[0]);
            args[0]
        }),
    );

    env.add_global_fn_sig(
        "+",
        Params::from_iter([Ty::Int, Ty::Int]),
        FnBody::from(|_: &mut _, args: Args| {
            StackVal::Int(args[0].as_int() + args[1].as_int() + 100)
        }),
    );

    env.add_global_fn_sig(
        "+",
        Params::from_iter([Ty::Float, Ty::Float]),
        FnBody::from(|_: &mut _, args: Args| {
            StackVal::Float(args[0].as_float() + args[1].as_float() + 200.0)
        }),
    );

    env.add_global_fn_sig(
        "+",
        Params::from_iter([Ty::Int, Ty::Float]),
        FnBody::from(|_: &mut _, args: Args| {
            StackVal::Float(args[0].as_float() + args[1].as_float() + 300.0)
        }),
    );

    env.add_global_fn_sig(
        "-",
        Params::from_iter([Ty::Float, Ty::Float]),
        FnBody::from(|_: &mut _, args: Args| {
            StackVal::Float(args[0].as_float() - args[1].as_float())
        }),
    );

    env.add_global_fn_sig(
        "==",
        Params::from_iter([Ty::Any, Ty::Any]),
        FnBody::from(|_: &mut _, args: Args| StackVal::Bool(args[0] == args[1])),
    );

    env.add_global_fn_sig(
        "!=",
        Params::from_iter([Ty::Int, Ty::Int]),
        FnBody::from(|_: &mut _, args: Args| StackVal::Bool(args[0].as_int() != args[1].as_int())),
    );

    env.add_global_fn_sig(
        "!=",
        Params::from_iter([Ty::Float, Ty::Float]),
        FnBody::from(|_: &mut _, args: Args| {
            StackVal::Bool(args[0].as_float() != args[1].as_float())
        }),
    );

    env.add_global_fn_sig(
        ">",
        Params::from_iter([Ty::Float, Ty::Float]),
        FnBody::from(|_: &mut _, args: Args| {
            StackVal::Bool(args[0].as_float() > args[1].as_float())
        }),
    );
}
