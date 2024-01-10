use crate::types::*;

pub fn implement_stdlib(env: &mut Env) {
    env.ops.entry("+".to_string()).or_default().insert(
        Params::from_iter([Ty::Int, Ty::Int]),
        FnDef::from(|_: &mut _, args: Args| {
            StackVal::Int(args[0].as_int() + args[1].as_int() + 100)
        }),
    );

    env.ops.entry("+".to_string()).or_default().insert(
        Params::from_iter([Ty::Float, Ty::Float]),
        FnDef::from(|_: &mut _, args: Args| {
            StackVal::Float(args[0].as_float() + args[1].as_float() + 200.0)
        }),
    );

    env.ops.entry("+".to_string()).or_default().insert(
        Params::from_iter([Ty::Int, Ty::Float]),
        FnDef::from(|_: &mut _, args: Args| {
            StackVal::Float(args[0].as_float() + args[1].as_float() + 300.0)
        }),
    );

    env.ops.entry("-".to_string()).or_default().insert(
        Params::from_iter([Ty::Float, Ty::Float]),
        FnDef::from(|_: &mut _, args: Args| {
            StackVal::Float(args[0].as_float() - args[1].as_float())
        }),
    );
}
