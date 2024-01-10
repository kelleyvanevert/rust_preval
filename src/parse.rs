use std::rc::Rc;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, one_of},
    combinator::{eof, map, opt, recognize, value},
    multi::{many0, many0_count, many1},
    sequence::{pair, preceded, terminated, tuple},
    IResult, Parser,
};

use crate::types::{Args, Ctx, FnBody, Params, Program, StackVal, Ty};

fn parse_int(s: &str) -> IResult<&str, Program> {
    map(recognize(many1(one_of("0123456789"))), |s: &str| {
        let num = s.parse::<i64>().unwrap();
        Program(Rc::new(move |_| StackVal::Int(num)))
    })
    .parse(s)
}

fn parse_identifier(s: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))
    .parse(s)
}

fn parse_var(s: &str) -> IResult<&str, Program> {
    map(parse_identifier, |id: &str| {
        let id = id.to_string();
        Program(Rc::new(move |ctx| ctx.get(&id).unwrap_or(StackVal::Nil)))
    })
    .parse(s)
}

fn parse_parameter(s: &str) -> IResult<&str, (&str, Ty)> {
    pair(
        parse_identifier,
        map(
            opt(preceded(
                tuple((multispace0, char(':'), multispace0)),
                parse_type_leaf,
            )),
            |ty| ty.unwrap_or(Ty::Any),
        ),
    )
    .parse(s)
}

fn parse_parameter_list(s: &str) -> IResult<&str, Vec<(&str, Ty)>> {
    map(
        tuple((
            char('('),
            multispace0,
            opt(tuple((
                parse_parameter,
                many0(map(
                    tuple((multispace0, char(','), multispace0, parse_parameter)),
                    |t| t.3,
                )),
            ))),
            multispace0,
            opt(char(',')),
            multispace0,
            char(')'),
        )),
        |t| match t.2 {
            None => vec![],
            Some((first, mut rest)) => {
                rest.insert(0, first);
                rest
            }
        },
    )
    .parse(s)
}

fn parse_invocation_args(s: &str) -> IResult<&str, Vec<Program>> {
    map(
        tuple((
            char('('),
            multispace0,
            opt(tuple((
                parse_expr,
                many0(map(
                    tuple((multispace0, char(','), multispace0, parse_expr)),
                    |t| t.3,
                )),
            ))),
            multispace0,
            opt(char(',')),
            multispace0,
            char(')'),
        )),
        |t| match t.2 {
            None => vec![],
            Some((first, mut rest)) => {
                rest.insert(0, first);
                rest
            }
        },
    )
    .parse(s)
}

fn parse_fn_decl(s: &str) -> IResult<&str, Program> {
    map(
        tuple((
            tag("fn"),
            multispace1,
            parse_identifier,
            multispace0,
            parse_parameter_list,
            multispace0,
            parse_block,
        )),
        |(_, _, name, _, parameters, _, body)| {
            let name = name.to_string();
            let params = Params::from_iter(parameters.iter().map(|p| p.1.clone()));
            let param_names = parameters
                .iter()
                .map(|p| p.0.to_string())
                .collect::<Vec<_>>();

            Program(Rc::new(move |ctx| {
                let param_names = param_names.clone();
                let body = body.clone();

                ctx.add_fn_sig(
                    &name,
                    params.clone(),
                    FnBody(Rc::new(move |ctx: &mut Ctx, args: Args| {
                        ctx.execute_in_fresh_scope(|ctx| {
                            for (i, name) in param_names.iter().enumerate() {
                                ctx.env.scopes[ctx.current_scope]
                                    .values
                                    .insert(name.clone(), args[i]);
                            }

                            body.eval(ctx)
                        })
                    })),
                );

                StackVal::Nil
            }))
        },
    )
    .parse(s)
}

/// TODO parse "items" (i.e. fn decls) first
fn parse_block(s: &str) -> IResult<&str, Program> {
    map(
        tuple((
            char('{'),
            multispace0,
            opt(tuple((
                parse_expr,
                many0(map(
                    tuple((multispace0, char(';'), multispace0, parse_expr)),
                    |t| t.3,
                )),
            ))),
            multispace0,
            opt(char(';')),
            multispace0,
            char('}'),
        )),
        |t| {
            let all_exprs = match t.2 {
                None => vec![],
                Some((first, mut rest)) => {
                    rest.insert(0, first);
                    rest
                }
            };

            Program(Rc::new(move |ctx| {
                let mut res = StackVal::Nil;

                for expr in &all_exprs {
                    res = expr.eval(ctx);
                }

                res
            }))
        },
    )
    .parse(s)
}

fn parse_expr_parenthesized(s: &str) -> IResult<&str, Program> {
    map(
        tuple((char('('), multispace0, parse_expr, multispace0, char(')'))),
        |t| t.2,
    )
    .parse(s)
}

fn parse_expr_leaf(s: &str) -> IResult<&str, Program> {
    alt((
        parse_fn_decl,
        parse_assign,
        parse_while,
        parse_int,
        parse_var,
        parse_expr_parenthesized,
        parse_block,
    ))
    .parse(s)
}

fn expr_invocation_stack(s: &str) -> IResult<&str, Program> {
    map(
        pair(
            parse_expr_leaf,
            many0(preceded(multispace0, parse_invocation_args)),
        ),
        |(mut f, invocations)| {
            for args in invocations {
                f = Program(Rc::new(move |ctx| {
                    let args = Args::from_iter(args.iter().map(|arg| arg.eval(ctx)));
                    let f: StackVal = f.eval(ctx);

                    ctx.call(f, args).expect("could not invoke inexistent fn")
                }));
            }

            f
        },
    )
    .parse(s)
}

fn unary_expr_stack(s: &str) -> IResult<&str, Program> {
    map(
        tuple((
            many0(terminated(tag("!"), multispace0)),
            expr_invocation_stack,
        )),
        |(ops, mut expr)| {
            for op in ops.into_iter().rev() {
                let op = op.to_string();
                expr = Program(Rc::new(move |ctx: &mut Ctx| {
                    let val = expr.eval(ctx);
                    ctx.call_by_name(&op, Args::from_iter([val]))
                        .expect(&format!("could not apply unary op {}", op))
                }));
            }
            expr
        },
    )
    .parse(s)
}

fn mul_expr_stack(s: &str) -> IResult<&str, Program> {
    map(
        tuple((
            unary_expr_stack,
            many0(tuple((
                multispace0,
                alt((tag("*"), tag("/"), tag("%"))),
                multispace0,
                unary_expr_stack,
            ))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                let op = op.to_string();
                expr = Program(Rc::new(move |ctx: &mut Ctx| {
                    let le = expr.eval(ctx);
                    let ri = right.eval(ctx);
                    ctx.call_by_name(&op, Args::from_iter([le, ri]))
                        .expect(&format!("could not apply binary op {}", op))
                }));
            }
            expr
        },
    )
    .parse(s)
}

fn add_expr_stack(s: &str) -> IResult<&str, Program> {
    map(
        tuple((
            mul_expr_stack,
            many0(tuple((
                multispace0,
                alt((tag("+"), tag("-"), tag("<<"))),
                multispace0,
                mul_expr_stack,
            ))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                let op = op.to_string();
                expr = Program(Rc::new(move |ctx: &mut Ctx| {
                    let le = expr.eval(ctx);
                    let ri = right.eval(ctx);
                    ctx.call_by_name(&op, Args::from_iter([le, ri]))
                        .expect(&format!("could not apply binary op {}", op))
                }));
            }
            expr
        },
    )
    .parse(s)
}

fn equ_expr_stack(s: &str) -> IResult<&str, Program> {
    map(
        tuple((
            add_expr_stack,
            many0(tuple((
                multispace0,
                alt((
                    tag("!="),
                    tag(">="),
                    tag("<="),
                    tag("=="),
                    tag("<"),
                    tag(">"),
                    tag("^"),
                )),
                multispace0,
                add_expr_stack,
            ))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                let op = op.to_string();
                expr = Program(Rc::new(move |ctx: &mut Ctx| {
                    let le = expr.eval(ctx);
                    let ri = right.eval(ctx);
                    ctx.call_by_name(&op, Args::from_iter([le, ri]))
                        .expect(&format!("could not apply binary op {}", op))
                }));
            }
            expr
        },
    )
    .parse(s)
}

fn and_expr_stack(s: &str) -> IResult<&str, Program> {
    map(
        tuple((
            equ_expr_stack,
            many0(tuple((
                multispace0,
                alt((
                    tag("&&"),
                    //
                )),
                multispace0,
                equ_expr_stack,
            ))),
        )),
        |(mut expr, ops)| {
            for (_, _op, _, right) in ops {
                expr = Program(Rc::new(move |ctx: &mut Ctx| {
                    let le = expr.eval(ctx);
                    if !le.as_truthy() {
                        le
                    } else {
                        right.eval(ctx)
                    }
                }));
            }
            expr
        },
    )
    .parse(s)
}

fn or_expr_stack(s: &str) -> IResult<&str, Program> {
    map(
        tuple((
            and_expr_stack,
            many0(tuple((
                multispace0,
                alt((
                    tag("||"),
                    tag("??"),
                    //
                )),
                multispace0,
                and_expr_stack,
            ))),
        )),
        |(mut expr, ops)| {
            for (_, op, _, right) in ops {
                match op {
                    "||" => {
                        expr = Program(Rc::new(move |ctx: &mut Ctx| {
                            let le = expr.eval(ctx);
                            if le.as_truthy() {
                                le
                            } else {
                                right.eval(ctx)
                            }
                        }));
                    }
                    "??" => {
                        expr = Program(Rc::new(move |ctx: &mut Ctx| {
                            let le = expr.eval(ctx);
                            if !le.is_nil() {
                                le
                            } else {
                                right.eval(ctx)
                            }
                        }));
                    }
                    _ => unreachable!(),
                }
            }
            expr
        },
    )
    .parse(s)
}

fn parse_expr(s: &str) -> IResult<&str, Program> {
    or_expr_stack.parse(s)
}

fn parse_assign(s: &str) -> IResult<&str, Program> {
    map(
        tuple((
            parse_identifier,
            multispace0,
            char('='),
            multispace0,
            parse_expr,
        )),
        |t| {
            let id = t.0.to_string();
            Program(Rc::new(move |ctx| {
                let val = t.4.eval(ctx);

                ctx.env.scopes[ctx.current_scope]
                    .values
                    .insert(id.clone(), val);

                val
            }))
        },
    )
    .parse(s)
}

fn parse_while(s: &str) -> IResult<&str, Program> {
    map(
        tuple((
            tag("while"),
            multispace1,
            parse_expr,
            multispace0,
            parse_block,
        )),
        |(_, _, cond, _, body)| {
            Program(Rc::new(move |ctx| {
                while cond.eval(ctx).as_truthy() {
                    body.eval(ctx);
                }

                StackVal::Nil
            }))
        },
    )
    .parse(s)
}

fn parse_type_leaf(s: &str) -> IResult<&str, Ty> {
    alt((
        value(Ty::Any, tag("any")),
        value(Ty::Nil, tag("nil")),
        value(Ty::Bool, tag("bool")),
        value(Ty::Str, tag("str")),
        value(Ty::Int, tag("int")),
        value(Ty::Float, tag("float")),
        value(Ty::FnDef, tag("fn")),
        value(Ty::List, tag("list")),
        value(Ty::Dict, tag("dict")),
    ))
    .parse(s)
}

pub fn parse_doc(s: &str) -> Option<Program> {
    terminated(parse_expr, eof).parse(s).map(|r| r.1).ok()
}
