use std::rc::Rc;

use fxhash::FxHashMap;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, one_of},
    combinator::{eof, map, opt, recognize},
    multi::{many0, many0_count, many1},
    sequence::{pair, terminated, tuple},
    IResult, Parser,
};

use crate::types::{Args, Ctx, Program, StackVal};

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
        parse_assign,
        parse_while,
        parse_print,
        parse_int,
        parse_var,
        parse_expr_parenthesized,
        parse_block,
    ))
    .parse(s)
}

fn unary_expr_stack(s: &str) -> IResult<&str, Program> {
    map(
        tuple((many0(terminated(tag("!"), multispace0)), parse_expr_leaf)),
        |(ops, mut expr)| {
            for op in ops.into_iter().rev() {
                let op = op.to_string();
                expr = Program(Rc::new(move |ctx: &mut Ctx| {
                    let val = expr.eval(ctx);
                    ctx.call(&op, Args::from_iter([val]))
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
                    ctx.call(&op, Args::from_iter([le, ri]))
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
                    ctx.call(&op, Args::from_iter([le, ri]))
                        .expect(&format!("could not apply binary op {}", op))
                }));
            }
            expr
        },
    )
    .parse(s)
}

fn parse_expr(s: &str) -> IResult<&str, Program> {
    add_expr_stack.parse(s)
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

fn parse_print(s: &str) -> IResult<&str, Program> {
    map(
        tuple((tag("print"), multispace1, parse_expr)),
        |(_, _, expr)| {
            Program(Rc::new(move |ctx| {
                let val = expr.eval(ctx);
                println!("{}", val);

                val
            }))
        },
    )
    .parse(s)
}

pub fn parse_doc(s: &str) -> Option<Program> {
    terminated(parse_expr, eof).parse(s).map(|r| r.1).ok()
}
