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

fn main() {
    let f = parse_doc("{ let x = 5 * 2; x }").unwrap();

    println!("Hello, world! {}", f.eval(&mut Default::default()));
}

type Env = FxHashMap<String, i64>;

#[derive(Clone)]
struct EvalFn(Rc<dyn Fn(&mut Env) -> i64>);

impl EvalFn {
    fn eval(&self, env: &mut Env) -> i64 {
        self.0(env)
    }

    fn with_unary_op(self, op: &str) -> EvalFn {
        match op {
            "!" => EvalFn(Rc::new(move |env| {
                let num = self.eval(env);
                if num == 0 {
                    1
                } else {
                    0
                }
            })),
            _ => unreachable!("unknown op: {}", op),
        }
    }

    fn with_binary_op(self, op: String, rhs: EvalFn) -> EvalFn {
        EvalFn(Rc::new(move |env| {
            let le = self.eval(env);
            let ri = rhs.eval(env);
            match &op[..] {
                "*" => le * ri,
                "/" => le / ri,
                "%" => le % ri,
                "+" => le + ri,
                "-" => le - ri,
                "<<" => le << ri,
                _ => unreachable!("unknown op: {}", op),
            }
        }))
    }
}

fn parse_int(s: &str) -> IResult<&str, EvalFn> {
    map(recognize(many1(one_of("0123456789"))), |s: &str| {
        let num = s.parse::<i64>().unwrap();
        EvalFn(Rc::new(move |_env| num))
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

fn parse_var(s: &str) -> IResult<&str, EvalFn> {
    map(parse_identifier, |id: &str| {
        let id = id.to_string();
        EvalFn(Rc::new(move |env| {
            let num = env.get(&id).cloned().unwrap_or(0);
            num
        }))
    })
    .parse(s)
}

fn parse_block(s: &str) -> IResult<&str, EvalFn> {
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

            EvalFn(Rc::new(move |env| {
                let mut res = 0;

                for expr in &all_exprs {
                    res = expr.eval(env);
                }

                res
            }))
        },
    )
    .parse(s)
}

fn parse_expr_parenthesized(s: &str) -> IResult<&str, EvalFn> {
    map(
        tuple((char('('), multispace0, parse_expr, multispace0, char(')'))),
        |t| t.2,
    )
    .parse(s)
}

fn parse_expr_leaf(s: &str) -> IResult<&str, EvalFn> {
    alt((
        parse_decl,
        parse_int,
        parse_var,
        parse_expr_parenthesized,
        parse_block,
    ))
    .parse(s)
}

fn unary_expr_stack(s: &str) -> IResult<&str, EvalFn> {
    map(
        tuple((many0(terminated(tag("!"), multispace0)), parse_expr_leaf)),
        |(ops, mut expr)| {
            for op in ops.into_iter().rev() {
                expr = expr.with_unary_op(op);
            }
            expr
        },
    )
    .parse(s)
}

fn mul_expr_stack(s: &str) -> IResult<&str, EvalFn> {
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
                expr = expr.with_binary_op(op.to_string(), right);
            }
            expr
        },
    )
    .parse(s)
}

fn add_expr_stack(s: &str) -> IResult<&str, EvalFn> {
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
                expr = expr.with_binary_op(op.to_string(), right);
            }
            expr
        },
    )
    .parse(s)
}

fn parse_expr(s: &str) -> IResult<&str, EvalFn> {
    add_expr_stack.parse(s)
}

fn parse_decl(s: &str) -> IResult<&str, EvalFn> {
    map(
        tuple((
            tag("let"),
            multispace1,
            parse_identifier,
            multispace0,
            char('='),
            multispace0,
            parse_expr,
        )),
        |t| {
            let id = t.2.to_string();
            EvalFn(Rc::new(move |env| {
                let val = t.6.eval(env);
                env.insert(id.clone(), val);
                val
            }))
        },
    )
    .parse(s)
}

fn parse_doc(s: &str) -> Option<EvalFn> {
    terminated(parse_expr, eof).parse(s).map(|r| r.1).ok()
}
