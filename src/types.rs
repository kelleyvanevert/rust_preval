use std::{cmp::Ordering, ops::Index, rc::Rc};

use fxhash::FxHashMap;
use smallvec::SmallVec;

use crate::signature_map::FnSignatureMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Any,
    Nil,
    Bool,
    Str,
    Int,
    Float,
    FnDef,
    List,
    Dict,
}

impl PartialOrd for Ty {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (a, b) if a == b => Some(Ordering::Equal),
            (Ty::Any, _) => Some(Ordering::Less),
            (_, Ty::Any) => Some(Ordering::Greater),
            (Ty::Float, Ty::Int) => Some(Ordering::Less),
            (Ty::Int, Ty::Float) => Some(Ordering::Greater),
            _ => None,
        }
    }
}

pub struct Args(SmallVec<[StackVal; 10]>);

impl FromIterator<StackVal> for Args {
    fn from_iter<T: IntoIterator<Item = StackVal>>(iter: T) -> Self {
        Args(SmallVec::from_iter(iter))
    }
}

impl Index<usize> for Args {
    type Output = StackVal;

    fn index(&self, index: usize) -> &Self::Output {
        self.0.index(index)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Params(SmallVec<[Ty; 10]>);

impl FromIterator<Ty> for Params {
    fn from_iter<T: IntoIterator<Item = Ty>>(iter: T) -> Self {
        Params(SmallVec::from_iter(iter))
    }
}

impl PartialOrd for Params {
    // (bool, int)
    // (int)

    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.0.len() != other.0.len() {
            return None;
        }

        let mut less_than = true;
        let mut greater_than = true;

        for (a, b) in self.0.iter().zip(other.0.iter()) {
            match a.partial_cmp(&b) {
                None => return None,
                Some(Ordering::Greater) => {
                    less_than = false;
                }
                Some(Ordering::Less) => {
                    greater_than = false;
                }
                Some(Ordering::Equal) => {}
            }
        }

        match (less_than, greater_than) {
            (true, true) => Some(Ordering::Equal),
            (true, _) => Some(Ordering::Less),
            (_, true) => Some(Ordering::Greater),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum StackVal {
    Nil,
    Bool(bool),
    Str {
        loc: usize,
    },
    Int(i64),
    Float(f64),
    // Regex {
    //     loc: usize,
    // },
    FnDef {
        loc: usize,
    },
    List {
        loc: usize,
        start: usize,
        len: usize,
    },
    Dict {
        loc: usize,
    },
}

impl std::fmt::Display for StackVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackVal::Nil => write!(f, "()"),
            StackVal::Bool(b) => write!(f, "{}", b),
            StackVal::Str { .. } => write!(f, "<str>"),
            StackVal::Int(n) => write!(f, "{}", n),
            StackVal::Float(n) => write!(f, "{:.1}", n),
            // StackVal::Regex { .. } => Ty::Regex,
            StackVal::FnDef { .. } => write!(f, "<fndef>"),
            StackVal::List { .. } => write!(f, "<list>"),
            StackVal::Dict { .. } => write!(f, "<dict>"),
        }
    }
}

impl StackVal {
    pub fn ty(&self) -> Ty {
        match self {
            StackVal::Nil => Ty::Nil,
            StackVal::Bool(_) => Ty::Bool,
            StackVal::Str { .. } => Ty::Str,
            StackVal::Int(_) => Ty::Int,
            StackVal::Float(_) => Ty::Float,
            // StackVal::Regex { .. } => Ty::Regex,
            StackVal::FnDef { .. } => Ty::FnDef,
            StackVal::List { .. } => Ty::List,
            StackVal::Dict { .. } => Ty::Dict,
        }
    }

    pub fn as_truthy(&self) -> bool {
        match self {
            StackVal::Nil => false,
            StackVal::Bool(b) => *b,
            StackVal::Str { .. } => true, // empty string?
            StackVal::Int(n) => *n != 0,
            StackVal::Float(n) => *n != 0.0,
            // StackVal::Regex { .. } => Ty::Regex,
            StackVal::FnDef { .. } => true,
            StackVal::List { .. } => true,
            StackVal::Dict { .. } => true,
        }
    }

    pub fn as_float(&self) -> f64 {
        match self {
            StackVal::Int(n) => *n as f64,
            StackVal::Float(n) => *n,
            _ => panic!("cannot cast {} as float", self),
        }
    }

    pub fn as_int(&self) -> i64 {
        match self {
            StackVal::Int(n) => *n,
            _ => panic!("cannot cast {} as int", self),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HeapVal {
    Str(String),
    // etc.
}

#[derive(Debug, Clone, Default)]
pub struct Scope {
    pub parent_scope: Option<usize>,
    pub values: FxHashMap<String, StackVal>,
}

pub struct Env {
    pub ops: FxHashMap<String, FnSignatureMap>,
    pub scopes: Vec<Scope>,
    pub heap: Vec<HeapVal>,
}

impl Env {}

pub struct Ctx {
    pub env: Env,
    pub current_scope: usize,
}

impl Ctx {
    pub fn new() -> Self {
        Ctx {
            env: Env {
                ops: Default::default(),
                scopes: vec![Default::default()],
                heap: vec![],
            },
            current_scope: 0,
        }
    }

    pub fn get(&self, name: &str) -> Option<StackVal> {
        let mut scope = &self.env.scopes[self.current_scope];

        loop {
            if let Some(v) = scope.values.get(name) {
                return Some(*v);
            }

            if let Some(id) = scope.parent_scope {
                scope = &self.env.scopes[id];
            } else {
                return None;
            }
        }
    }

    pub fn call(&mut self, name: &str, args: Args) -> Option<StackVal> {
        self.env
            .ops
            .get(name)
            .map(|signatures| {
                let params = args.0.iter().map(|arg| arg.ty()).collect::<Params>();
                signatures.find_best_match(params)
            })
            .flatten()
            .map(|def| def.eval(self, args))
    }
}

#[derive(Clone)]
pub struct Program(pub Rc<dyn Fn(&mut Ctx) -> StackVal>);

impl Program {
    pub fn eval(&self, ctx: &mut Ctx) -> StackVal {
        self.0(ctx)
    }

    fn unary(f: Program, map: fn(StackVal, &mut Ctx) -> StackVal) -> Program {
        Program(Rc::new(move |ctx| map(f.eval(ctx), ctx)))
    }

    fn unary_from_op(f: Program, op: &str) -> Program {
        match op {
            "!" => Program::unary(f, |val, _ctx| match val {
                StackVal::Bool(b) => StackVal::Bool(!b),
                _ => panic!(),
            }),
            "-" => Program::unary(f, |val, _ctx| match val {
                StackVal::Int(n) => StackVal::Int(0 - n),
                StackVal::Float(n) => StackVal::Float(0.0 - n),
                _ => panic!(),
            }),
            _ => unreachable!("unknown op: {}", op),
        }
    }

    fn binary(
        lhs: Program,
        rhs: Program,
        map: fn(StackVal, StackVal, &mut Ctx) -> StackVal,
    ) -> Program {
        Program(Rc::new(move |ctx| map(lhs.eval(ctx), rhs.eval(ctx), ctx)))
    }

    // fn binary_from_op(lhs: Program, op: &str, rhs: Program) -> Program {
    //     match op {
    //         "*" => Program::binary(lhs, rhs, |le, ri, _ctx| le * ri),
    //         "/" => Program::binary(lhs, rhs, |le, ri, _ctx| le / ri),
    //         "%" => Program::binary(lhs, rhs, |le, ri, _ctx| le % ri),
    //         "+" => Program::binary(lhs, rhs, |le, ri, _ctx| le + ri),
    //         "-" => Program::binary(lhs, rhs, |le, ri, _ctx| le - ri),
    //         "<<" => Program::binary(lhs, rhs, |le, ri, _ctx| le << ri),
    //         _ => unreachable!("unknown op: {}", op),
    //     }
    // }
}

#[derive(Clone)]
pub struct FnDef(pub Rc<dyn Fn(&mut Ctx, Args) -> StackVal>);

impl FnDef {
    pub fn eval(&self, ctx: &mut Ctx, args: Args) -> StackVal {
        self.0(ctx, args)
    }
}

impl std::fmt::Debug for FnDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("FnDef").finish()
    }
}

impl<F> From<F> for FnDef
where
    F: Fn(&mut Ctx, Args) -> StackVal + 'static,
{
    fn from(f: F) -> FnDef {
        FnDef(Rc::new(f))
    }
}

impl FnDef {
    pub fn error(message: String) -> FnDef {
        FnDef(Rc::new(move |_, _| panic!("{}", message)))
    }
}
