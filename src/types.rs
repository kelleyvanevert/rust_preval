use std::{cmp::Ordering, ops::Index, rc::Rc};

use fxhash::FxHashMap;
use smallvec::SmallVec;

use crate::{signature_map::FnSignatureMap, stdlib::implement_stdlib};

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

#[allow(unused)]
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

    pub fn is_nil(&self) -> bool {
        match self {
            StackVal::Nil => true,
            _ => false,
        }
    }
}

#[allow(unused)]
#[derive(Debug)]
pub enum HeapVal {
    Str(String),
    FnDef(FnDef),
    // etc.
}

impl HeapVal {
    pub fn as_fn_def_mut(&mut self) -> &mut FnDef {
        match self {
            HeapVal::FnDef(def) => def,
            _ => panic!("cannot cast heap val {:?} as fn def", self),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Scope {
    pub parent_scope: Option<usize>,
    pub values: FxHashMap<String, StackVal>,
}

#[derive(Debug)]
pub struct Env {
    pub scopes: Vec<Scope>,
    pub heap: Vec<HeapVal>,
}

impl Env {
    pub fn add_fn_sig(&mut self, scope_id: usize, name: &str, params: Params, sig: FnBody) {
        let loc = match self.scopes[scope_id].values.get(name) {
            None => {
                let loc = self.heap.len();

                self.heap.push(HeapVal::FnDef(Default::default()));

                self.scopes[0]
                    .values
                    .insert(name.to_string(), StackVal::FnDef { loc });

                loc
            }
            Some(stack_val) => match stack_val {
                StackVal::FnDef { loc } => *loc,
                _ => panic!("var `{}` isn't a fn", name),
            },
        };

        self.heap[loc]
            .as_fn_def_mut()
            .signatures
            .insert(params, sig);
    }

    pub fn add_global_fn_sig(&mut self, name: &str, params: Params, sig: FnBody) {
        self.add_fn_sig(0, name, params, sig);
    }
}

pub struct Ctx {
    pub env: Env,
    pub current_scope: usize,
}

impl Ctx {
    pub fn new() -> Self {
        Ctx {
            env: Env {
                scopes: vec![Default::default()],
                heap: vec![],
            },
            current_scope: 0,
        }
    }

    pub fn with_stdlib() -> Self {
        let mut ctx = Ctx::new();
        implement_stdlib(&mut ctx.env);
        ctx
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

    pub fn call(&mut self, stack_val: StackVal, args: Args) -> Option<StackVal> {
        let StackVal::FnDef { loc } = stack_val else {
            panic!("stack val is not a fn def");
        };

        let HeapVal::FnDef(def) = &self.env.heap[loc] else {
            panic!("heap val is not a fn def");
        };

        let params = args.0.iter().map(|arg| arg.ty()).collect::<Params>();

        let Some(body) = def.signatures.find_best_match(params.clone()) else {
            return None;
        };

        Some(body.eval(self, args))
    }

    pub fn call_by_name(&mut self, name: &str, args: Args) -> Option<StackVal> {
        self.get(name)
            .and_then(|stack_val| self.call(stack_val, args))
    }

    pub fn add_fn_sig(&mut self, name: &str, params: Params, sig: FnBody) {
        self.env.add_fn_sig(self.current_scope, name, params, sig);
    }

    pub fn execute_in_fresh_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Ctx) -> R,
    {
        let id = self.env.scopes.len();

        self.env.scopes.push(Scope {
            parent_scope: Some(self.current_scope),
            values: Default::default(),
        });

        let curr = self.current_scope;
        self.current_scope = id;

        let res = f(self);

        self.current_scope = curr;

        res
    }
}

#[derive(Clone)]
pub struct Program(pub Rc<dyn Fn(&mut Ctx) -> StackVal>);

impl Program {
    pub fn eval(&self, ctx: &mut Ctx) -> StackVal {
        self.0(ctx)
    }

    // fn unary(f: Program, map: fn(StackVal, &mut Ctx) -> StackVal) -> Program {
    //     Program(Rc::new(move |ctx| map(f.eval(ctx), ctx)))
    // }

    // fn binary(
    //     lhs: Program,
    //     rhs: Program,
    //     map: fn(StackVal, StackVal, &mut Ctx) -> StackVal,
    // ) -> Program {
    //     Program(Rc::new(move |ctx| map(lhs.eval(ctx), rhs.eval(ctx), ctx)))
    // }
}

#[derive(Clone)]
pub struct FnBody(pub Rc<dyn Fn(&mut Ctx, Args) -> StackVal>);

impl FnBody {
    pub fn eval(&self, ctx: &mut Ctx, args: Args) -> StackVal {
        self.0(ctx, args)
    }
}

impl std::fmt::Debug for FnBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("FnSig").finish()
    }
}

impl<F> From<F> for FnBody
where
    F: Fn(&mut Ctx, Args) -> StackVal + 'static,
{
    fn from(f: F) -> FnBody {
        FnBody(Rc::new(f))
    }
}

impl FnBody {
    #[allow(unused)]
    pub fn error(message: String) -> FnBody {
        FnBody(Rc::new(move |_, _| panic!("{}", message)))
    }
}

#[derive(Debug, Default)]
pub struct FnDef {
    pub signatures: FnSignatureMap,
}
