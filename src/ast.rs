use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::zip;

use serde::{Deserialize, Serialize};

use crate::context::Context;
use crate::eval::eval_expr;
use crate::{CalcError, CalcResult, Number};

pub enum CalcFuncRef<'a> {
    Builtin(&'a BuiltinFunc),
    UserDef(&'a UserFunc),
}

impl<'a> CalcFuncRef<'a> {
    pub fn call(&self, args: &[Number], ctx: &Context) -> CalcResult {
        match self {
            CalcFuncRef::Builtin(func) => {
                // First, check arity
                if func.arity != args.len() {
                    return Err(CalcError::IncorrectArity(func.arity, args.len()));
                }

                // Call function
                func.apply(args, ctx)
            }
            CalcFuncRef::UserDef(func) => {
                // Check arity
                if func.arity() != args.len() {
                    return Err(CalcError::IncorrectArity(func.arity(), args.len()));
                }

                func.apply(args, ctx)
            }
        }
    }
}

#[derive(Clone)]
pub enum CalcFunc {
    Builtin(BuiltinFunc),
    UserDef(UserFunc),
}

// impl CalcFunc {
//     pub fn call(&self, args: &[Number], ctx: &Context) -> CalcResult {
//         match self {
//             CalcFunc::Builtin(func) => {
//                 // First, check arity
//                 if func.arity != args.len() {
//                     return Err(CalcError::IncorrectArity(func.arity, args.len()));
//                 }
//
//                 // Call function
//                 func.apply(args, ctx)
//             }
//             CalcFunc::UserDef(func) => {
//                 // Check arity
//                 if func.arity() != args.len() {
//                     return Err(CalcError::IncorrectArity(func.arity(), args.len()));
//                 }
//
//                 func.apply(args, ctx)
//             }
//         }
//     }
// }

#[derive(Clone)]
pub struct BuiltinFunc {
    pub arity: usize,

    // TODO: Probably doesn't need context.
    apply: fn(&[Number], &Context) -> CalcResult,
}

impl BuiltinFunc {
    pub fn new(arity: usize, apply: fn(&[Number], &Context) -> CalcResult) -> BuiltinFunc {
        BuiltinFunc { arity, apply }
    }

    pub fn apply(&self, args: &[Number], ctx: &Context) -> CalcResult {
        (self.apply)(args, ctx)
    }
}

impl Debug for BuiltinFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "builtin function")
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct UserFunc {
    bindings: Vec<String>,
    body: Expr,
}

impl UserFunc {
    pub fn new(bindings: Vec<String>, body: Expr) -> UserFunc {
        UserFunc { bindings, body }
    }

    pub fn apply(&self, args: &[Number], ctx: &Context) -> CalcResult {
        // Create evaluation scope
        let mut eval_scope = ctx.clone();
        let bindings = HashMap::from_iter(zip(self.bindings.iter().cloned(), args.iter().cloned()));
        eval_scope.add_scope(bindings);

        // Evaluate function body in new scope
        eval_expr(&self.body, &eval_scope)
    }

    /// Return the number of arguments the function takes.
    pub fn arity(&self) -> usize {
        self.bindings.len()
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Atom {
    Symbol(String),
    Num(Number),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum BinaryOp {
    Plus,
    Minus,
    Times,
    Divide,
    Power,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum UnaryOp {
    Negate,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Expr {
    AtomExpr(Atom),
    UnaryExpr {
        op: UnaryOp,
        data: Box<Expr>,
    },
    BinaryExpr {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: BinaryOp,
    },
    FunctionCall {
        function: String,
        args: Vec<Expr>,
    },
    // BlockExpr {
    //     stmts: Vec<Stmt>,
    //     final_expr: Box<Expr>,
    // },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Stmt {
    FuncDef {
        name: String,
        params: Vec<String>,
        body: Expr,
    },
    Assignment {
        name: String,
        value: Expr,
    },
    ExprStmt(Expr),
}
