//! Contains all the enums used to represent the AST.

use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::zip;

use serde::{Deserialize, Serialize};

use crate::context::Context;
use crate::eval::eval_expr;
use crate::{CalcError, CalcResult, Number};

/// Holds a reference to a builtin or user-defined function.
pub enum CalcFuncRef<'a> {
    /// Builtin function (like log or sqrt).
    Builtin(&'a BuiltinFunc),

    /// User-defined function.
    UserDef(&'a UserFunc),
}

impl<'a> CalcFuncRef<'a> {
    /// Call the function with arguments `args` in context `ctx`.
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

/// A function that can be called by the user.
#[derive(Clone)]
pub enum CalcFunc {
    /// Builtin function (like log or sqrt).
    Builtin(BuiltinFunc),

    /// User-defined function.
    UserDef(UserFunc),
}

/// Builtin function (like log or sqrt).
#[derive(Clone)]
pub struct BuiltinFunc {
    /// The number of arguments that the function takes.
    pub arity: usize,

    // TODO: Probably doesn't need context.
    apply: fn(&[Number], &Context) -> CalcResult,
}

impl BuiltinFunc {
    pub(crate) fn new(arity: usize, apply: fn(&[Number], &Context) -> CalcResult) -> BuiltinFunc {
        BuiltinFunc { arity, apply }
    }

    /// Call the function on `args` in `ctx`.
    pub fn apply(&self, args: &[Number], ctx: &Context) -> CalcResult {
        (self.apply)(args, ctx)
    }
}

impl Debug for BuiltinFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "builtin function")
    }
}

/// User-defined function.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct UserFunc {
    bindings: Vec<String>,
    body: Expr,
}

impl UserFunc {
    /// Create a new function with parameters `bindings` (in order) and body expression `body`.
    pub fn new(bindings: Vec<String>, body: Expr) -> UserFunc {
        UserFunc { bindings, body }
    }

    /// Call the function on `args` in `ctx`.
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

#[allow(missing_docs)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Atom {
    Symbol(String),
    Num(Number),
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum BinaryOp {
    Plus,
    Minus,
    Times,
    Divide,
    Power,
}

#[allow(missing_docs)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum UnaryOp {
    Negate,
}

#[allow(missing_docs)]
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
}

#[allow(missing_docs)]
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
