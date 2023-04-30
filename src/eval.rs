use std::fmt::Display;

use astro_float::Consts;

use crate::ast::{Atom, BinaryOp, Expr, Stmt, UnaryOp, UserFunc};
use crate::context::Context;
use crate::formatting::float_to_string;
use crate::{CalcError, CalcResult, Number, PREC, RM};

pub fn eval_atom(atom: &Atom, ctx: &Context) -> CalcResult {
    match atom {
        Atom::Num(num) => Ok(num.clone()),
        Atom::Symbol(name) => ctx.lookup_value(name),
    }
}

pub fn eval_expr(expr: &Expr, ctx: &Context) -> CalcResult {
    match expr {
        Expr::AtomExpr(atom) => eval_atom(atom, ctx),
        Expr::UnaryExpr { op, data } => {
            let data = eval_expr(data, ctx)?;
            match op {
                UnaryOp::Negate => Ok(-data),
            }
        }
        Expr::BinaryExpr { lhs, rhs, op } => {
            let lhs = eval_expr(lhs, ctx)?;
            let rhs = eval_expr(rhs, ctx)?;
            Ok(match op {
                BinaryOp::Plus => lhs.add(&rhs, PREC, RM),
                BinaryOp::Minus => lhs.sub(&rhs, PREC, RM),
                BinaryOp::Times => lhs.mul(&rhs, PREC, RM),
                BinaryOp::Divide => lhs.div(&rhs, PREC, RM),
                BinaryOp::Power => {
                    // TODO: Don't unwrap
                    let mut consts = Consts::new().unwrap();
                    lhs.pow(&rhs, PREC, RM, &mut consts)
                }
            })
        }
        Expr::FunctionCall { function, args } => {
            let function = ctx.lookup_fn(function)?;
            let args = args
                .into_iter()
                .map(|arg| eval_expr(arg, ctx))
                .collect::<Result<Vec<Number>, CalcError>>()?;

            function.call(&args, ctx)
        } // Expr::BlockExpr { stmts, final_expr } => {
          //     // Create new scope
          //     let mut eval_scope = ctx.clone();
          //     eval_scope.add_scope(HashMap::new());

          //     // Evaulate stmts in new scope
          //     for stmt in stmts {
          //         eval_stmt(stmt, &mut eval_scope)?;
          //     }

          //     // Evaluate expr in new scope
          //     eval_expr(final_expr, &eval_scope)
          // }
    }
}

pub enum CalcValue {
    Ok,
    Value(Number),
}

impl Display for CalcValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CalcValue::Ok => write!(f, "ok"),
            CalcValue::Value(v) => write!(f, "{}", float_to_string(v)),
        }
    }
}

pub fn eval_stmt(stmt: &Stmt, ctx: &mut Context) -> Result<CalcValue, CalcError> {
    match stmt {
        Stmt::FuncDef { name, params, body } => {
            let func = UserFunc::new(params.clone(), body.clone());
            ctx.bind_fn(name.clone(), func)?;
            Ok(CalcValue::Ok)
        }
        Stmt::Assignment { name, value } => {
            let value = eval_expr(&value, ctx)?;
            ctx.bind_value(name.clone(), value)
                .map(|v| CalcValue::Value(v))
        }
        Stmt::ExprStmt(expr) => eval_expr(expr, ctx).map(|v| CalcValue::Value(v)),
    }
}
