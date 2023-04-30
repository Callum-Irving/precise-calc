use std::fmt::Display;

use astro_float::{BigFloat, RoundingMode};

pub mod ast;
pub mod context;
pub mod eval;
pub mod formatting;
pub mod parser;

pub type Number = BigFloat;
pub type CalcResult = Result<Number, CalcError>;
// Preicison of floating point numbers
pub const PREC: usize = 128;
pub const BASE_10_PREC: usize = 38; // log(2^PREC)
pub const RM: RoundingMode = RoundingMode::ToEven;

#[derive(Debug, Clone)]
pub enum CalcError {
    /// Symbol is not in context.
    NameNotFound(String),

    /// Use tried to assign a value to a name that already has a value assigned to it.
    NameAlreadyBound(String),

    /// Function called with incorrect number of arguments. `IncorrectArity(expected, found)`.
    IncorrectArity(usize, usize),

    ParseNum,
    ParseError,
    IOError,
}

impl Display for CalcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CalcError::NameNotFound(name) => write!(f, "ERROR: Name not found: \"{}\"", name),
            CalcError::NameAlreadyBound(name) => {
                write!(f, "ERROR: Name already bound: \"{}\"", name)
            }
            CalcError::IncorrectArity(expected, found) => {
                write!(f, "ERROR: Expected {} arguments, found {}", expected, found)
            }
            CalcError::ParseError => write!(f, "ERROR: Parsing error"),
            CalcError::IOError => write!(f, "ERROR: IO error"),
            CalcError::ParseNum => write!(f, "ERROR: Number parsing error"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ast::*;
    use super::context::Context;
    use super::eval::*;
    use crate::PREC;

    use astro_float::BigFloat;

    #[test]
    fn test_atom_eval() {
        let num = BigFloat::from_i32(123, PREC);

        let mut ctx = Context::new();
        ctx.bind_value("a".to_string(), num.clone())
            .expect("failed to bind value");

        let sym_atom = Atom::Symbol("a".to_string());
        let res = eval_atom(&sym_atom, &ctx).expect("failed to evaluate symbol atom");
        assert_eq!(num, res);

        let num_atom = Atom::Num(num.clone());
        let res = eval_atom(&num_atom, &ctx).expect("failed to evaluate number atom");
        assert_eq!(num, res);
    }

    #[test]
    fn test_expr_eval() {
        let num = BigFloat::from_i32(123, PREC);
        let num2 = BigFloat::from_i32(-123, PREC);
        let num3 = BigFloat::from_i32(10, PREC);
        let num4 = BigFloat::from_i32(20, PREC);
        let num5 = BigFloat::from_i32(30, PREC);

        let ctx = Context::new();

        let expr = Expr::UnaryExpr {
            op: UnaryOp::Negate,
            data: Box::new(Expr::AtomExpr(Atom::Num(num))),
        };
        let res = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(res, num2);

        let lhs = Expr::AtomExpr(Atom::Num(num3));
        let rhs = Expr::AtomExpr(Atom::Num(num4));
        let add_expr = Expr::BinaryExpr {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: BinaryOp::Plus,
        };
        let res = eval_expr(&add_expr, &ctx).unwrap();
        assert_eq!(res, num5);
    }

    #[test]
    fn test_function_call() {
        let num1 = BigFloat::from_i32(10, PREC);
        let num2 = BigFloat::from_i32(20, PREC);
        let num3 = BigFloat::from_i32(30, PREC);

        let mut ctx = Context::new();

        let func = UserFunc::new(
            vec!["x".to_string(), "y".to_string()],
            Expr::BinaryExpr {
                lhs: Box::new(Expr::AtomExpr(Atom::Symbol("x".to_string()))),
                rhs: Box::new(Expr::AtomExpr(Atom::Symbol("y".to_string()))),
                op: BinaryOp::Plus,
            },
        );
        ctx.bind_fn("f".to_string(), func).unwrap();

        let func_call = Expr::FunctionCall {
            function: "f".to_string(),
            args: vec![
                Expr::AtomExpr(Atom::Num(num1)),
                Expr::AtomExpr(Atom::Num(num2)),
            ],
        };

        let res = eval_expr(&func_call, &ctx).unwrap();

        assert_eq!(res, num3);
    }
}
