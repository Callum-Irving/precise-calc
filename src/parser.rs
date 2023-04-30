use astro_float::{BigFloat, Radix};
use nom::branch::alt;
use nom::bytes::complete::take_while;
use nom::character::complete::{char, digit1, satisfy};
use nom::combinator::{cut, map, opt, recognize};
use nom::multi::{fold_many0, many0, many1, separated_list0};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::IResult;

use crate::Number;
use crate::RM;
use crate::{ast, PREC};

pub fn parse_stmt_list(input: &str) -> IResult<&str, Vec<ast::Stmt>> {
    separated_list0(newline1, parse_stmt)(input)
}

fn newline1(input: &str) -> IResult<&str, &str> {
    recognize(many1(char('\n')))(input)
}

fn whitespace0(input: &str) -> IResult<&str, &str> {
    recognize(many0(alt((char(' '), char('\t')))))(input)
}

pub fn parse_stmt(input: &str) -> IResult<&str, ast::Stmt> {
    alt((
        map(
            tuple((
                preceded(whitespace0, parse_symbol),
                delimited(
                    char('('),
                    separated_list0(delimited(whitespace0, char(','), whitespace0), parse_symbol),
                    char(')'),
                ),
                delimited(whitespace0, char('='), whitespace0),
                terminated(parse_expr, whitespace0),
            )),
            |(name, params, _, body)| ast::Stmt::FuncDef { name, params, body },
        ),
        map(
            tuple((
                delimited(whitespace0, parse_symbol, whitespace0),
                char('='),
                delimited(whitespace0, parse_expr, whitespace0),
            )),
            |(name, _, value)| ast::Stmt::Assignment { name, value },
        ),
        map(delimited(whitespace0, parse_expr, whitespace0), |expr| {
            ast::Stmt::ExprStmt(expr)
        }),
    ))(input)
}

pub fn parse_expr(input: &str) -> IResult<&str, ast::Expr> {
    let (input, first_term) = parse_term(input)?;
    // I cannot for the life of me figure out why I need to bind this or clone first_term but here we are.
    let x = fold_many0(
        tuple((parse_addop, parse_term)),
        || first_term.clone(),
        |acc, (op, term)| ast::Expr::BinaryExpr {
            lhs: Box::new(acc),
            rhs: Box::new(term),
            op,
        },
    )(input);
    x
}

fn parse_term(input: &str) -> IResult<&str, ast::Expr> {
    let (input, first_factor) = parse_unary_expr(input)?;
    let x = fold_many0(
        tuple((parse_mulop, parse_unary_expr)),
        || first_factor.clone(),
        |acc, (op, factor)| ast::Expr::BinaryExpr {
            lhs: Box::new(acc),
            rhs: Box::new(factor),
            op,
        },
    )(input);
    x
}

fn parse_unary_expr(input: &str) -> IResult<&str, ast::Expr> {
    alt((
        map(
            tuple((parse_unop, preceded(whitespace0, parse_expr))),
            |(op, expr)| ast::Expr::UnaryExpr {
                op,
                data: Box::new(expr),
            },
        ),
        parse_exponent,
    ))(input)
}

fn parse_unop(input: &str) -> IResult<&str, ast::UnaryOp> {
    map(char('-'), |_| ast::UnaryOp::Negate)(input)
}

fn parse_exponent(input: &str) -> IResult<&str, ast::Expr> {
    let (input, first_base) = parse_parens(input)?;
    let x = fold_many0(
        tuple((char('^'), parse_exponent)),
        || first_base.clone(),
        |base, (_, expt)| ast::Expr::BinaryExpr {
            lhs: Box::new(base),
            rhs: Box::new(expt),
            op: ast::BinaryOp::Power,
        },
    )(input);
    x
}

fn parse_parens(input: &str) -> IResult<&str, ast::Expr> {
    alt((
        delimited(
            preceded(whitespace0, char('(')),
            parse_expr,
            terminated(char(')'), whitespace0),
        ),
        //parse_blockexpr,
        parse_function_call,
        map(parse_atom, |atom| ast::Expr::AtomExpr(atom)),
    ))(input)
}

// fn parse_blockexpr(input: &str) -> IResult<&str, ast::Expr> {
//     map(
//         delimited(
//             preceded(whitespace0, char('{')),
//             tuple((many0(parse_stmt), parse_expr)),
//             terminated(char('}'), whitespace0),
//         ),
//         |(stmts, expr)| ast::Expr::BlockExpr {
//             stmts,
//             final_expr: Box::new(expr),
//         },
//     )(input)
// }

fn parse_function_call(input: &str) -> IResult<&str, ast::Expr> {
    map(
        tuple((
            parse_symbol,
            delimited(char('('), separated_list0(char(','), parse_expr), char(')')),
        )),
        |(function, args)| ast::Expr::FunctionCall { function, args },
    )(input)
}

fn parse_addop(input: &str) -> IResult<&str, ast::BinaryOp> {
    delimited(
        whitespace0,
        map(alt((char('+'), char('-'))), |c: char| match c {
            '+' => ast::BinaryOp::Plus,
            '-' => ast::BinaryOp::Minus,
            _ => unreachable!(),
        }),
        whitespace0,
    )(input)
}

fn parse_mulop(input: &str) -> IResult<&str, ast::BinaryOp> {
    delimited(
        whitespace0,
        map(alt((char('*'), char('/'))), |c: char| match c {
            '*' => ast::BinaryOp::Times,
            '/' => ast::BinaryOp::Divide,
            _ => unreachable!(),
        }),
        whitespace0,
    )(input)
}

fn parse_atom(input: &str) -> IResult<&str, ast::Atom> {
    delimited(
        whitespace0,
        alt((
            map(parse_number, |num| ast::Atom::Num(num)),
            map(parse_symbol, |sym| ast::Atom::Symbol(sym)),
        )),
        whitespace0,
    )(input)
}

fn recognize_number(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        opt(alt((char('+'), char('-')))),
        alt((
            map(tuple((digit1, opt(pair(char('.'), opt(digit1))))), |_| ()),
            map(tuple((char('.'), digit1)), |_| ()),
        )),
        opt(tuple((
            alt((char('e'), char('E'))),
            opt(alt((char('+'), char('-')))),
            cut(digit1),
        ))),
        // opt(char('i')),
    )))(input)
}

fn parse_number(input: &str) -> IResult<&str, Number> {
    map(recognize_number, |s: &str| {
        BigFloat::parse(s, Radix::Dec, PREC, RM)
    })(input)
}

fn parse_symbol(input: &str) -> IResult<&str, String> {
    map(
        recognize(tuple((
            satisfy(|c| is_symbol_character(c) && !c.is_ascii_digit()),
            take_while(is_symbol_character),
        ))),
        |s: &str| s.to_string(),
    )(input)
}

fn is_symbol_character(c: char) -> bool {
    c.is_alphanumeric()
}

#[cfg(test)]
mod tests {
    use crate::{context::Context, eval, PREC};

    use super::*;

    #[test]
    fn test_parse_number() {
        recognize_number("123").unwrap();
        recognize_number("123").unwrap();
        recognize_number("123.456").unwrap();
        recognize_number("123E10").unwrap();
        recognize_number("-12.45E-10").unwrap();

        let (_rest, num) = parse_number("123").unwrap();
        assert_eq!(num, BigFloat::from_f64(123_f64, PREC));
        let (_rest, num) = parse_number("10e10").unwrap();
        assert_eq!(num, BigFloat::from_f64(10e10_f64, PREC));
        let (_rest, num) = parse_number("-12.45E-10").unwrap();
        assert_eq!(num, BigFloat::parse("-12.45e-10", Radix::Dec, PREC, RM));
    }

    #[test]
    fn test_parse_expr() {
        let (_rest, expr) = parse_expr("123 + 456 + 7").unwrap();

        let ctx = Context::new();

        assert_eq!(
            eval::eval_expr(&expr, &ctx).unwrap(),
            BigFloat::from_f64(123_f64 + 456_f64 + 7_f64, PREC)
        );

        let (_rest, expr) = parse_expr("sqrt(1) + 3").unwrap();
        println!("{:?}", expr);
    }

    #[test]
    fn test_parse_fn_call() {
        let (_rest, _expr) = parse_function_call("g(  x , y)").unwrap();
    }

    #[test]
    fn test_parse_stmt() {
        let (_rest, _stmt) = parse_stmt("sqrt(1) + 2 * 3;").unwrap();
    }

    // #[test]
    // fn test_parse_block() {
    //     let (_rest, _expr) = parse_blockexpr("{3; 4}").unwrap();
    //     let (_rest, _other) = parse_blockexpr("{g = 2; 5; 1}").unwrap();
    //     let (_, _) = parse_stmt("    g   =    3  ;").unwrap();
    //     let (_, _) = parse_expr("{ g    =    3;    g} ").unwrap();
    //     let (_, expr) = parse_expr("{g=3;g}").unwrap();
    //     println!("{:?}", expr);
    //     let ctx = Context::new();
    //     assert_eq!(
    //         eval_expr(&expr, &ctx).unwrap(),
    //         BigFloat::from_f64(3_f64, PREC)
    //     )
    // }

    #[test]
    fn test_parse_fn_def() {
        let (_rest, _def) = parse_stmt("f(x) = x + 1;").unwrap();
        let (_rest, _def) = parse_stmt("f(x) = sqrt(x) + 1;").unwrap();
    }

    #[test]
    fn test_parse_stmt_list() {
        let (rest, stmts) = parse_stmt_list("x=5\n1+2").unwrap();
        assert_eq!(stmts.len(), 2);
        assert!(rest.is_empty());
    }
}
