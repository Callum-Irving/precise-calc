use std::io;
use std::io::Write;

use atty::Stream;

use precise_calc::context::Context;
use precise_calc::eval::CalcValue;
use precise_calc::{ast, eval, parser, CalcError};

fn read() -> Result<ast::Stmt, CalcError> {
    if atty::is(Stream::Stdin) {
        print!("calculator> ");
    }

    io::stdout().flush().map_err(|_| CalcError::IOError)?;
    let mut buf = String::new();
    io::stdin()
        .read_line(&mut buf)
        .map_err(|_| CalcError::IOError)?;

    if buf.trim() == "exit" {
        std::process::exit(0);
    }

    // if stdin is piped and empty then just exit with code 0 without outputting anything
    if !atty::is(Stream::Stdin) && buf.is_empty() {
        std::process::exit(0);
    }

    let (rest, stmt) = parser::parse_stmt(&buf).map_err(|_| CalcError::ParseError)?;

    if !rest.trim().is_empty() {
        Err(CalcError::ParseError)
    } else {
        Ok(stmt)
    }
}

fn eval(stmt: ast::Stmt, ctx: &mut Context) -> Result<CalcValue, CalcError> {
    eval::eval_stmt(&stmt, ctx)
}

fn main() {
    let mut ctx = Context::new();
    loop {
        let input = match read() {
            Ok(stmt) => stmt,
            Err(e) => {
                println!("{}", e);
                continue;
            }
        };
        match eval(input, &mut ctx) {
            Ok(val) => println!("{}", val),
            Err(e) => println!("{}", e),
        };
    }
}
