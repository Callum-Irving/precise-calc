use astro_float::BigFloat;

use crate::{context::Context, CalcError, CalcResult, Number, PREC, RM};

/// Take the square root of a number.
pub fn sqrt(args: &[Number], _ctx: &Context) -> CalcResult {
    Ok(args[0].sqrt(PREC, RM))
}

/// Take the natural logarithm of a number.
pub fn ln(args: &[Number], _ctx: &Context) -> CalcResult {
    let mut consts = astro_float::Consts::new().map_err(|_| CalcError::Unknown)?;
    Ok(args[0].ln(PREC, RM, &mut consts))
}

/// Take the base 10 logarithm of a number.
pub fn log(args: &[Number], _ctx: &Context) -> CalcResult {
    let mut consts = astro_float::Consts::new().map_err(|_| CalcError::Unknown)?;
    Ok(args[0].log10(PREC, RM, &mut consts))
}

/// Return sine of arg.
pub fn sin(args: &[Number], _ctx: &Context) -> CalcResult {
    let mut consts = astro_float::Consts::new().map_err(|_| CalcError::Unknown)?;

    // Hack to round k*pi to 0
    // TODO: Use some sort of epsilon based on precision of arg rather than checking equality.
    let res = if args[0].rem(&consts.pi(PREC, RM)).abs() == BigFloat::from_f32(0.0, PREC) {
        BigFloat::from_f32(0.0, PREC)
    } else {
        args[0].sin(PREC, RM, &mut consts)
    };

    Ok(res)
}

/// Return cosine of arg.
pub fn cos(args: &[Number], _ctx: &Context) -> CalcResult {
    let mut consts = astro_float::Consts::new().map_err(|_| CalcError::Unknown)?;

    // Hack to round pi(k+1/2) to 0
    // TODO: Use some sort of epsilon based on precision of arg rather than checking equality.
    let res = if args[0]
        .rem(&consts.pi(PREC, RM))
        .div(&consts.pi(PREC, RM), PREC, RM)
        .abs()
        == BigFloat::from_f32(0.5, PREC)
    {
        BigFloat::from_f32(0.0, PREC)
    } else {
        args[0].cos(PREC, RM, &mut consts)
    };

    Ok(res)
}

/// Return tangent of arg.
pub fn tan(args: &[Number], _ctx: &Context) -> CalcResult {
    let mut consts = astro_float::Consts::new().map_err(|_| CalcError::Unknown)?;
    Ok(args[0].tan(PREC, RM, &mut consts))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trig() {
        let mut consts = astro_float::Consts::new().unwrap();
        let ctx = Context::new();
        let pi = consts.pi(PREC, RM);
        let two = BigFloat::from_f32(2.0, PREC);
        let two_pi = pi.mul(&two, PREC, RM);
        let pi_by_2 = pi.div(&two, PREC, RM);
        println!("{}", sin(&[consts.pi(PREC, RM)], &ctx).unwrap());
        println!("{}", sin(&[two_pi], &ctx).unwrap());
        println!("{}", cos(&[pi_by_2], &ctx).unwrap());
    }
}
