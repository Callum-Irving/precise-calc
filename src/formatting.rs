use std::collections::VecDeque;

use astro_float::{BigFloat, Sign};

use crate::{BASE_10_PREC, RM};

/// Round mantissa to `precision` base 10 digits.
fn round_to_digit(
    mut exponent: i32,
    mantissa: Vec<u8>,
    precision: usize,
) -> Option<(i32, Vec<u8>)> {
    let mut mantissa = VecDeque::from(mantissa);

    // Find index of first non-zero digit in mantissa
    let i = match mantissa.iter().enumerate().find(|&(_, &digit)| digit != 0) {
        Some((i, _)) => i,
        None => return Some((exponent, Vec::from(mantissa))),
    };

    // Get digit precision digits after i
    // TODO: Check this algorithm
    let rounding_digit = match mantissa.get(i + precision) {
        Some(digit) => digit,
        None => {
            return Some((exponent, Vec::from(mantissa)));
        }
    };
    if *rounding_digit >= 5 {
        // Round up
        // Add 1 to mantissa[i+precision-1];
        // Carry add to previous digit
        let old_len = mantissa.len();
        mantissa.truncate(i + precision);
        mantissa = add1_to_vec(mantissa)?;
        if mantissa.len() > old_len {
            exponent += 1;
        }
    } else {
        // Round down
        // Just chop off extra digits
        mantissa.truncate(i + precision);
    }

    let mantissa = Vec::from(mantissa);
    Some((exponent, mantissa))
}

fn add1_to_vec(mut digits: VecDeque<u8>) -> Option<VecDeque<u8>> {
    // Add one to last digit
    *digits.get_mut(digits.len() - 1)? += 1;

    // Perform carry
    let mut i = digits.len() - 1;
    while digits[i] == 10 {
        digits[i] = 0;

        if i == 0 {
            // Add 1 to front of vector
            digits.push_front(1);
            break;
        } else {
            digits[i - 1] += 1;
            i -= 1;
        }
    }

    Some(digits)
}

fn format_num(sign: astro_float::Sign, mantissa: &[u8], mut expt: i32) -> String {
    // Remove trailing zeros
    let mut mantissa = Vec::from(mantissa);

    // Handle 0 case
    if mantissa.is_empty() {
        return "0".to_string();
    }

    while *mantissa.last().unwrap() == 0 {
        mantissa.pop();
    }

    if expt.abs() > 3 {
        // The way astro_float does exponents is weird
        // 0.123e1 = 1.23 (astro_float way)
        // 1.23e0 = 1.23 (my way)
        expt -= 1;

        // Scientific notation
        let mut bytes: Vec<u8> = Vec::new();

        // Handle negative sign
        if sign == Sign::Neg {
            bytes.reserve(mantissa.len() + expt.abs() as usize / 10 + 3); // number of digits + exponent digits + 'e' + '.' + '-'
            bytes.push(b'-');
        } else {
            bytes.reserve(mantissa.len() + expt.abs() as usize / 10 + 2);
        }

        // Push first digit
        bytes.push(mantissa[0] + 48);

        // Add digits after decimal place
        bytes.push(b'.');
        if mantissa.len() > 1 {
            for d in &mantissa[1..] {
                bytes.push(d + 48);
            }
        } else {
            // Add '0'
            bytes.push(b'0');
        }

        // Add exponent
        bytes.push(b'e');
        // TODO: Allocate space for this negative at start
        if expt < 0 {
            bytes.push(b'-');
        }
        let mut exponent_digits: Vec<u8> =
            expt.abs().to_string().chars().map(|d| d as u8).collect();
        bytes.append(&mut exponent_digits);

        String::from_utf8(bytes).unwrap()
    } else {
        // Normal format
        let mut bytes = Vec::new();

        // First, sign
        if sign == Sign::Neg {
            bytes.push(b'-');
        }

        // Expt plus one digits
        if expt >= mantissa.len() as i32 {
            // TODO: Preallocate vector

            // Add digits
            for d in &mantissa {
                bytes.push(d + 48);
            }

            // Add trailing zeros
            for _ in 0..(expt - mantissa.len() as i32) {
                bytes.push(b'0');
            }
        } else if expt <= 0 {
            // TODO: Preallocate vector

            bytes.push(b'0');
            bytes.push(b'.');
            for _ in 0..expt.abs() {
                bytes.push(b'0');
            }
            for d in mantissa {
                bytes.push(d + 48);
            }
        } else {
            // TODO: Preallocate vector

            // Add digits before decimal
            for d in &mantissa[0..expt as usize] {
                bytes.push(d + 48);
            }

            // Add decimal place
            bytes.push(b'.');

            // Add digits after decimal
            for d in &mantissa[expt as usize..] {
                bytes.push(d + 48);
            }
        }

        String::from_utf8(bytes).unwrap()
    }
}

pub fn float_to_string(num: &BigFloat) -> String {
    let (s, m, e) = num.convert_to_radix(astro_float::Radix::Dec, RM).unwrap();
    let (e, m) = round_to_digit(e, m, BASE_10_PREC).unwrap();
    format_num(s, &m, e)
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use astro_float::{BigFloat, Radix};

    use crate::{
        formatting::{add1_to_vec, format_num, round_to_digit},
        PREC, RM,
    };

    #[test]
    fn test_add1() {
        let digits = VecDeque::from([0, 0, 0, 0, 9]);
        let digits = add1_to_vec(digits).unwrap();
        assert_eq!(digits, vec![0, 0, 0, 1, 0]);

        let digits = VecDeque::from([0, 0, 0, 0, 5]);
        let digits = add1_to_vec(digits).unwrap();
        assert_eq!(digits, vec![0, 0, 0, 0, 6]);

        let digits = VecDeque::from([9, 9, 9]);
        let digits = add1_to_vec(digits).unwrap();
        assert_eq!(digits, vec![1, 0, 0, 0]);
    }

    #[test]
    fn test_round() {
        // Precision is log_10(2^PREC)

        let (_s, m, e) = BigFloat::from_f64(0.1 + 0.2, PREC)
            .convert_to_radix(astro_float::Radix::Dec, RM)
            .unwrap();
        let (e, m) = round_to_digit(e, m, 15).unwrap();
        assert_eq!(m, vec![3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        assert_eq!(e, 0);

        let (_s, m, e) = BigFloat::from_f64(0.3, PREC)
            .convert_to_radix(astro_float::Radix::Dec, RM)
            .unwrap();
        let (e, m) = round_to_digit(e, m, 15).unwrap();
        assert_eq!(m, vec![3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        assert_eq!(e, 0);
    }

    #[test]
    fn test_format() {
        let (s, m, e) = BigFloat::from_f64(0.3, PREC)
            .convert_to_radix(Radix::Dec, RM)
            .unwrap();
        let (e, m) = round_to_digit(e, m, 15).unwrap();
        let str_num = format_num(s, &m, e);
        assert_eq!(str_num, "0.3");
    }
}
