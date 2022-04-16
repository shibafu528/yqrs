//! Original YQ compatible operands

use crate::v1::eval::*;
use crate::v1::expr::Expression;

/// # Compatibility note
/// 1引数で呼び出された場合、その引数が `t` であれば真を返します。
pub fn op_equals(context: &mut Context, cdr: &Expression) -> Result<Expression, Error> {
    let args: Vec<&Expression> = cdr.iter().collect();
    match args.len() {
        0 => Err(error_wrong_number_of_arguments()),
        1 => {
            let first = context.evaluate(args.first().unwrap())?;
            if first == Expression::t() {
                Ok(Expression::t())
            } else {
                Ok(Expression::nil())
            }
        }
        _ => {
            let (first, rest) = args.split_first().unwrap();
            let first = context.evaluate(first)?;
            for compare in rest {
                if first != context.evaluate(compare)? {
                    return Ok(Expression::nil());
                }
            }
            Ok(Expression::t())
        }
    }
}

/// # Compatibility note
/// 1引数で呼び出された場合、その引数が `t` 以外であれば真を返します。
pub fn op_noteq(context: &mut Context, cdr: &Expression) -> Result<Expression, Error> {
    if op_equals(context, cdr)?.is_nil() {
        Ok(Expression::t())
    } else {
        Ok(Expression::Atom(Atom::Nil))
    }
}

pub fn op_and(context: &mut Context, cdr: &Expression) -> Result<Expression, Error> {
    let true_value = Expression::t();

    let args: Vec<&Expression> = cdr.iter().collect();
    if args.is_empty() {
        return Ok(true_value);
    }

    for expr in args {
        if context.evaluate(expr)? != true_value {
            return Ok(Expression::nil());
        }
    }
    Ok(true_value)
}

pub fn op_or(context: &mut Context, cdr: &Expression) -> Result<Expression, Error> {
    let true_value = Expression::t();

    let args: Vec<&Expression> = cdr.iter().collect();
    if args.is_empty() {
        return Ok(true_value);
    }

    for expr in args {
        if context.evaluate(expr)? == true_value {
            return Ok(true_value);
        }
    }
    Ok(Expression::nil())
}

pub fn op_not(context: &mut Context, cdr: &Expression) -> Result<Expression, Error> {
    if let Some(first) = cdr.iter().next() {
        let true_value = Expression::t();
        if context.evaluate(first)? == true_value {
            Ok(true_value)
        } else {
            Ok(Expression::nil())
        }
    } else {
        Err(error_wrong_number_of_arguments())
    }
}
