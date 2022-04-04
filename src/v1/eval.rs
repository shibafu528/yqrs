use crate::v1::expr::{Atom, Expression};

pub struct Context {
    variable_provider: Option<Box<dyn VariableProvider>>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            variable_provider: None,
        }
    }

    pub fn evaluate(&mut self, expr: &Expression) -> Result<Expression, Error> {
        match expr {
            Expression::Atom(Atom::Symbol(sym)) => self.get_variable(sym),
            Expression::Atom(_) => Ok(expr.clone()),
            Expression::Cons(cons) => match cons.car() {
                Expression::Atom(Atom::Symbol(sym)) => self.call(sym, cons.cdr()),
                Expression::Atom(Atom::Nil) => Err(Error::VoidFunction),
                _ => Err(Error::InvalidFunction),
            },
        }
    }

    pub fn set_variable_provider(&mut self, provider: Box<dyn VariableProvider>) {
        self.variable_provider = Some(provider);
    }

    fn get_variable(&self, symbol: &str) -> Result<Expression, Error> {
        self.variable_provider
            .as_ref()
            .and_then(|p| p.get(symbol))
            .ok_or_else(|| Error::VoidVariable(symbol.to_string()))
    }

    fn call(&mut self, symbol: &str, cdr: &Expression) -> Result<Expression, Error> {
        match symbol {
            "equals" | "eq" | "=" | "==" => self.op_equals(cdr),
            "noteq" | "neq" | "!=" | "/=" => self.op_noteq(cdr),
            _ => Err(Error::VoidFunction),
        }
    }

    fn op_equals(&mut self, cdr: &Expression) -> Result<Expression, Error> {
        match cdr {
            Expression::Atom(Atom::Nil) => Ok(Expression::Atom(Atom::Nil)),
            Expression::Atom(_) => Ok(Expression::t()),
            Expression::Cons(cons) => {
                let car = self.evaluate(cons.car())?;
                let mut cdr = cons.cdr();
                while !cdr.is_nil() {
                    let cadr = match cdr {
                        Expression::Cons(c) => {
                            cdr = c.cdr(); // cddr
                            c.car()
                        }
                        atom @ Expression::Atom(_) => {
                            cdr = &Expression::Atom(Atom::Nil);
                            atom
                        }
                    };
                    if car != self.evaluate(cadr)? {
                        return Ok(Expression::Atom(Atom::Nil));
                    }
                }
                Ok(Expression::t())
            }
        }
    }

    fn op_noteq(&mut self, cdr: &Expression) -> Result<Expression, Error> {
        if self.op_equals(cdr)?.is_nil() {
            Ok(Expression::t())
        } else {
            Ok(Expression::Atom(Atom::Nil))
        }
    }
}

#[derive(Debug)]
pub enum Error {
    VoidFunction,
    InvalidFunction,
    VoidVariable(String),
}

pub trait VariableProvider {
    fn get(&self, symbol: &str) -> Option<Expression>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::v1::expr::Cons;

    #[test]
    fn equals_equal() {
        let expr = Cons::new(
            Box::new(Atom::Symbol("equals".to_string()).into()),
            Box::new(
                Cons::new(
                    Box::new(Atom::Integer(1).into()),
                    Box::new(Atom::Integer(1).into()),
                )
                .into(),
            ),
        )
        .into();
        let mut context = Context::new();
        match context.evaluate(&expr) {
            Ok(ret) => assert_eq!(Expression::t(), ret),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn equals_not_equal() {
        let expr = Cons::new(
            Box::new(Atom::Symbol("equals".to_string()).into()),
            Box::new(
                Cons::new(
                    Box::new(Atom::Integer(1).into()),
                    Box::new(Atom::Integer(2).into()),
                )
                .into(),
            ),
        )
        .into();
        let mut context = Context::new();
        match context.evaluate(&expr) {
            Ok(ret) => assert_eq!(Expression::Atom(Atom::Nil), ret),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn noteq_equal() {
        let expr = Cons::new(
            Box::new(Atom::Symbol("noteq".to_string()).into()),
            Box::new(
                Cons::new(
                    Box::new(Atom::Integer(1).into()),
                    Box::new(Atom::Integer(1).into()),
                )
                .into(),
            ),
        )
        .into();
        let mut context = Context::new();
        match context.evaluate(&expr) {
            Ok(ret) => assert_eq!(Expression::Atom(Atom::Nil), ret),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn noteq_not_equal() {
        let expr = Cons::new(
            Box::new(Atom::Symbol("noteq".to_string()).into()),
            Box::new(
                Cons::new(
                    Box::new(Atom::Integer(1).into()),
                    Box::new(Atom::Integer(2).into()),
                )
                .into(),
            ),
        )
        .into();
        let mut context = Context::new();
        match context.evaluate(&expr) {
            Ok(ret) => assert_eq!(Expression::t(), ret),
            Err(_) => assert!(false),
        }
    }
}
