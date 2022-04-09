use crate::v1::eval::Error::WrongNumberOfArguments;
use crate::v1::expr::{Atom, Expression};
use std::collections::HashMap;

pub struct Context {
    functions: HashMap<String, Box<dyn FnMut(&mut Context, &str, &Expression) -> Result<Expression, Error>>>,
    variable_provider: Option<Box<dyn VariableProvider>>,
    method_dispatcher: Option<Box<dyn MethodDispatcher>>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            functions: HashMap::new(),
            variable_provider: None,
            method_dispatcher: None,
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

    pub fn register_function(
        &mut self,
        symbol: &str,
        f: impl FnMut(&mut Context, &str, &Expression) -> Result<Expression, Error> + 'static,
    ) {
        self.functions.insert(symbol.to_string(), Box::new(f));
    }

    pub fn set_variable_provider(&mut self, provider: Box<dyn VariableProvider>) {
        self.variable_provider = Some(provider);
    }

    pub fn set_method_dispatcher(&mut self, dispatcher: Box<dyn MethodDispatcher>) {
        self.method_dispatcher = Some(dispatcher);
    }

    fn get_variable(&self, symbol: &str) -> Result<Expression, Error> {
        if symbol == "t" {
            return Ok(Expression::t());
        }
        self.variable_provider
            .as_ref()
            .and_then(|p| p.get(symbol))
            .ok_or_else(|| Error::VoidVariable(symbol.to_string()))
    }

    fn call(&mut self, symbol: &str, cdr: &Expression) -> Result<Expression, Error> {
        // (symbol reference cddr...) => foreign method dispatch
        if let Expression::Cons(cdr) = cdr {
            if let Expression::Atom(a @ Atom::Reference(_)) = cdr.car() {
                return match &self.method_dispatcher {
                    Some(d) => d.dispatch(symbol, a.clone(), cdr.cdr()),
                    None => Err(Error::VoidFunction),
                };
            }
        }
        // dispatch registered functions
        if let Some((s, mut f)) = self.functions.remove_entry(symbol) {
            let result = f(self, symbol, cdr);
            self.functions.insert(s, f);
            return result;
        }
        // dispatch builtins
        self.call_builtin(symbol, cdr)
    }

    fn call_builtin(&mut self, symbol: &str, cdr: &Expression) -> Result<Expression, Error> {
        match symbol {
            "and" | "&" => self.op_and(cdr),
            "or" | "|" => self.op_or(cdr),
            "not" | "!" => self.op_not(cdr),
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

    fn op_and(&mut self, cdr: &Expression) -> Result<Expression, Error> {
        let mut last = Expression::t();
        for expr in cdr.iter() {
            last = self.evaluate(expr)?;
            if last.is_nil() {
                break;
            }
        }
        Ok(last)
    }

    fn op_or(&mut self, cdr: &Expression) -> Result<Expression, Error> {
        let mut last = Expression::Atom(Atom::Nil);
        for expr in cdr.iter() {
            last = self.evaluate(expr)?;
            if !last.is_nil() {
                break;
            }
        }
        Ok(last)
    }

    fn op_not(&mut self, cdr: &Expression) -> Result<Expression, Error> {
        if let Some(first) = cdr.iter().next() {
            Ok(self.evaluate(first)?.not())
        } else {
            Err(WrongNumberOfArguments)
        }
    }
}

#[derive(Debug)]
pub enum Error {
    VoidFunction,
    InvalidFunction,
    VoidVariable(String),
    WrongNumberOfArguments,
}

pub trait VariableProvider {
    fn get(&self, symbol: &str) -> Option<Expression>;
}

pub trait MethodDispatcher {
    fn dispatch(&self, symbol: &str, receiver: Atom, cddr: &Expression) -> Result<Expression, Error>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::v1::expr::Cons;

    #[test]
    fn equals_equal() {
        let expr = Cons::new(
            Box::new(Atom::Symbol("equals".to_string()).into()),
            Box::new(Cons::new(Box::new(Atom::Integer(1).into()), Box::new(Atom::Integer(1).into())).into()),
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
            Box::new(Cons::new(Box::new(Atom::Integer(1).into()), Box::new(Atom::Integer(2).into())).into()),
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
            Box::new(Cons::new(Box::new(Atom::Integer(1).into()), Box::new(Atom::Integer(1).into())).into()),
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
            Box::new(Cons::new(Box::new(Atom::Integer(1).into()), Box::new(Atom::Integer(2).into())).into()),
        )
        .into();
        let mut context = Context::new();
        match context.evaluate(&expr) {
            Ok(ret) => assert_eq!(Expression::t(), ret),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn and_empty() {
        let expr = Cons::new(
            Box::new(Atom::Symbol("and".to_string()).into()),
            Box::new(Atom::Nil.into()),
        )
        .into();
        let mut context = Context::new();
        match context.evaluate(&expr) {
            Ok(ret) => assert_eq!(Expression::t(), ret),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn and_t_t() {
        let expr = Cons::new(
            Box::new(Atom::Symbol("and".to_string()).into()),
            Box::new(
                Cons::new(
                    Box::new(Atom::Symbol("t".to_string()).into()),
                    Box::new(Atom::Symbol("t".to_string()).into()),
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
    fn or_empty() {
        let expr = Cons::new(
            Box::new(Atom::Symbol("or".to_string()).into()),
            Box::new(Atom::Nil.into()),
        )
        .into();
        let mut context = Context::new();
        match context.evaluate(&expr) {
            Ok(ret) => assert_eq!(Expression::Atom(Atom::Nil), ret),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn or_t_nil() {
        let expr = Cons::new(
            Box::new(Atom::Symbol("or".to_string()).into()),
            Box::new(
                Cons::new(
                    Box::new(Atom::Symbol("t".to_string()).into()),
                    Box::new(Atom::Nil.into()),
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
    fn not_t_is_nil() {
        let expr = Cons::new(
            Box::new(Atom::Symbol("not".to_string()).into()),
            Box::new(
                Cons::new(
                    Box::new(Atom::Symbol("t".to_string()).into()),
                    Box::new(Atom::Nil.into()),
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
    fn not_nil_is_t() {
        let expr = Cons::new(
            Box::new(Atom::Symbol("not".to_string()).into()),
            Box::new(Cons::new(Box::new(Atom::Nil.into()), Box::new(Atom::Nil.into())).into()),
        )
        .into();
        let mut context = Context::new();
        match context.evaluate(&expr) {
            Ok(ret) => assert_eq!(Expression::t(), ret),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn call_foreign_method() {
        struct Dispatcher {}
        impl MethodDispatcher for Dispatcher {
            fn dispatch(&self, symbol: &str, receiver: Atom, cddr: &Expression) -> Result<Expression, Error> {
                assert_eq!("test_method", symbol);
                assert_eq!(Atom::Reference(1024), receiver);

                let args: Expression = Cons::new(Box::new(Atom::Integer(1).into()), Box::new(Atom::Nil.into())).into();
                assert_eq!(&args, cddr);

                Ok(Atom::Nil.into())
            }
        }

        let dispatcher = Box::new(Dispatcher {});
        let expr = Cons::new(
            Box::new(Atom::Symbol("test_method".to_string()).into()),
            Box::new(
                Cons::new(
                    Box::new(Atom::Reference(1024).into()),
                    Box::new(Cons::new(Box::new(Atom::Integer(1).into()), Box::new(Atom::Nil.into())).into()),
                )
                .into(),
            ),
        )
        .into();
        let mut context = Context::new();
        context.set_method_dispatcher(dispatcher);
        match context.evaluate(&expr) {
            Ok(ret) => assert_eq!(Expression::Atom(Atom::Nil), ret),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn call_foreign_function() {
        let expr = Cons::new(
            Box::new(Atom::Symbol("test_function".to_string()).into()),
            Box::new(Cons::new(Box::new(Atom::Integer(1).into()), Box::new(Atom::Nil.into())).into()),
        )
        .into();
        let mut context = Context::new();
        context.register_function("test_function", |_, symbol, cddr| {
            assert_eq!(symbol, "test_function");
            let args: Expression = Cons::new(Box::new(Atom::Integer(1).into()), Box::new(Atom::Nil.into())).into();
            assert_eq!(&args, cddr);
            Ok(Expression::Atom(Atom::Nil))
        });
        match context.evaluate(&expr) {
            Ok(ret) => assert_eq!(Expression::Atom(Atom::Nil), ret),
            Err(_) => assert!(false),
        }
    }
}
