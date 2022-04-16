use crate::v1::expr::{Atom, Cons, Expression};
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
                Expression::Atom(Atom::Nil) => Err(error_void_function()),
                _ => Err(error_invalid_function()),
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
        match symbol {
            "nil" | "false" | "f" | "False" | "FALSE" => Ok(Expression::Atom(Atom::Nil)),
            "t" | "true" | "True" | "TRUE" => Ok(Expression::t()),
            _ => self
                .variable_provider
                .as_ref()
                .and_then(|p| p.get(symbol))
                .ok_or_else(|| error_void_variable(symbol.to_string())),
        }
    }

    fn call(&mut self, symbol: &str, cdr: &Expression) -> Result<Expression, Error> {
        // (symbol reference cddr...) => foreign method dispatch
        if let Expression::Cons(cdr) = cdr {
            if let Expression::Atom(a @ Atom::Reference(_)) = cdr.car() {
                return match &self.method_dispatcher {
                    Some(d) => d.dispatch(symbol, a.clone(), cdr.cdr()),
                    None => Err(error_void_function()),
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
            "contains" | "in" => self.op_contains(cdr),
            "list" => self.op_list(cdr),
            "quote" => Ok(cdr.clone()),
            "+" => self.op_add(cdr),
            "-" => self.op_subtract(cdr),
            "*" => self.op_multiply(cdr),
            "/" => self.op_divide(cdr),
            "%" | "mod" => self.op_modulo(cdr),
            _ => Err(error_void_function()),
        }
    }

    fn op_equals(&mut self, cdr: &Expression) -> Result<Expression, Error> {
        let args: Vec<&Expression> = cdr.iter().collect();
        match args.len() {
            0 => Err(error_wrong_number_of_arguments()),
            1 => Ok(Expression::t()),
            _ => {
                let (first, rest) = args.split_first().unwrap();
                let first = self.evaluate(first)?;
                for compare in rest {
                    if first != self.evaluate(compare)? {
                        return Ok(Expression::nil());
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
            Err(error_wrong_number_of_arguments())
        }
    }

    fn op_contains(&mut self, cdr: &Expression) -> Result<Expression, Error> {
        let mut iter = cdr.iter();
        let haystack = self.evaluate(iter.next().ok_or_else(|| error_wrong_number_of_arguments())?)?;
        let needle = self.evaluate(iter.next().ok_or_else(|| error_wrong_number_of_arguments())?)?;
        match (haystack, needle) {
            (
                Expression::Atom(Atom::Symbol(h)) | Expression::Atom(Atom::String(h)),
                Expression::Atom(Atom::Symbol(n)) | Expression::Atom(Atom::String(n)),
            ) => {
                if h.contains(&n) {
                    Ok(Expression::t())
                } else {
                    Ok(Expression::Atom(Atom::Nil))
                }
            }
            (list @ Expression::Cons(_), needle) => {
                if list.iter().any(|item| *item == needle) {
                    Ok(Expression::t())
                } else {
                    Ok(Expression::Atom(Atom::Nil))
                }
            }
            _ => Ok(Expression::Atom(Atom::Nil)),
        }
    }

    fn op_list(&mut self, cdr: &Expression) -> Result<Expression, Error> {
        let mut list = vec![];
        for expr in cdr.iter() {
            list.push(self.evaluate(expr)?);
        }
        Ok(list.into_iter().rfold(Expression::Atom(Atom::Nil), |cdr, car| {
            Expression::Cons(Cons::new(Box::new(car), Box::new(cdr)))
        }))
    }

    fn op_add(&mut self, cdr: &Expression) -> Result<Expression, Error> {
        let mut acc = 0;
        for expr in cdr.iter() {
            acc += self.evaluate(expr).and_then(expr_to_int)?;
        }
        Ok(Expression::Atom(Atom::Integer(acc)))
    }

    fn op_subtract(&mut self, cdr: &Expression) -> Result<Expression, Error> {
        let mut acc = 0;
        for expr in cdr.iter() {
            acc -= self.evaluate(expr).and_then(expr_to_int)?;
        }
        Ok(Expression::Atom(Atom::Integer(acc)))
    }

    fn op_multiply(&mut self, cdr: &Expression) -> Result<Expression, Error> {
        let mut acc = 1;
        for expr in cdr.iter() {
            acc *= self.evaluate(expr).and_then(expr_to_int)?;
        }
        Ok(Expression::Atom(Atom::Integer(acc)))
    }

    fn op_divide(&mut self, cdr: &Expression) -> Result<Expression, Error> {
        let mut iter = cdr.iter();
        let mut acc = iter
            .next()
            .ok_or_else(|| error_wrong_type_argument())
            .and_then(|expr| self.evaluate(expr))
            .and_then(expr_to_int)?;
        for expr in iter {
            acc /= self.evaluate(expr).and_then(expr_to_int)?;
        }
        Ok(Expression::Atom(Atom::Integer(acc)))
    }

    fn op_modulo(&mut self, cdr: &Expression) -> Result<Expression, Error> {
        let mut iter = cdr.iter();
        let mut acc = iter
            .next()
            .ok_or_else(|| error_wrong_type_argument())
            .and_then(|expr| self.evaluate(expr))
            .and_then(expr_to_int)?;
        for expr in iter {
            acc %= self.evaluate(expr).and_then(expr_to_int)?;
        }
        Ok(Expression::Atom(Atom::Integer(acc)))
    }
}

pub type Error = Expression;

pub trait VariableProvider {
    fn get(&self, symbol: &str) -> Option<Expression>;
}

pub trait MethodDispatcher {
    fn dispatch(&self, symbol: &str, receiver: Atom, cddr: &Expression) -> Result<Expression, Error>;
}

fn expr_to_int(expr: Expression) -> Result<i64, Error> {
    match expr {
        Expression::Atom(Atom::Integer(i)) => Ok(i),
        Expression::Atom(Atom::Float(f)) => Ok(f as i64),
        Expression::Atom(Atom::String(s)) | Expression::Atom(Atom::Symbol(s)) => {
            s.as_str().parse().map_err(|_| error_wrong_type_argument())
        }
        _ => Err(error_wrong_type_argument()),
    }
}

fn error(typ: &str) -> Error {
    error_with_data(typ, Expression::nil())
}

fn error_with_data(typ: &str, data: Expression) -> Error {
    Expression::Cons(Cons::from(Atom::symbol(typ).into(), data))
}

fn error_void_function() -> Error {
    error("void-function")
}

fn error_invalid_function() -> Error {
    error("invalid-function")
}

fn error_void_variable(symbol: String) -> Error {
    error_with_data("void-variable", Atom::Symbol(symbol).into())
}

fn error_wrong_number_of_arguments() -> Error {
    error("wrong-number-of-arguments")
}

fn error_wrong_type_argument() -> Error {
    error("wrong-type-argument")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::v1::expr::Cons;
    use crate::v1::macros::*;

    #[test]
    fn equals_equal() {
        let expr = yq!((equals 1 1));
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
    fn contains_string() {
        let expr = yq!((contains "lorem ipsum" "ore"));
        let mut context = Context::new();
        match context.evaluate(&expr) {
            Ok(ret) => assert_eq!(Expression::t(), ret),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn list() {
        let expr = yq!((list 1 2 3 4 5));
        let expect = yq!((1 2 3 4 5));
        let mut context = Context::new();
        match context.evaluate(&expr) {
            Ok(ret) => assert_eq!(expect, ret),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn quote() {
        let expr = yq!((quote foo (bar) baz));
        let expect = yq!((foo (bar) baz));
        let mut context = Context::new();
        match context.evaluate(&expr) {
            Ok(ret) => assert_eq!(expect, ret),
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn call_foreign_method() {
        struct Dispatcher {}
        impl MethodDispatcher for Dispatcher {
            fn dispatch(&self, symbol: &str, receiver: Atom, cddr: &Expression) -> Result<Expression, Expression> {
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
