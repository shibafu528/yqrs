use crate::v1::eval;
use crate::v1::eval::{Error, MethodDispatcher, VariableProvider};
use crate::v1::expr::{Atom, Expression};
use crate::v1::lex::LexerError;
use crate::v1::parser::ParseError;
use crate::v1::query::{Query, Source};
use libc::c_char;
use std::ffi::{CStr, CString};
use std::ptr::{null, null_mut};

#[repr(C)]
pub struct StringRef {
    ptr: *const c_char,
    len: usize,
}

impl StringRef {
    fn null() -> Self {
        StringRef {
            ptr: null(),
            len: 0,
        }
    }
}

trait ToStringRef {
    fn to_string_ref(&self) -> StringRef;
}

impl ToStringRef for &str {
    fn to_string_ref(&self) -> StringRef {
        StringRef {
            ptr: self.as_bytes().as_ptr() as *const c_char,
            len: self.len(),
        }
    }
}

#[repr(C)]
pub enum ParseStatus {
    Success = 0,
    InvalidQuery,

    UnexpectedToken = 1000,
    UnexpectedEOF,
    UnparseableNumber,
    UnterminatedList,

    LexerStringIsNotClosed = 2000,
}

impl From<LexerError> for ParseStatus {
    fn from(e: LexerError) -> Self {
        match e {
            LexerError::StringIsNotClosed => ParseStatus::LexerStringIsNotClosed,
        }
    }
}

impl From<ParseError> for ParseStatus {
    fn from(e: ParseError) -> Self {
        match e {
            ParseError::LexerError(e) => e.into(),
            ParseError::UnexpectedToken(_) => ParseStatus::UnexpectedToken,
            ParseError::UnexpectedEOF => ParseStatus::UnexpectedEOF,
            ParseError::UnparseableNumber(_) => ParseStatus::UnparseableNumber,
            ParseError::UnterminatedList => ParseStatus::UnterminatedList,
        }
    }
}

type Function = extern "C" fn(
    context: *mut Context,
    symbol: *const c_char,
    cdr: *const Expression,
    result: *mut *mut Expression,
) -> EvalError;

type VariableProviderCallback = extern "C" fn(symbol: *const c_char) -> *mut Expression;

type MethodDispatcherCallback = extern "C" fn(
    symbol: *const c_char,
    receiver: *const Expression,
    cddr: *const Expression,
    result: *mut *mut Expression,
) -> EvalError;

pub struct Context {
    context: eval::Context,
    last_error: Option<eval::Error>,
}

#[repr(C)]
pub enum ExprType {
    Nil,
    Symbol,
    String,
    Integer,
    Float,
    Reference,
    Cons,
}

#[repr(C)]
pub enum EvalError {
    Success = 0,
    VoidFunction,
    InvalidFunction,
    VoidVariable,
    WrongNumberOfArguments,
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_parse(query: *const c_char, out: *mut *mut Query) -> ParseStatus {
    let query = match CStr::from_ptr(query).to_str() {
        Ok(s) => s,
        Err(_) => return ParseStatus::InvalidQuery,
    };
    match crate::v1::parser::parse(query) {
        Ok(q) => {
            *out = Box::into_raw(Box::new(q));
            ParseStatus::Success
        }
        Err(e) => e.into(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_query_get_source(
    query: *const Query,
    index: usize,
) -> *const Source {
    if query.is_null() {
        return null();
    }
    let sources = (*query).sources();
    &sources[index]
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_query_get_sources_size(query: *const Query) -> usize {
    if query.is_null() {
        return 0;
    }
    (*query).sources().len()
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_query_get_expression(query: *const Query) -> *const Expression {
    if query.is_null() {
        return null();
    }
    (*query).expression()
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_query_free(query: *mut Query) {
    if query.is_null() {
        return;
    }
    let query = Box::from_raw(query);
    drop(query);
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_source_get_class(source: *const Source) -> StringRef {
    if source.is_null() {
        return StringRef::null();
    }
    (*source).class().to_string_ref()
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_source_get_argument(source: *const Source) -> StringRef {
    if source.is_null() {
        return StringRef::null();
    }
    match (*source).argument() {
        Some(a) => a.to_string_ref(),
        None => StringRef::null(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_context_new() -> *mut Context {
    Box::into_raw(Box::new(Context {
        context: eval::Context::new(),
        last_error: None,
    }))
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_context_free(context: *mut Context) {
    if context.is_null() {
        return;
    }
    let context = Box::from_raw(context);
    drop(context);
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_context_eval(
    context: *mut Context,
    expr: *const Expression,
) -> *mut Expression {
    if context.is_null() {
        return null_mut();
    }
    (*context).last_error = None;
    match (*context).context.evaluate(&*expr) {
        Ok(expr) => Box::into_raw(Box::new(expr)),
        Err(e) => {
            (*context).last_error = Some(e);
            null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_context_register_function(
    context: *mut Context,
    symbol: *const c_char,
    function: Function,
) {
    if context.is_null() {
        return;
    }
    (*context).context.register_function(
        CStr::from_ptr(symbol).to_str().unwrap(),
        move |_, symbol, cdr| {
            let symbol = CString::new(symbol).unwrap();
            let mut result = null_mut();
            let error = function(context, symbol.as_ptr(), cdr, &mut result);

            let expr = if result.is_null() {
                Expression::Atom(Atom::Nil)
            } else {
                *unsafe { Box::from_raw(result) }
            };

            match error {
                EvalError::Success => Ok(expr),
                EvalError::VoidFunction => Err(eval::Error::VoidFunction),
                EvalError::InvalidFunction => Err(eval::Error::InvalidFunction),
                EvalError::VoidVariable => Err(eval::Error::VoidVariable("*unknown*".to_string())),
                EvalError::WrongNumberOfArguments => {
                    Err(eval::Error::WrongNumberOfArguments("".to_string()))
                }
            }
        },
    );
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_context_set_variable_provider(
    context: *mut Context,
    callback: VariableProviderCallback,
) {
    struct CVariableProvider(VariableProviderCallback);
    impl VariableProvider for CVariableProvider {
        fn get(&self, symbol: &str) -> Option<Expression> {
            let symbol = CString::new(symbol).unwrap();
            let expr = self.0(symbol.as_ptr());
            if expr.is_null() {
                None
            } else {
                let expr = unsafe { Box::from_raw(expr) };
                Some((*expr).clone())
            }
        }
    }

    if context.is_null() {
        return;
    }
    (*context)
        .context
        .set_variable_provider(Box::new(CVariableProvider(callback)));
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_context_set_method_dispatcher(
    context: *mut Context,
    callback: MethodDispatcherCallback,
) {
    struct CMethodDispatcher(MethodDispatcherCallback);
    impl MethodDispatcher for CMethodDispatcher {
        fn dispatch(
            &self,
            symbol: &str,
            receiver: Atom,
            cddr: &Expression,
        ) -> Result<Expression, Error> {
            let symbol = CString::new(symbol).unwrap();
            let receiver = Expression::Atom(receiver);
            let mut result = null_mut();
            let error = self.0(symbol.as_ptr(), &receiver, cddr, &mut result);

            let expr = if result.is_null() {
                Expression::Atom(Atom::Nil)
            } else {
                *unsafe { Box::from_raw(result) }
            };

            match error {
                EvalError::Success => Ok(expr),
                EvalError::VoidFunction => Err(eval::Error::VoidFunction),
                EvalError::InvalidFunction => Err(eval::Error::InvalidFunction),
                EvalError::VoidVariable => Err(eval::Error::VoidVariable("*unknown*".to_string())),
                EvalError::WrongNumberOfArguments => {
                    Err(eval::Error::WrongNumberOfArguments("".to_string()))
                }
            }
        }
    }

    if context.is_null() {
        return;
    }
    (*context)
        .context
        .set_method_dispatcher(Box::new(CMethodDispatcher(callback)));
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_context_get_last_error(context: *mut Context) -> EvalError {
    if context.is_null() {
        return EvalError::Success;
    }
    match &(*context).last_error {
        Some(e) => match e {
            Error::VoidFunction => EvalError::VoidFunction,
            Error::InvalidFunction => EvalError::InvalidFunction,
            Error::VoidVariable(_) => EvalError::VoidVariable,
            Error::WrongNumberOfArguments(_) => EvalError::WrongNumberOfArguments,
        },
        None => EvalError::Success,
    }
}

#[no_mangle]
pub extern "C" fn yq_v1_expression_new_nil() -> *mut Expression {
    Box::into_raw(Box::new(Expression::Atom(Atom::Nil)))
}

#[no_mangle]
pub extern "C" fn yq_v1_expression_new_t() -> *mut Expression {
    Box::into_raw(Box::new(Expression::t()))
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_expression_new_symbol(symbol: *const c_char) -> *mut Expression {
    match CStr::from_ptr(symbol).to_str() {
        Ok(symbol) => {
            let expr = Box::new(Expression::Atom(Atom::Symbol(String::from(symbol))));
            Box::into_raw(expr)
        }
        Err(_) => null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_expression_new_string(string: *const c_char) -> *mut Expression {
    match CStr::from_ptr(string).to_str() {
        Ok(string) => {
            let expr = Box::new(Expression::Atom(Atom::String(String::from(string))));
            Box::into_raw(expr)
        }
        Err(_) => null_mut(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_expression_new_integer(value: i64) -> *mut Expression {
    Box::into_raw(Box::new(Expression::Atom(Atom::Integer(value))))
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_expression_new_float(value: f64) -> *mut Expression {
    Box::into_raw(Box::new(Expression::Atom(Atom::Float(value))))
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_expression_new_reference(r#ref: u64) -> *mut Expression {
    Box::into_raw(Box::new(Expression::Atom(Atom::Reference(r#ref))))
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_expression_is_nil(expr: *const Expression) -> bool {
    if expr.is_null() {
        true
    } else {
        (*expr).is_nil()
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_expression_get_type(expr: *const Expression) -> ExprType {
    if expr.is_null() {
        return ExprType::Nil;
    }
    match *expr {
        Expression::Cons(_) => ExprType::Cons,
        Expression::Atom(Atom::Nil) => ExprType::Nil,
        Expression::Atom(Atom::Symbol(_)) => ExprType::Symbol,
        Expression::Atom(Atom::String(_)) => ExprType::String,
        Expression::Atom(Atom::Integer(_)) => ExprType::Integer,
        Expression::Atom(Atom::Float(_)) => ExprType::Float,
        Expression::Atom(Atom::Reference(_)) => ExprType::Reference,
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_expression_get_string(expr: *const Expression) -> StringRef {
    if expr.is_null() {
        return StringRef::null();
    }
    match &*expr {
        Expression::Atom(Atom::Symbol(s)) => s.as_str().to_string_ref(),
        Expression::Atom(Atom::String(s)) => s.as_str().to_string_ref(),
        _ => StringRef::null(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_expression_get_integer(expr: *const Expression) -> i64 {
    if expr.is_null() {
        return 0;
    }
    match *expr {
        Expression::Atom(Atom::Integer(i)) => i,
        _ => 0,
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_expression_get_float(expr: *const Expression) -> f64 {
    if expr.is_null() {
        return 0.0;
    }
    match *expr {
        Expression::Atom(Atom::Float(f)) => f,
        _ => 0.0,
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_expression_get_reference(expr: *const Expression) -> u64 {
    if expr.is_null() {
        return 0;
    }
    match *expr {
        Expression::Atom(Atom::Reference(r)) => r,
        _ => 0,
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_expression_free(expr: *mut Expression) {
    if expr.is_null() {
        return;
    }
    let expr = Box::from_raw(expr);
    drop(expr);
}
