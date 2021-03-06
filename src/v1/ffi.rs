use crate::v1::eval;
use crate::v1::eval::{CompatFlags, Error, MethodDispatcher, VariableProvider};
use crate::v1::expr::{Atom, Cons, Expression};
use crate::v1::lex::LexerError;
use crate::v1::parser::ParseError;
use crate::v1::query::{Query, Source};
use libc::c_char;
use std::cmp::min;
use std::ffi::{c_void, CStr, CString};
use std::ptr::{null, null_mut};
use thiserror::Error;

#[repr(C)]
pub struct StringRef {
    ptr: *const c_char,
    len: usize,
}

impl StringRef {
    fn null() -> Self {
        StringRef { ptr: null(), len: 0 }
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

pub struct Parser {
    last_error: Option<ParserLastError>,
}

#[derive(Error, Debug)]
enum ParserLastError {
    #[error(transparent)]
    ParseError(#[from] ParseError),
    #[error("invalid query string")]
    InvalidQuery,
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

impl From<&LexerError> for ParseStatus {
    fn from(e: &LexerError) -> Self {
        match e {
            LexerError::StringIsNotClosed => ParseStatus::LexerStringIsNotClosed,
        }
    }
}

impl From<&ParseError> for ParseStatus {
    fn from(e: &ParseError) -> Self {
        match e {
            ParseError::LexerError(e) => e.into(),
            ParseError::UnexpectedToken(_) => ParseStatus::UnexpectedToken,
            ParseError::UnexpectedEOF => ParseStatus::UnexpectedEOF,
            ParseError::UnparseableNumber(_) => ParseStatus::UnparseableNumber,
            ParseError::UnterminatedList => ParseStatus::UnterminatedList,
        }
    }
}

impl From<&ParserLastError> for ParseStatus {
    fn from(e: &ParserLastError) -> Self {
        match e {
            ParserLastError::ParseError(e) => e.into(),
            ParserLastError::InvalidQuery => ParseStatus::InvalidQuery,
        }
    }
}

type Function = extern "C" fn(
    context: *mut Context,
    user_data: *mut c_void,
    symbol: *const c_char,
    cdr: *const Expression,
    error: *mut bool,
) -> *mut Expression;

type VariableProviderCallback = extern "C" fn(symbol: *const c_char) -> *mut Expression;

type MethodDispatcherCallback = extern "C" fn(
    symbol: *const c_char,
    receiver: *const Expression,
    cddr: *const Expression,
    error: *mut bool,
) -> *mut Expression;

pub struct Context {
    context: eval::Context,
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

#[no_mangle]
pub unsafe extern "C" fn yq_v1_parser_new() -> *mut Parser {
    Box::into_raw(Box::new(Parser { last_error: None }))
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_parser_parse(parser: *mut Parser, query: *const c_char) -> *mut Query {
    if parser.is_null() {
        return null_mut();
    }
    (*parser).last_error = None;
    let query = match CStr::from_ptr(query).to_str() {
        Ok(s) => s,
        Err(_) => {
            (*parser).last_error = Some(ParserLastError::InvalidQuery);
            return null_mut();
        }
    };
    match crate::v1::parser::parse(query) {
        Ok(q) => Box::into_raw(Box::new(q)),
        Err(e) => {
            (*parser).last_error = Some(ParserLastError::ParseError(e));
            null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_parser_get_last_error(parser: *const Parser) -> ParseStatus {
    if parser.is_null() {
        return ParseStatus::Success;
    }
    match (*parser).last_error.as_ref() {
        Some(e) => e.into(),
        None => ParseStatus::Success,
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_parser_get_last_error_message(
    parser: *const Parser,
    dest: *mut c_char,
    len: usize,
) -> usize {
    if parser.is_null() {
        return 0;
    }

    let message = (*parser).last_error.as_ref().map(|e| e.to_string()).unwrap_or_default();
    let c_message = CString::new(message).unwrap();
    let actual_len = c_message.as_bytes().len();

    // TODO: can i copy from `message` directly?
    dest.copy_from_nonoverlapping(c_message.as_ptr(), min(len, actual_len + 1));
    actual_len
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_query_get_source(query: *const Query, index: usize) -> *const Source {
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
    error: *mut bool,
) -> *mut Expression {
    if context.is_null() {
        return null_mut();
    }
    *error = false;
    match (*context).context.evaluate(&*expr) {
        Ok(expr) => Box::into_raw(Box::new(expr)),
        Err(e) => {
            *error = true;
            Box::into_raw(Box::new(e))
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_context_register_function(
    context: *mut Context,
    symbol: *const c_char,
    function: Function,
    user_data: *mut c_void,
) {
    if context.is_null() {
        return;
    }
    (*context)
        .context
        .register_function(CStr::from_ptr(symbol).to_str().unwrap(), move |_, symbol, cdr| {
            let symbol = CString::new(symbol).unwrap();
            let mut error = false;
            let result = function(context, user_data, symbol.as_ptr(), cdr, &mut error);

            let expr = if result.is_null() {
                Expression::Atom(Atom::Nil)
            } else {
                *unsafe { Box::from_raw(result) }
            };

            if error {
                Err(expr)
            } else {
                Ok(expr)
            }
        });
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
        fn dispatch(&self, symbol: &str, receiver: Atom, cddr: &Expression) -> Result<Expression, Error> {
            let symbol = CString::new(symbol).unwrap();
            let receiver = Expression::Atom(receiver);
            let mut error = false;
            let result = self.0(symbol.as_ptr(), &receiver, cddr, &mut error);

            let expr = if result.is_null() {
                Expression::Atom(Atom::Nil)
            } else {
                *unsafe { Box::from_raw(result) }
            };

            if error {
                Err(expr)
            } else {
                Ok(expr)
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
pub unsafe extern "C" fn yq_v1_context_set_compat_flags(context: *mut Context, flags: CompatFlags) {
    if context.is_null() {
        return;
    }
    (*context).context.set_compat_flags(flags);
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
pub unsafe extern "C" fn yq_v1_expression_new_cons(car: *mut Expression, cdr: *mut Expression) -> *mut Expression {
    Box::into_raw(Box::new(Expression::Cons(Cons::new(
        if car.is_null() {
            Box::new(Expression::nil())
        } else {
            Box::from_raw(car)
        },
        if cdr.is_null() {
            Box::new(Expression::nil())
        } else {
            Box::from_raw(cdr)
        },
    ))))
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
pub unsafe extern "C" fn yq_v1_expression_get_car(expr: *const Expression) -> *const Expression {
    if expr.is_null() {
        return null();
    }
    match &*expr {
        Expression::Cons(cons) => cons.car(),
        _ => null(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_expression_get_cdr(expr: *const Expression) -> *const Expression {
    if expr.is_null() {
        return null();
    }
    match &*expr {
        Expression::Cons(cons) => cons.cdr(),
        _ => null(),
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
