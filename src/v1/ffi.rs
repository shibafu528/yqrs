use crate::v1::eval;
use crate::v1::eval::Error;
use crate::v1::expr::Expression;
use crate::v1::lex::LexerError;
use crate::v1::parser::ParseError;
use crate::v1::query::{Query, Source};
use libc::c_char;
use std::ffi::CStr;
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

pub struct Context {
    context: eval::Context,
    last_error: Option<eval::Error>,
}

#[repr(C)]
pub enum EvalError {
    Success = 0,
    VoidFunction,
    InvalidFunction,
    VoidVariable,
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
pub unsafe extern "C" fn yq_v1_context_get_last_error(context: *mut Context) -> EvalError {
    if context.is_null() {
        return EvalError::Success;
    }
    match &(*context).last_error {
        Some(e) => match e {
            Error::VoidFunction => EvalError::VoidFunction,
            Error::InvalidFunction => EvalError::InvalidFunction,
            Error::VoidVariable(_) => EvalError::VoidVariable,
        },
        None => EvalError::Success,
    }
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
pub unsafe extern "C" fn yq_v1_expression_free(expr: *mut Expression) {
    if expr.is_null() {
        return;
    }
    let expr = Box::from_raw(expr);
    drop(expr);
}
