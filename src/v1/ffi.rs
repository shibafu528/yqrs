use crate::v1::lex::LexerError;
use crate::v1::parser::ParseError;
use crate::v1::query::{Query, Source};
use libc::c_char;
use std::ffi::CStr;
use std::ptr::null;

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
pub unsafe extern "C" fn yq_v1_query_free(query: *mut Query) {
    if query.is_null() {
        return;
    }
    let query = Box::from_raw(query);
    drop(query);
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_source_get_class(
    source: *const Source,
    out: *mut *const c_char,
) -> usize {
    if source.is_null() {
        return 0;
    }
    let class = (*source).class();
    *out = class.as_bytes().as_ptr() as *const c_char;
    class.len()
}

#[no_mangle]
pub unsafe extern "C" fn yq_v1_source_get_argument(
    source: *const Source,
    out: *mut *const c_char,
) -> usize {
    if source.is_null() {
        return 0;
    }
    match (*source).argument() {
        Some(a) => {
            *out = a.as_bytes().as_ptr() as *const c_char;
            a.len()
        }
        None => 0,
    }
}
