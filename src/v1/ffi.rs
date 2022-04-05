use crate::v1::lex::LexerError;
use crate::v1::parser::ParseError;
use crate::v1::query::Query;
use libc::c_char;
use std::ffi::CStr;

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
pub unsafe extern "C" fn yq_v1_parser_parse(
    query: *const c_char,
    out: *mut *mut Query,
) -> ParseStatus {
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
pub unsafe extern "C" fn yq_v1_query_free(query: *mut Query) {
    if query.is_null() {
        return;
    }
    let query = Box::from_raw(query);
    drop(query);
}
