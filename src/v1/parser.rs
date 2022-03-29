use std::iter::Peekable;
use crate::v1::lex::{Lexer, LexerError, Token};
use crate::v1::query::{Query, Source};
use thiserror::Error;

const DEFAULT_QUERY: &str = "from all";

pub fn parse(query: &str) -> Result<Query, ParseError> {
    let query = if query.trim().is_empty() {
        DEFAULT_QUERY
    } else {
        query
    };

    let mut lex = Lexer::new(query.trim()).peekable();

    let mut sources = vec![];
    match lex.next() {
        Some(Ok(Token::Literal(s, _))) if s == "from" => {
            match parse_from_clause(&mut lex) {
                Ok(srcs) => sources = srcs,
                Err(e) => return Err(e)
            }
        }
        Some(Ok(Token::Literal(s, _))) if s == "where" => {}
        Some(Ok(t)) => return Err(ParseError::UnexpectedToken(t)),
        Some(Err(e)) => return Err(e.into()),
        None => panic!(),
    }

    if sources.is_empty() {
        sources.push(Source::new("all".to_string()))
    }

    Ok(Query::new(sources))
}

fn parse_from_clause(lex: &mut Peekable<Lexer>) -> Result<Vec<Source>, ParseError> {
    /*
    <from-clause> ::= <source> ("," <source>)*
    <source>      ::= <class> | <class> ":" <argument>
    <class>       ::= LITERAL
    <argument>    ::= STRING
     */
    let mut src = vec![];
    loop {
        match lex.next() {
            // break
            Some(Ok(Token::Literal(s, _))) if s == "where" => return Ok(src),
            None => return Ok(src),

            // source
            Some(Ok(Token::Literal(class, _))) => {
                // take argument?
                match lex.peek() {
                    // maybe <class> ":" <argument>
                    Some(Ok(Token::Colon(_))) => {
                        let _ = lex.next();
                        match lex.next() {
                            // <class> ":" <argument>
                            Some(Ok(Token::String(argument, _))) => {
                                src.push(Source::new_with_argument(class, argument))
                            }

                            // error
                            Some(Ok(t)) => return Err(ParseError::UnexpectedToken(t)),
                            Some(Err(e)) => return Err(e.into()),
                            None => return Err(ParseError::UnexpectedEOF),
                        }
                    }

                    // <class>
                    _ => {
                        src.push(Source::new(class))
                    }
                }
            }

            // source delimiter
            Some(Ok(t @ Token::Comma(_))) => {
                if src.is_empty() {
                    return Err(ParseError::UnexpectedToken(t));
                }
                match lex.peek() {
                    Some(Ok(Token::Literal(_, _))) => {} // ok
                    _ => return Err(ParseError::UnexpectedToken(t))
                }
            }

            // error
            Some(Ok(t)) => return Err(ParseError::UnexpectedToken(t)),
            Some(Err(e)) => return Err(e.into()),
        }
    }
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error(transparent)]
    LexerError(#[from] LexerError),
    #[error("unexpected token {0}")]
    UnexpectedToken(Token),
    #[error("unexpected eof")]
    UnexpectedEOF,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_query_is_from_all_where_true() {
        let result = parse("");
        assert!(result.is_ok());

        let query = result.unwrap();
        assert_eq!(1, query.sources().len());
        assert_eq!("all", query.sources()[0].class());
        assert!(query.sources()[0].argument().is_none());

        // TODO: assert "where t"
    }

    #[test]
    fn from_empty() {
        let result = parse("from");
        assert!(result.is_ok());

        let query = result.unwrap();
        assert_eq!(1, query.sources().len());
        assert_eq!("all", query.sources()[0].class());
        assert!(query.sources()[0].argument().is_none());
    }

    #[test]
    fn from_all() {
        let result = parse("from all");
        assert!(result.is_ok());

        let query = result.unwrap();
        assert_eq!(1, query.sources().len());
        assert_eq!("all", query.sources()[0].class());
        assert!(query.sources()[0].argument().is_none());
    }

    #[test]
    fn from_all_all() {
        let result = parse("from all,all");
        assert!(result.is_ok());

        let query = result.unwrap();
        assert_eq!(2, query.sources().len());
        assert_eq!("all", query.sources()[0].class());
        assert!(query.sources()[0].argument().is_none());
        assert_eq!("all", query.sources()[1].class());
        assert!(query.sources()[1].argument().is_none());
    }

    #[test]
    fn from_home() {
        let result = parse(r#"from home:"yukari4a""#);
        assert!(result.is_ok());

        let query = result.unwrap();
        assert_eq!(1, query.sources().len());
        assert_eq!("home", query.sources()[0].class());
        assert!(query.sources()[0].argument().is_some());
        assert_eq!("yukari4a", query.sources()[0].argument().unwrap());
    }

    #[test]
    fn from_all_home() {
        let result = parse(r#"from all,home:"yukari4a""#);
        assert!(result.is_ok());

        let query = result.unwrap();
        assert_eq!(2, query.sources().len());
        assert_eq!("all", query.sources()[0].class());
        assert!(query.sources()[0].argument().is_none());
        assert_eq!("home", query.sources()[1].class());
        assert!(query.sources()[1].argument().is_some());
        assert_eq!("yukari4a", query.sources()[1].argument().unwrap());
    }

    #[test]
    fn from_home_all() {
        let result = parse(r#"from home:"yukari4a",all"#);
        assert!(result.is_ok());

        let query = result.unwrap();
        assert_eq!(2, query.sources().len());
        assert_eq!("home", query.sources()[0].class());
        assert!(query.sources()[0].argument().is_some());
        assert_eq!("yukari4a", query.sources()[0].argument().unwrap());
        assert_eq!("all", query.sources()[1].class());
        assert!(query.sources()[1].argument().is_none());
    }
}