use crate::v1::expr::{Atom, Cons, Expression};
use crate::v1::lex::{Lexer, LexerError, Token};
use crate::v1::query::{Query, Source};
use std::iter::Peekable;
use thiserror::Error;

const DEFAULT_QUERY: &str = "from all";

pub fn parse(query: &str) -> Result<Query, ParseError> {
    let query = if query.trim().is_empty() { DEFAULT_QUERY } else { query };

    let mut lex = Lexer::new(query.trim()).peekable();

    let mut sources = vec![];
    match lex.next() {
        Some(Ok(Token::Literal(s, _))) if s == "from" => match parse_from_clause(&mut lex) {
            Ok(srcs) => sources = srcs,
            Err(e) => return Err(e),
        },
        Some(Ok(Token::Literal(s, _))) if s == "where" => {}
        Some(Ok(t)) => return Err(ParseError::UnexpectedToken(t)),
        Some(Err(e)) => return Err(e.into()),
        None => panic!(),
    }
    if sources.is_empty() {
        sources.push(Source::new("all".to_string()))
    }

    Ok(Query::new(sources, parse_where_clause(&mut lex)?))
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
                    _ => src.push(Source::new(class)),
                }
            }

            // source delimiter
            Some(Ok(t @ Token::Comma(_))) => {
                if src.is_empty() {
                    return Err(ParseError::UnexpectedToken(t));
                }
                match lex.peek() {
                    Some(Ok(Token::Literal(_, _))) => {} // ok
                    _ => return Err(ParseError::UnexpectedToken(t)),
                }
            }

            // error
            Some(Ok(t)) => return Err(ParseError::UnexpectedToken(t)),
            Some(Err(e)) => return Err(e.into()),
        }
    }
}

fn parse_where_clause(lex: &mut Peekable<Lexer>) -> Result<Expression, ParseError> {
    if let None = lex.peek() {
        return Ok(Atom::Symbol("t".to_string()).into());
    }

    let result = parse_expression(lex)?;
    match lex.next() {
        None => Ok(result),
        Some(Ok(t)) => Err(ParseError::UnexpectedToken(t)),
        Some(Err(e)) => Err(e.into()),
    }
}

fn parse_expression(lex: &mut Peekable<Lexer>) -> Result<Expression, ParseError> {
    match lex.next() {
        Some(Ok(Token::LeftParenthesis(_))) => Ok(Expression::Cons(parse_list(lex)?)),
        Some(Ok(Token::Literal(s, _))) => Ok(Expression::Atom(parse_literal(s)?)),
        Some(Ok(Token::String(s, _))) => Ok(Expression::Atom(Atom::String(s))),
        Some(Ok(t)) => Err(ParseError::UnexpectedToken(t)),
        Some(Err(e)) => Err(e.into()),
        None => Err(ParseError::UnexpectedEOF),
    }
}

fn parse_list(lex: &mut Peekable<Lexer>) -> Result<Cons, ParseError> {
    // (? ?...
    //   `-- check
    match lex.peek() {
        // => ()
        Some(Ok(Token::RightParenthesis(_))) => return Ok(Cons::empty()),

        // error
        Some(Err(_)) => return Err(lex.next().unwrap().unwrap_err().into()),
        None => return Err(ParseError::UnterminatedList),

        // continue
        _ => {}
    }

    // (car ?...
    //   `-- parse
    let car = parse_expression(lex)?;

    // (car ?...
    //       `-- check
    let is_dotted_pair = match lex.peek() {
        // => (car . ?...
        Some(Ok(Token::Literal(s, _))) if s == "." => true,

        // error
        Some(Err(_)) => return Err(lex.next().unwrap().unwrap_err().into()),
        None => return Err(ParseError::UnterminatedList),

        // other
        _ => false,
    };

    let cdr = if is_dotted_pair {
        let _ = lex.next();

        // (car . ?...
        //         `-- parse
        let cdr = parse_expression(lex)?;

        // (car . cdr ?...
        //             `-- check
        match lex.next() {
            // => (car . cdr)
            Some(Ok(Token::RightParenthesis(_))) => {}

            // error
            Some(Err(e)) => return Err(e.into()),
            _ => return Err(ParseError::UnterminatedList),
        }

        cdr
    } else {
        // (car ?...
        //       `-- parse
        let cdr = parse_list(lex)?;
        if cdr.is_nil() {
            let _ = lex.next();
            Atom::Nil.into()
        } else {
            cdr.into()
        }
    };

    Ok(Cons::new(car.into(), cdr.into()))
}

fn parse_literal(literal: String) -> Result<Atom, ParseError> {
    if literal == "nil" {
        return Ok(Atom::Nil);
    }

    let mut has_point = false;
    for chr in literal.chars() {
        match chr {
            '.' => {
                if has_point {
                    // double point is invalid
                    return Ok(Atom::Symbol(literal));
                }
                has_point = true
            }
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' | '+' => (),
            _ => return Ok(Atom::Symbol(literal)),
        }
    }

    if has_point {
        literal
            .parse::<f64>()
            .map(|f| Atom::Float(f))
            .map_err(|_| ParseError::UnparseableNumber(literal))
    } else {
        literal
            .parse::<i64>()
            .map(|i| Atom::Integer(i))
            .map_err(|_| ParseError::UnparseableNumber(literal))
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
    #[error("unparseable number: {0}")]
    UnparseableNumber(String),
    #[error("unterminated list")]
    UnterminatedList,
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

        let bool = match query.expression() {
            Expression::Atom(s @ Atom::Symbol(_)) if !s.is_nil() => true,
            _ => false,
        };
        assert!(bool)
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

    #[test]
    fn expr_true() {
        let result = parse(r#"where (t)"#);
        assert!(result.is_ok());

        let query = result.unwrap();
        match query.expression() {
            Expression::Cons(c) => {
                match c.car() {
                    Expression::Atom(Atom::Symbol(s)) => assert_eq!("t", s),
                    _ => assert!(false),
                }
                match c.cdr() {
                    Expression::Atom(Atom::Nil) => assert!(true),
                    _ => assert!(false),
                }
            }
            _ => assert!(false),
        }
    }
}
