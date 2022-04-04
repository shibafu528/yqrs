use std::fmt::{Display, Formatter};
use std::str::CharIndices;
use thiserror::Error;

pub struct Lexer<'a> {
    chars: CharIndices<'a>,
    rest: Option<(usize, char)>,
}

impl<'a> Lexer<'a> {
    pub fn new(query: &'a str) -> Self {
        let query = query.trim_end();
        Lexer {
            chars: query.char_indices(),
            rest: None,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut begin: Option<usize> = None;
        let mut buf: Vec<char> = vec![];
        loop {
            let token = match self.rest {
                Some(v) => {
                    self.rest = None;
                    Some(v)
                }
                None => self.chars.next(),
            };
            match token {
                Some((pos, char)) => {
                    if let Some(p) = begin {
                        // find end of literal
                        match char {
                            ' ' | '\t' | '\r' | '\n' | '"' | '\'' | ',' | ':' | '(' | ')' => {
                                self.rest = Some((pos, char));
                                return Some(Ok(Token::Literal(
                                    buf.into_iter().collect(),
                                    TokenMeta { pos: p },
                                )));
                            }
                            _ => buf.push(char),
                        }
                    } else {
                        match char {
                            ' ' | '\t' | '\r' | '\n' => {}
                            c @ '"' | c @ '\'' => {
                                return match read_until(&mut self.chars, c) {
                                    Ok(s) => Some(Ok(Token::String(s, TokenMeta { pos }))),
                                    Err(e) => Some(Err(e)),
                                }
                            }
                            ',' => return Some(Ok(Token::Comma(TokenMeta { pos }))),
                            ':' => return Some(Ok(Token::Colon(TokenMeta { pos }))),
                            '(' => return Some(Ok(Token::LeftParenthesis(TokenMeta { pos }))),
                            ')' => return Some(Ok(Token::RightParenthesis(TokenMeta { pos }))),
                            _ => {
                                begin = Some(pos);
                                buf.push(char);
                            }
                        }
                    }
                }
                None => {
                    return match begin {
                        Some(pos) => Some(Ok(Token::Literal(
                            buf.into_iter().collect(),
                            TokenMeta { pos },
                        ))),
                        None => None,
                    }
                }
            }
        }
    }
}

fn read_until(chars: &mut CharIndices, stop: char) -> Result<String, LexerError> {
    let mut skip_next = false;
    let mut str: Vec<char> = vec![];
    for (_, c) in chars {
        if skip_next {
            skip_next = false;
            continue;
        }
        match c {
            '\\' => skip_next = true,
            c if c == stop => return Ok(str.into_iter().collect()),
            _ => str.push(c),
        }
    }

    Err(LexerError::StringIsNotClosed)
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    /// Unquoted string
    Literal(String, TokenMeta),
    /// Quoted string literal, ex. "hello world"
    String(String, TokenMeta),
    /// ","
    Comma(TokenMeta),
    /// ":"
    Colon(TokenMeta),
    /// "("
    LeftParenthesis(TokenMeta),
    /// ")"
    RightParenthesis(TokenMeta),
}

impl Token {
    #[allow(dead_code)]
    pub fn meta(&self) -> &TokenMeta {
        match self {
            Token::Literal(_, m) => m,
            Token::String(_, m) => m,
            Token::Comma(m) => m,
            Token::Colon(m) => m,
            Token::LeftParenthesis(m) => m,
            Token::RightParenthesis(m) => m,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Literal(s, m) => write!(f, "LITERAL({}) at position {}", s, m.pos.to_string()),
            Token::String(s, m) => write!(f, "STRING({}) at position {}", s, m.pos.to_string()),
            Token::Comma(m) => write!(f, "COMMA at position {}", m.pos.to_string()),
            Token::Colon(m) => write!(f, "COLON at position {}", m.pos.to_string()),
            Token::LeftParenthesis(m) => {
                write!(f, "LEFT_PARENTHESIS at position {}", m.pos.to_string())
            }
            Token::RightParenthesis(m) => {
                write!(f, "RIGHT_PARENTHESIS at position {}", m.pos.to_string())
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct TokenMeta {
    pos: usize,
}

impl TokenMeta {
    #[allow(dead_code)]
    pub fn pos(&self) -> usize {
        self.pos
    }
}

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("string literal is not closed")]
    StringIsNotClosed,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_returns_none() {
        let mut lex = Lexer::new("");
        assert!(lex.next().is_none());
    }

    #[test]
    fn literal() {
        let lex = Lexer::new("foo bar");

        let expects = vec![
            Token::Literal("foo".to_string(), TokenMeta { pos: 0 }),
            Token::Literal("bar".to_string(), TokenMeta { pos: 4 }),
        ];

        for (expect, actual) in expects.into_iter().zip(lex) {
            match actual {
                Ok(t) => assert_eq!(expect, t),
                Err(_) => panic!(),
            }
        }
    }

    #[test]
    fn quotation() {
        let lex = Lexer::new("\"foo\" \"bar\" \"hoge fuga\"");

        let expects = vec![
            Token::String("foo".to_string(), TokenMeta { pos: 0 }),
            Token::String("bar".to_string(), TokenMeta { pos: 6 }),
            Token::String("hoge fuga".to_string(), TokenMeta { pos: 12 }),
        ];

        for (expect, actual) in expects.into_iter().zip(lex) {
            match actual {
                Ok(t) => assert_eq!(expect, t),
                Err(_) => panic!(),
            }
        }
    }

    #[test]
    fn single_quotation() {
        let lex = Lexer::new("'foo' \"bar\" 'hoge \"fuga\" piyo' \"hoge 'fuga' piyo\"");

        let expects = vec![
            Token::String("foo".to_string(), TokenMeta { pos: 0 }),
            Token::String("bar".to_string(), TokenMeta { pos: 6 }),
            Token::String("hoge \"fuga\" piyo".to_string(), TokenMeta { pos: 12 }),
            Token::String("hoge 'fuga' piyo".to_string(), TokenMeta { pos: 31 }),
        ];

        for (expect, actual) in expects.into_iter().zip(lex) {
            match actual {
                Ok(t) => assert_eq!(expect, t),
                Err(_) => panic!(),
            }
        }
    }

    #[test]
    fn unclosed_quotation() {
        let mut lex = Lexer::new("\"foo\" \"bar\" \"hoge fuga");

        assert!(lex.next().map_or(false, |v| v.is_ok()));
        assert!(lex.next().map_or(false, |v| v.is_ok()));
        assert!(lex.next().map_or(false, |v| {
            if let Err(LexerError::StringIsNotClosed) = v {
                true
            } else {
                false
            }
        }));
    }

    #[test]
    fn comma() {
        let lex = Lexer::new("hoge, fuga");

        let expects = vec![
            Token::Literal("hoge".to_string(), TokenMeta { pos: 0 }),
            Token::Comma(TokenMeta { pos: 4 }),
            Token::Literal("fuga".to_string(), TokenMeta { pos: 6 }),
        ];

        for (expect, actual) in expects.into_iter().zip(lex) {
            match actual {
                Ok(t) => assert_eq!(expect, t),
                Err(_) => panic!(),
            }
        }
    }

    #[test]
    fn colon() {
        let lex = Lexer::new("hoge: \"fuga\"");

        let expects = vec![
            Token::Literal("hoge".to_string(), TokenMeta { pos: 0 }),
            Token::Colon(TokenMeta { pos: 4 }),
            Token::String("fuga".to_string(), TokenMeta { pos: 6 }),
        ];

        for (expect, actual) in expects.into_iter().zip(lex) {
            match actual {
                Ok(t) => assert_eq!(expect, t),
                Err(_) => panic!(),
            }
        }
    }

    #[test]
    fn parenthesis() {
        let lex = Lexer::new("hoge: (fuga)");

        let expects = vec![
            Token::Literal("hoge".to_string(), TokenMeta { pos: 0 }),
            Token::Colon(TokenMeta { pos: 4 }),
            Token::LeftParenthesis(TokenMeta { pos: 6 }),
            Token::Literal("fuga".to_string(), TokenMeta { pos: 7 }),
            Token::RightParenthesis(TokenMeta { pos: 11 }),
        ];

        for (expect, actual) in expects.into_iter().zip(lex) {
            match actual {
                Ok(t) => assert_eq!(expect, t),
                Err(_) => panic!(),
            }
        }
    }
}
