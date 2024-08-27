use std::str::Chars;

use token::{HasInfix, Location, Precedence, Token, TokenKind};
use tracing::{event, Level};

pub mod token;

use crate::parser::{
    ast::Identifier,
    parse_errors::{ParseError, ParseErrorKind},
};

#[derive(Debug)]
pub struct Lexer<'a> {
    chars: Chars<'a>,
    current_pos: usize,
    current_token: Option<Token>,
    current_line_number: u16,
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str) -> Self {
        let mut lexer = Lexer {
            chars: source_code.chars(),
            current_pos: 0,
            current_token: None,
            current_line_number: 1,
        };
        event!(Level::DEBUG, "Initializing lexer");
        lexer.advance();
        lexer.advance_through_whitespace();
        event!(Level::DEBUG, "Advanced lexer to starting position");

        lexer
    }

    pub fn consume(&mut self) -> Option<Token> {
        event!(
            Level::TRACE,
            "Consuming at state: current token: {:?}",
            self.current_token
        );

        let token = self.current_token.take();
        self.advance();

        event!(Level::TRACE, "returning token: {:?}", token);

        self.advance_through_whitespace();

        token
    }

    fn advance(&mut self) {
        self.current_token = if let Some(c) = self.chars.next() {
            self.current_pos += c.len_utf8();
            let token_kind = self.tokenize(c);

            let token = Token {
                token_kind,
                location: Location {
                    line_number: self.current_line_number,
                },
            };
            Some(token)
        } else {
            None
        };
    }

    /// At a later point we will return whitespace tokens to the client in order to
    /// generate token lists for syntax nodes, but as of now we just skip it
    fn advance_through_whitespace(&mut self) {
        while self
            .current_token
            .as_ref()
            .map(Token::is_whitespace)
            .unwrap_or(false)
        {
            event!(
                Level::TRACE,
                "found whitespace token, we advance: {:?}",
                self.current_token
            );
            self.advance();
        }
    }

    pub fn expect(&mut self) -> Result<Token, ParseError> {
        self.consume().ok_or_else(|| {
            ParseError::new(
                Token {
                    token_kind: TokenKind::EOF,
                    // TODO: need to figure out last line of file
                    location: Location {
                        line_number: self.current_line_number,
                    },
                },
                ParseErrorKind::ExpectedToken,
            )
        })
    }

    pub fn peek(&self) -> Option<&Token> {
        self.current_token.as_ref()
    }

    pub fn has_next(&self) -> bool {
        self.current_token.as_ref().is_some()
    }

    pub fn expect_peek(&self) -> Result<&Token, ParseError> {
        self.current_token.as_ref().ok_or(ParseError::new(
            Token {
                token_kind: TokenKind::EOF,
                location: Location {
                    line_number: self.current_line_number,
                },
            },
            ParseErrorKind::ExpectedToken,
        ))
    }

    fn tokenize(&mut self, first_char: char) -> TokenKind {
        match first_char {
            '!' => self.lex_bang(),
            '=' => self.lex_equal(),
            '+' => TokenKind::Add,
            '-' => TokenKind::Minus,
            ':' => TokenKind::Colon,
            '}' => TokenKind::RBrace,
            '{' => TokenKind::LBrace,
            ')' => TokenKind::RParen,
            '(' => TokenKind::LParen,
            ']' => TokenKind::RBracket,
            '[' => TokenKind::LBracket,
            '<' => TokenKind::LessThan,
            '>' => TokenKind::GreaterThan,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Period,
            '~' => TokenKind::Lasagna,
            '/' => TokenKind::Slash,
            '*' => TokenKind::Asterix,
            '"' => self.lex_string(),
            '\n' => {
                self.current_line_number += 1;
                TokenKind::NewLine
            }
            c if c.is_whitespace() => self.lex_whitespace(c),
            c if c.is_ascii_digit() => self.lex_number(c),
            c if c.is_alphabetic() => self.lex_identifier_or_keyword(c),
            _ => TokenKind::Illegal,
        }
    }

    fn lex_bang(&mut self) -> TokenKind {
        if self.chars.clone().next() == Some('=') {
            self.chars.next();
            self.current_pos += 1;
            TokenKind::NotEqual
        } else {
            TokenKind::Bang
        }
    }

    fn lex_equal(&mut self) -> TokenKind {
        if self.chars.clone().next() == Some('=') {
            self.chars.next();
            self.current_pos += 1;
            TokenKind::Equal
        } else {
            TokenKind::Illegal
        }
    }

    fn lex_string(&mut self) -> TokenKind {
        let mut value = String::new();
        for c in self.chars.by_ref() {
            self.current_pos += c.len_utf8();
            if c == '"' {
                break;
            }
            value.push(c);
        }
        TokenKind::Str(value)
    }

    fn lex_whitespace(&mut self, current_char: char) -> TokenKind {
        let mut value = String::new();
        value.push(current_char);

        while let Some(c) = self.chars.clone().next() {
            if !c.is_whitespace() || c == '\n' {
                break;
            }
            value.push(c);
            self.chars.next();
            self.current_pos += c.len_utf8();
        }
        TokenKind::Space
    }

    fn lex_number(&mut self, current_char: char) -> TokenKind {
        let mut value = String::new();
        value.push(current_char);

        while let Some(c) = self.chars.clone().next() {
            if !c.is_ascii_digit() {
                break;
            }
            value.push(c);
            self.chars.next();
            self.current_pos += c.len_utf8();
        }
        TokenKind::Int(value)
    }

    fn lex_identifier_or_keyword(&mut self, current_char: char) -> TokenKind {
        let mut value = String::new();
        value.push(current_char);

        while let Some(c) = self.chars.clone().next() {
            if Self::not_valid_ident_char(c) {
                break;
            }
            value.push(c);
            self.chars.next();
            self.current_pos += c.len_utf8();
        }
        TokenKind::parse_keyword(&value)
    }

    fn not_valid_ident_char(c: char) -> bool {
        let is_allowed = c.is_alphabetic() || c == '_';

        !is_allowed
    }

    pub fn next_token_has_infix(&mut self) -> bool {
        let next_token = self.peek();
        let has_infix = match next_token {
            Some(token) => !matches!(token.token_kind.has_infix(), HasInfix::No(_)),
            None => false,
        };

        event!(
            Level::DEBUG,
            "Checking infix of token {next_token:?}: {has_infix}"
        );

        has_infix
    }

    pub fn next_token_is(&mut self, is_token: &TokenKind) -> bool {
        match self.peek() {
            Some(token) => is_token == &token.token_kind,
            None => false,
        }
    }

    /// This is meant to keep parsing the program, even if we get an error,
    /// so that the user can get multiple error reports for a single parse.
    /// However, we need to figure out a good strategy to parse, since we dont use
    /// statement seperators, and newlines dont matter
    pub fn iterate_to_next_statement(&mut self) {
        event!(Level::TRACE, "Iterating to next statement");
        while let Some(token) = self.consume() {
            if let TokenKind::NewLine = token.token_kind {
                event!(Level::TRACE, "Found beginning, stop advancing");
                break;
            }
        }
    }

    pub fn expect_token(&mut self, expected_token_type: TokenKind) -> Result<Token, ParseError> {
        match self.peek() {
            Some(token) if token.token_kind == expected_token_type => Ok(self.consume().unwrap()),
            Some(token) => Err(ParseError::new(
                token.clone(),
                ParseErrorKind::UnexpectedToken(expected_token_type),
            )),
            None => Err(ParseError::new(
                Token {
                    token_kind: TokenKind::EOF,
                    location: Location {
                        line_number: self.current_line_number,
                    },
                },
                ParseErrorKind::ExpectedToken,
            )),
        }
    }

    pub fn expect_optional_token(&mut self, expected_token_type: TokenKind) -> Option<Token> {
        if self.next_token_is(&expected_token_type) {
            return self.consume();
        }

        None
    }

    pub fn expected_identifier(&mut self) -> Result<(Identifier, Token), ParseError> {
        match self.peek() {
            Some(token) => {
                let parsed_identifier = Identifier::parse_from_token(token)?;
                let token = self.expect()?;
                Ok((parsed_identifier, token))
            }
            None => Err(ParseError::new(
                Token {
                    token_kind: TokenKind::EOF,
                    location: Location {
                        line_number: self.current_line_number,
                    },
                },
                ParseErrorKind::ExpectedToken,
            )),
        }
    }

    pub fn next_token_precedence(&mut self) -> Precedence {
        match self.peek() {
            Some(token) => token.token_kind.get_precedence(),
            None => Precedence::Lowest,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test_util;

    use super::{token::TokenKind, Lexer};

    #[test]
    fn parse_sympols() {
        test_util::setup_logger();
        let source_code = "
            !+:}{)(][~
        ";

        let expected_tokens = vec![
            TokenKind::Bang,
            TokenKind::Add,
            TokenKind::Colon,
            TokenKind::RBrace,
            TokenKind::LBrace,
            TokenKind::RParen,
            TokenKind::LParen,
            TokenKind::RBracket,
            TokenKind::LBracket,
            TokenKind::Lasagna,
        ];

        let mut found_tokens: Lexer = Lexer::new(source_code);
        let mut expected_iter = expected_tokens.into_iter();
        while let Some(token) = found_tokens.consume() {
            let expected_token = expected_iter.next().unwrap();
            assert_eq!(token.token_kind, expected_token);
        }
    }

    #[test]
    fn parse_identifier() {
        test_util::setup_logger();
        let source_code = "
            foo
        ";

        let expected_tokens = [TokenKind::Ident("foo".to_string())];

        let found_tokens = test_util::tokenize(source_code);

        assert_eq!(
            found_tokens.len(),
            expected_tokens.len(),
            "List of expected tokens should be the same as found tokens"
        );
        expected_tokens.iter().enumerate().for_each(|(idx, token)| {
            assert_eq!(
                token,
                &found_tokens.get(idx).expect("Should have token").token_kind,
                "Token in position {idx} was not parsed"
            )
        });
    }

    #[test]
    fn parse_comma_seperated_identifier() {
        let source_code = "
(foo, bar)
        ";

        let expected_tokens = [
            TokenKind::LParen,
            TokenKind::Ident("foo".to_string()),
            TokenKind::Comma,
            TokenKind::Ident("bar".to_string()),
            TokenKind::RParen,
        ];

        let mut lexer = Lexer::new(source_code);

        expected_tokens.iter().for_each(|token| {
            let found = &lexer.consume().unwrap();
            assert_eq!(
                token, &found.token_kind,
                "Expected {token:?}, but got {found:?}"
            )
        });
    }

    #[test]
    fn parse_code() {
        let source_code = "
            let foo: 5~
            foo + 6
            ~fooFunc(x, y):
                ~res x+y
                return res
            ~

            ~if(5 < 10):
                5 + 6
                6 + 7
                return true
            ~else:
!
                return false
            ~
            ==
            !=
            \"foo\" + \"bar hei\"
            \"\"
        ";

        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Ident("foo".to_string()),
            TokenKind::Colon,
            TokenKind::Int("5".to_string()),
            TokenKind::Lasagna,
            TokenKind::Ident("foo".to_string()),
            TokenKind::Add,
            TokenKind::Int("6".to_string()),
            TokenKind::Lasagna,
            TokenKind::Ident("fooFunc".to_string()),
            TokenKind::LParen,
            TokenKind::Ident("x".to_string()),
            TokenKind::Comma,
            TokenKind::Ident("y".to_string()),
            TokenKind::RParen,
            TokenKind::Colon,
            TokenKind::Lasagna,
            TokenKind::Ident("res".to_string()),
            TokenKind::Ident("x".to_string()),
            TokenKind::Add,
            TokenKind::Ident("y".to_string()),
            TokenKind::Return,
            TokenKind::Ident("res".to_string()),
            TokenKind::Lasagna,
            TokenKind::Lasagna,
            TokenKind::If,
            TokenKind::LParen,
            TokenKind::Int("5".to_string()),
            TokenKind::LessThan,
            TokenKind::Int("10".to_string()),
            TokenKind::RParen,
            TokenKind::Colon,
            TokenKind::Int("5".to_string()),
            TokenKind::Add,
            TokenKind::Int("6".to_string()),
            TokenKind::Int("6".to_string()),
            TokenKind::Add,
            TokenKind::Int("7".to_string()),
            TokenKind::Return,
            TokenKind::True,
            TokenKind::Lasagna,
            TokenKind::Else,
            TokenKind::Colon,
            TokenKind::Bang,
            TokenKind::Return,
            TokenKind::False,
            TokenKind::Lasagna,
            TokenKind::Equal,
            TokenKind::NotEqual,
            TokenKind::Str("foo".to_string()),
            TokenKind::Add,
            TokenKind::Str("bar hei".to_string()),
            TokenKind::Str("".to_string()),
        ];

        let mut found_tokens: Lexer = Lexer::new(source_code);
        let mut expected_iter = expected_tokens.into_iter();
        while let Some(token) = found_tokens.consume() {
            let expected_token = expected_iter.next().unwrap();
            assert_eq!(token.token_kind, expected_token);
        }
    }
}
