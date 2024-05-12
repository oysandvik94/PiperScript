use std::fmt::Display;

use lexer::token::Token;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected_token: Token,
        found_token: Option<Token>,
    },
    ExpectedToken,
    UnknownToken(Token),
    ExpressionError(String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected_token,
                found_token,
            } => write!(f, "Expected token of type {expected_token:?}, but received token of type {found_token:?}"),
            ParseError::ExpectedToken => write!(f, "Expected to receive a token, but no token was received"),
            ParseError::UnknownToken(token) => write!(f, "Received unknown token of type {token:?}, don't know how to handle it"),
            ParseError::ExpressionError(error) => write!(f, "{error}")
        }
    }
}
