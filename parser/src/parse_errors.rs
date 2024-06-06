use std::{fmt::Display, num::ParseIntError};

use lexer::token::Token;

use crate::lexer;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected_token: TokenExpectation,
        found_token: Option<Token>,
    },
    ExpectedToken,
    UnknownToken(Token),
    ExpressionError(String),
    NoPrefixExpression(Token),
    NoInfixExpression(Token),
    ParseIntegerError(Token, ParseIntError),
    NoPrefixPartner,
}

impl ParseError {
    pub fn single_unexpected(expected_token: &Token, found_token: Option<&Token>) -> ParseError {
        ParseError::UnexpectedToken {
            expected_token: TokenExpectation::SingleExpectation(expected_token.clone()),
            found_token: found_token.cloned(),
        }
    }

    pub fn multiple_unexpected(
        expected_tokens: Vec<Token>,
        found_token: Option<&Token>,
    ) -> ParseError {
        let expected_tokens = TokenExpectation::MultipleExpectation(expected_tokens);
        ParseError::UnexpectedToken {
            expected_token: expected_tokens,
            found_token: found_token.cloned(),
        }
    }
}

#[derive(Debug)]
pub enum TokenExpectation {
    SingleExpectation(Token),
    MultipleExpectation(Vec<Token>),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected_token,
                found_token,
            } => write!(
                f,
                "Expected token {expected_token:?}, but received token of type {found_token:?}"
            ),
            ParseError::ExpectedToken => {
                write!(f, "Expected to receive a token, but no token was received")
            }
            ParseError::UnknownToken(token) => write!(
                f,
                "Received unknown token of type {token:?}, don't know how to handle it"
            ),
            ParseError::ExpressionError(error) => write!(f, "{error}"),
            ParseError::ParseIntegerError(token, error) => write!(
                f,
                "Tried to parse token {token:?} as an integer, but got error {error}"
            ),
            ParseError::NoPrefixExpression(token) => {
                write!(f, "No prefix parse function for {token:?} found")
            }
            ParseError::NoPrefixPartner => write!(f, "Expected expression to follow prefix"),
            ParseError::NoInfixExpression(token) => {
                write!(f, "No infix parse function for {token:?} found")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::Program, parse_errors::ParseError, test_util::parse_program};

    #[test]
    fn test_parse_errors() {
        let source_code = "
            foo: .
        ";

        let program: Program = parse_program(source_code);

        program.parse_errors.iter().for_each(|parse_error| {
            assert!(matches!(parse_error, ParseError::NoPrefixExpression(_)))
        });
    }
}
