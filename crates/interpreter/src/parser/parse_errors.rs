use std::{fmt::Display, num::ParseIntError};

use lexer::token::TokenKind;

use crate::parser::lexer;

use super::lexer::token::Token;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub token: Token,
    pub kind: ParseErrorKind,
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnexpectedToken(TokenKind),
    ExpectedToken,
    UnknownToken,
    ExpressionError(String),
    NoPrefixExpression,
    NoInfixExpression,
    ParseIntegerError(ParseIntError),
    NoPrefixPartner,
    UnfinishedIfStatement,
    UnfinishedFunctionDeclaration,
    FunctionBlockError(Box<ParseError>),
    IfBlockError(Box<ParseError>),
}

impl ParseError {
    pub fn new(token: Token, kind: ParseErrorKind) -> ParseError {
        ParseError { token, kind }
    }
}

pub trait StatementParseError {}

#[derive(Debug)]
pub enum TokenExpectation {
    SingleExpectation(TokenKind),
    MultipleExpectation(Vec<TokenKind>),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token = &self.token;
        match &self.kind {
            ParseErrorKind::UnexpectedToken(token_kind) => write!(
                f,
                "Expected token {token_kind:?}, but received token of type {token:?}"
            ),
            ParseErrorKind::ExpectedToken => {
                write!(f, "Expected to receive a token, but no token was received")
            }
            ParseErrorKind::UnknownToken => write!(
                f,
                "Received unknown token of type {token:?}, don't know how to handle it"
            ),
            ParseErrorKind::ExpressionError(error) => write!(f, "{error}"),
            ParseErrorKind::ParseIntegerError(error) => write!(
                f,
                "Tried to parse token {token:?} as an integer, but got error {error}"
            ),
            ParseErrorKind::NoPrefixExpression => {
                write!(f, "No prefix parse function for {token:?} found")
            }
            ParseErrorKind::NoPrefixPartner => write!(f, "Expected expression to follow prefix"),
            ParseErrorKind::NoInfixExpression => {
                write!(f, "No infix parse function for {token:?} found")
            }
            ParseErrorKind::UnfinishedIfStatement => writeln!(
                f,
                "If statement should be closed with ~ or else, but found {token:?} instead"
            ),
            ParseErrorKind::UnfinishedFunctionDeclaration => writeln!(
                f,
                "Function should be closed with , or ), but found {token:?} instead"
            ),
            ParseErrorKind::FunctionBlockError(error) => {
                writeln!(f, "Error parsing statements in function: {error}")
            }
            ParseErrorKind::IfBlockError(error) => {
                writeln!(f, "Error parsing statements in if condition: {error}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{parse_errors::ParseErrorKind, ParsedProgram},
        test_util,
    };

    #[test]
    fn test_parse_errors() {
        let source_code = "
            foo: .
        ";

        let program = test_util::parse_program(source_code);

        match program {
            ParsedProgram::ValidProgram(_) => panic!("Program did not fail"),
            ParsedProgram::InvalidProgram(parse_errors) => {
                parse_errors.iter().for_each(|parse_error| {
                    assert!(matches!(
                        parse_error.parse_error.kind,
                        ParseErrorKind::NoPrefixExpression
                    ))
                });
            }
        }
    }
}
