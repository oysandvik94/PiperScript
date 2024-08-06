pub mod assign_statement;
pub mod ast;
pub mod expressions;
pub mod lexer;
pub mod parse_errors;
pub mod return_statement;

use std::fmt::Display;

use expressions::expression_statement;
use parse_errors::ParseErrorKind;
use tracing::{event, span, Level};

use crate::{
    parser::assign_statement::AssignStatement,
    parser::ast::Statement,
    parser::lexer::{lexedtokens::Lexer, token::TokenKind},
    parser::parse_errors::ParseError,
    parser::return_statement::ReturnStatement,
};

pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
}

pub enum ParsedProgram {
    ValidProgram(Vec<Statement>),
    InvalidProgram(Vec<StatementError>),
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Parser {
            lexer: Lexer::new(input),
        }
    }
    pub fn parse_tokens(tokens: Lexer) -> ParsedProgram {
        let mut parser = Parser { lexer: tokens };

        parser.parse_program()
    }

    pub fn parse_program(&mut self) -> ParsedProgram {
        let mut statements: Vec<Statement> = Vec::new();
        let mut parse_errors: Vec<StatementError> = Vec::new();

        while self.lexer.has_next() {
            let statement_span = span!(Level::DEBUG, "Statement");
            let _enter = statement_span.enter();

            match self.parse_statement() {
                Ok(parsed_statement) => {
                    event!(Level::DEBUG, "Parsed statement: {parsed_statement:?}",);
                    statements.push(parsed_statement)
                }
                Err(parse_error) => {
                    // TODO: maybe it is possible to defer creating the error object
                    // with the correct document location to here
                    self.lexer.iterate_to_next_statement();
                    parse_errors.push(parse_error)
                }
            };
        }

        if !parse_errors.is_empty() {
            return ParsedProgram::InvalidProgram(parse_errors);
        }

        ParsedProgram::ValidProgram(statements)
    }

    fn parse_statement(&mut self) -> Result<Statement, StatementError> {
        let current_token = self
            .lexer
            .expect_peek()
            .expect("Function should be called after assertion");

        event!(
            Level::DEBUG,
            "Parsing statement begining with token_kind: {:?}",
            current_token
        );
        match current_token.token_kind {
            TokenKind::Return => ReturnStatement::parse_return_statement(self),
            TokenKind::Let => AssignStatement::parse(self),
            _ => expression_statement::parse(self),
        }
    }
}

#[derive(Debug)]
pub struct StatementError {
    parse_error: ParseError,
    statement_type: StatementType,
}

// TODO: Investigate whether we can use a struct for statements with this
// as the type, instead of an enum of statements
#[derive(Debug)]
pub enum StatementType {
    Return,
    Let,
    Expression,
}

impl Display for StatementError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Error on line {}: ",
            self.parse_error.token.location.line_number
        )?;
        match self.statement_type {
            StatementType::Return => writeln!(f, "{}", self.parse_error),
            StatementType::Let => match &self.parse_error.kind {
                ParseErrorKind::NoPrefixExpression => {
                    writeln!(f, "expected binding to let statement")
                }
                ParseErrorKind::UnexpectedToken(expected_token) => match expected_token {
                    TokenKind::Ident(_) => {
                        writeln!(f, "expected identifier in let statement")
                    }
                    not_handled => writeln!(
                        f,
                        "expected {} in variable declaration, got {}",
                        expected_token, not_handled
                    ),
                },
                _ => writeln!(f, "{}", self.parse_error),
            },
            StatementType::Expression => writeln!(f, "{}", self.parse_error),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test_util;

    #[test]
    fn test_operator_precedence() {
        struct TestCase {
            input: String,
            expected: String,
        }
        let test_cases: [TestCase; 26] = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4. -5 * 5.", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == -3 < 4", "((5 > 4) == ((-3) < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ]
        .map(|(input, expected)| TestCase {
            input: input.to_string(),
            expected: expected.to_string(),
        });

        for testcase in test_cases {
            let actual = test_util::parse_program(&testcase.input);

            if test_util::has_parser_errors(&actual) {
                let expected = testcase.expected;
                println!("{expected}");
                panic!("Found parser errors");
            }

            assert_eq!(actual.to_string().replace('\n', ""), testcase.expected);
        }
    }
}
