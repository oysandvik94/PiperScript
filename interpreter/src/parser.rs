mod assign_statement;
pub(crate) mod ast;
pub(crate) mod expressions;
pub(crate) mod lexer;
pub(crate) mod parse_errors;
mod return_statement;

#[cfg(test)]
pub(crate) mod test_util;

use tracing::{event, span, Level};

use crate::{
    parser::assign_statement::AssignStatement,
    parser::ast::Statement,
    parser::expressions::expression_statement::ExpressionStatement,
    parser::lexer::{lexedtokens::LexedTokens, token::Token},
    parser::parse_errors::ParseError,
    parser::return_statement::ReturnStatement,
};

pub struct Parser {
    pub tokens: LexedTokens,
}

pub enum ParsedProgram {
    ValidProgram(Vec<Statement>),
    InvalidProgram(Vec<ParseError>),
}

impl Parser {
    pub fn parse_tokens(tokens: LexedTokens) -> ParsedProgram {
        let mut parser = Parser { tokens };

        parser.parse_program()
    }

    fn parse_program(&mut self) -> ParsedProgram {
        let mut statements: Vec<Statement> = Vec::new();
        let mut parse_errors: Vec<ParseError> = Vec::new();

        while self.tokens.peek().is_some() {
            let statement_span = span!(Level::DEBUG, "Statement");
            let _enter = statement_span.enter();

            match self.parse_statement() {
                Ok(parsed_statement) => {
                    event!(Level::DEBUG, "Parsed statement: {parsed_statement:?}",);
                    statements.push(parsed_statement)
                }
                Err(parse_error) => {
                    event!(Level::DEBUG, "Error parsing statement: {parse_error:?}");
                    self.tokens.iterate_to_next_statement();
                    parse_errors.push(parse_error)
                }
            };
        }

        if !parse_errors.is_empty() {
            return ParsedProgram::InvalidProgram(parse_errors);
        }

        ParsedProgram::ValidProgram(statements)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.tokens.peek() {
            Some(Token::Return) => ReturnStatement::parse_return_statement(self),
            Some(Token::Let) => AssignStatement::parse(self),
            Some(_) => ExpressionStatement::parse(self),
            None => Err(ParseError::ExpectedToken),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util;

    #[test]
    fn test_operator_precedence() {
        struct TestCase {
            input: String,
            expected: String,
        }
        let test_cases: [TestCase; 24] = [
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
