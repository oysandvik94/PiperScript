use std::fmt::Display;

use tracing::{event, span, Level};

use crate::parser::parse_errors::ParseErrorKind;

use super::{
    ast::{Identifier, StatementType},
    expressions::expression::Expression,
    lexer::token::{Precedence, TokenKind},
    parse_errors::ParseError,
    Parser, StatementError, StatementErrorType,
};

#[derive(PartialEq, Debug, Clone)]
pub struct AssignStatement {
    pub identifier: Identifier,
    pub assignment: Expression,
}

impl AssignStatement {
    pub fn parse(parser: &mut Parser) -> Result<StatementType, StatementError> {
        let let_stmt_span = span!(Level::DEBUG, "Assign");
        let _enter = let_stmt_span.enter();

        match Self::parse_assignment(parser) {
            Ok(statement) => Ok(statement),
            Err(err) => {
                event!(
                    Level::DEBUG,
                    "Found error parsing assignment statement: {err}"
                );

                Err(err)
            }
        }
    }

    fn parse_assignment(parser: &mut Parser) -> Result<StatementType, StatementError> {
        parser
            .lexer
            .expect_token(TokenKind::Let)
            .map_err(Self::handle_parse_error)?;

        let identifier = parser
            .lexer
            .expected_identifier()
            .map_err(Self::handle_parse_error)?;

        let colon = parser
            .lexer
            .expect_token(TokenKind::Colon)
            .map_err(Self::handle_parse_error)?;

        event!(Level::DEBUG, "Parsing binding for assignstatement");
        let expression =
            Expression::parse(parser, Precedence::Lowest).map_err(|error| match error.kind {
                ParseErrorKind::NoPrefixExpression => {
                    if error.token.token_kind.is_beginning_of_statement() {
                        StatementError {
                            parse_error: ParseError {
                                kind: ParseErrorKind::NoPrefixExpression,
                                token: colon,
                            },
                            statement_type: StatementErrorType::Let,
                        }
                    } else {
                        Self::handle_parse_error(error)
                    }
                }
                _ => Self::handle_parse_error(error),
            })?;

        parser.lexer.expect_optional_token(TokenKind::Period);

        Ok(StatementType::Assign(AssignStatement {
            identifier,
            assignment: expression,
        }))
    }

    fn handle_parse_error(parse_error: ParseError) -> StatementError {
        StatementError {
            parse_error,
            statement_type: StatementErrorType::Let,
        }
    }
}

impl Display for AssignStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {}: {}.", self.identifier, self.assignment)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            ast::{Identifier, StatementType},
            expressions::expression::Expression,
        },
        test_util,
    };

    #[test]
    fn parse_test_statement() {
        test_util::setup_logger();
        let source_code = "let x: 5";

        let _ = test_util::expect_parsed_program(source_code);
    }
    #[test]
    fn parse_assign_statement() {
        test_util::setup_logger();
        let source_code = "
            let x: 5
            let y: 10
            let foobar: 54456
        ";

        let statements = test_util::expect_parsed_program(source_code);

        assert_eq!(
            statements.len(),
            3,
            "Program should be parsed to 3 statements"
        );

        let expected_identifiers: [Identifier; 3] = [
            Identifier(String::from("x")),
            Identifier(String::from("y")),
            Identifier(String::from("foobar")),
        ];

        let expected_expression: [Expression; 3] = [
            Expression::IntegerLiteral(5),
            Expression::IntegerLiteral(10),
            Expression::IntegerLiteral(54456),
        ];

        expected_identifiers
            .iter()
            .enumerate()
            .for_each(|(idx, ident)| {
                assert_let_statement(&statements[idx], ident, &expected_expression[idx])
            });
    }

    fn assert_let_statement(
        found: &StatementType,
        expected_identifier: &Identifier,
        expected_expression: &Expression,
    ) {
        match found {
            StatementType::Assign(assign_statement) => {
                assert_eq!(&assign_statement.identifier, expected_identifier);
                assert_eq!(&assign_statement.assignment, expected_expression);
            }
            incorrect => panic!("Expected let-statement, but got {incorrect:?}"),
        };
    }
}
