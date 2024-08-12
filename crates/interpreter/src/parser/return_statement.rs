use std::fmt::Display;

use crate::{
    parser::ast::StatementType,
    parser::expressions::expression::Expression,
    parser::lexer::token::{Precedence, TokenKind},
    parser::parse_errors::ParseError,
};

use super::{Parser, StatementError, StatementErrorType};

#[derive(PartialEq, Debug, Clone)]
pub struct ReturnStatement {
    pub return_value: Expression,
}

impl ReturnStatement {
    pub fn parse_return_statement(parser: &mut Parser) -> Result<StatementType, StatementError> {
        parser
            .lexer
            .expect_token(TokenKind::Return)
            .map_err(Self::handle_parse_error)?;

        let expression =
            Expression::parse(parser, Precedence::Lowest).map_err(Self::handle_parse_error)?;

        parser.lexer.expect_optional_token(TokenKind::Period);
        Ok(StatementType::Return(ReturnStatement {
            return_value: expression,
        }))
    }

    fn handle_parse_error(parse_error: ParseError) -> StatementError {
        StatementError {
            parse_error,
            statement_type: StatementErrorType::Return,
        }
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {}", self.return_value)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            ast::StatementType, expressions::expression::Expression,
            return_statement::ReturnStatement,
        },
        test_util,
    };

    #[test]
    fn parse_return_statement_test() {
        test_util::setup_logger();
        let source_code = "
            return 5.
            return foobar.
        ";

        let statements = test_util::expect_parsed_program(source_code);

        assert_eq!(
            statements.len(),
            2,
            "Program should be parsed to 3 statements"
        );

        let first_statement = statements.first().expect("Should get statement");
        assert_eq!(
            first_statement.statement_type,
            StatementType::Return(ReturnStatement {
                return_value: Expression::IntegerLiteral(5)
            })
        );
        let second_statement = statements.get(1).expect("Should get statement");
        assert_eq!(
            second_statement.statement_type,
            StatementType::Return(ReturnStatement {
                return_value: test_util::create_identifierliteral("foobar")
            })
        );
    }
}
