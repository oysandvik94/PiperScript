use tracing::{event, span, Level};

use crate::parser::{
    ast::Statement,
    lexer::token::{Precedence, TokenKind},
    parse_errors::ParseError,
    Parser, StatementError, StatementType,
};

use super::expression::Expression;

pub fn parse(parser: &mut Parser) -> Result<Statement, StatementError> {
    let expression_statement_span = span!(Level::DEBUG, "Expression");
    let _enter = expression_statement_span.enter();

    event!(Level::DEBUG, "Parsing expression statement");

    let expression = Expression::parse(parser, Precedence::Lowest)
        .map_err(self::handle_parse_error)
        .inspect_err(|err| {
            event!(
                Level::DEBUG,
                "Found error parsing expression statement: {err}"
            )
        })?;

    parser.lexer.expect_optional_token(TokenKind::Period);

    Ok(Statement::Expression(expression))
}

fn handle_parse_error(parse_error: ParseError) -> StatementError {
    StatementError {
        parse_error,
        statement_type: StatementType::Expression,
    }
}
