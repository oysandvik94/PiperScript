use tracing::{event, span, Level};

use crate::parser::{
    ast::StatementType,
    lexer::token::{Precedence, Token, TokenKind},
    parse_errors::ParseError,
    Parser, StatementError, StatementErrorType,
};

use super::expression::Expression;

pub fn parse(parser: &mut Parser) -> Result<(StatementType, Vec<Token>), StatementError> {
    let expression_statement_span = span!(Level::DEBUG, "Expression");
    let _enter = expression_statement_span.enter();

    event!(Level::DEBUG, "Parsing expression statement");

    let (expression, mut tokens) = Expression::parse(parser, Precedence::Lowest)
        .map_err(self::handle_parse_error)
        .inspect_err(|err| {
            event!(
                Level::DEBUG,
                "Found error parsing expression statement: {err}"
            )
        })?;

    if let Some(token) = parser.lexer.expect_optional_token(TokenKind::Period) {
        tokens.push(token);
    }

    let expr = StatementType::Expression(expression);
    Ok((expr, tokens))
}

fn handle_parse_error(parse_error: ParseError) -> StatementError {
    StatementError {
        parse_error,
        statement_type: StatementErrorType::Expression,
    }
}
