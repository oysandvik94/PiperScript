use tracing::{event, span, Level};

use crate::parser::{
    ast::Statement,
    lexer::token::{Precedence, Token},
    parse_errors::ParseError,
    Parser,
};

use super::expression::Expression;

pub fn parse(parser: &mut Parser) -> Result<Statement, ParseError> {
    let first_token = parser.tokens.expect()?;
    event!(
        Level::DEBUG,
        "Parsing expression statement with starting token {first_token:?}"
    );

    let expression_statement_span = span!(Level::DEBUG, "Expression");
    let _enter = expression_statement_span.enter();

    let expression = Expression::parse(parser, &first_token, Precedence::Lowest)?;

    parser.tokens.expect_optional_token(Token::Period);

    Ok(Statement::Expression(expression))
}
