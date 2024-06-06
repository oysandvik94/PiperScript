use std::fmt::Display;

use tracing::{event, span, Level};

use crate::{
    ast::{Expression, Statement},
    lexer::token::{Precedence, Token},
    parse_errors::ParseError,
    parser::Parser,
};

#[derive(PartialEq, Debug)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}

impl ExpressionStatement {
    pub fn parse(parser: &mut Parser) -> Result<Statement, ParseError> {
        let first_token = parser.tokens.expect()?;
        event!(
            Level::DEBUG,
            "Parsing expression statement with starting token {first_token:?}"
        );

        let expression_statement_span = span!(Level::DEBUG, "Expression");
        let _enter = expression_statement_span.enter();

        let expression = parser.parse_expression(first_token, Precedence::Lowest)?;

        parser.tokens.expect_optional_token(Token::Period);

        Ok(Statement::Expression(ExpressionStatement { expression }))
    }
}
