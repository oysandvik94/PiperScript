use crate::{
    ast::{Expression, Identifier, Statement},
    lexer::{
        lexedtokens::LexedTokens,
        token::{Precedence, Token},
    },
    parse_errors::ParseError,
    parser::Parser,
};

#[derive(PartialEq, Debug)]
pub struct AssignStatement {
    pub identifier: Identifier,
    pub assignment: Expression,
}

impl AssignStatement {
    pub fn parse(parser: &mut Parser) -> Result<Statement, ParseError> {
        parser.tokens.expect_token(Token::Let)?;
        let identifier = parser.tokens.expected_identifier()?;
        parser.tokens.expect_token(Token::Assign)?;

        let next_token = parser.tokens.expect()?;
        let expression = parser.parse_expression(next_token, Precedence::Lowest)?;

        parser.tokens.expect_optional_token(Token::Period);

        Ok(Statement::Assign(AssignStatement {
            identifier,
            assignment: expression,
        }))
    }
}
