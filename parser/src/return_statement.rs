use std::fmt::Display;

use crate::{
    ast::{Expression, Statement},
    lexer::token::{Precedence, Token},
    parse_errors::ParseError,
    parser::Parser,
};

#[derive(PartialEq, Debug)]
pub struct ReturnStatement {
    pub return_value: Expression,
}

impl ReturnStatement {
    pub fn parse_return_statement(parser: &mut Parser) -> Result<Statement, ParseError> {
        parser.tokens.expect_token(Token::Return)?;
        let next_token = parser.tokens.expect()?;
        let expression = parser.parse_expression(next_token, Precedence::Lowest)?;

        parser.tokens.expect_optional_token(Token::Period);

        Ok(Statement::Return(ReturnStatement {
            return_value: expression,
        }))
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
        ast::{Expression, Program, Statement},
        return_statement::ReturnStatement,
        test_util::*,
    };

    #[test]
    fn parse_return_statement_test() {
        let source_code = "
            return 5.
            return foobar.
        ";

        let program: Program = parse_program(source_code);

        has_parser_errors(&program);
        assert_eq!(
            program.statements.len(),
            2,
            "Program should be parsed to 3 statements"
        );

        let first_statement = program.statements.first().expect("Should get statement");
        assert_eq!(
            first_statement,
            &Statement::Return(ReturnStatement {
                return_value: Expression::IntegerLiteral(5)
            })
        );
        let second_statement = program.statements.get(1).expect("Should get statement");
        assert_eq!(
            second_statement,
            &Statement::Return(ReturnStatement {
                return_value: create_identifierliteral("foobar")
            })
        );
    }
}
