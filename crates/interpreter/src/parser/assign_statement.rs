use std::fmt::Display;

use super::{
    ast::{Identifier, Statement},
    expressions::expression::Expression,
    lexer::token::{Precedence, Token},
    parse_errors::ParseError,
    Parser,
};

#[derive(PartialEq, Debug, Clone)]
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
        let expression = Expression::parse(parser, next_token, Precedence::Lowest)?;

        parser.tokens.expect_optional_token(Token::Period);

        Ok(Statement::Assign(AssignStatement {
            identifier,
            assignment: expression,
        }))
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
            ast::{Identifier, Statement},
            expressions::expression::Expression,
        },
        test_util,
    };

    #[test]
    fn parse_assign_statement() {
        let source_code = "
            let x: 5.
            let y: 10.
            let foobar: 54456.
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
        found: &Statement,
        expected_identifier: &Identifier,
        expected_expression: &Expression,
    ) {
        match found {
            Statement::Assign(assign_statement) => {
                assert_eq!(&assign_statement.identifier, expected_identifier);
                assert_eq!(&assign_statement.assignment, expected_expression);
            }
            incorrect => panic!("Expected let-statement, but got {incorrect:?}"),
        };
    }
}
