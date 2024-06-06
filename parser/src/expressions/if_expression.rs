use crate::{
    ast::BlockStatement,
    lexer::token::{Precedence, Token},
    parse_errors::{ParseError, TokenExpectation},
    parser::Parser,
};

use super::expression::Expression;

#[derive(PartialEq, Debug)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl IfExpression {
    pub fn parse_if_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
        let next_token = parser.tokens.expect()?;
        let condition = Expression::parse(parser, next_token, Precedence::Lowest)?;

        parser.tokens.expect_token(Token::Assign)?;

        let consequence = Expression::parse_blockstatement(parser)?;

        let alternative = Self::parse_alternative(parser)?;

        Ok(Expression::If(IfExpression {
            condition: Box::from(condition),
            consequence,
            alternative,
        }))
    }

    fn parse_alternative(parser: &mut Parser) -> Result<Option<BlockStatement>, ParseError> {
        let alternative = match parser.tokens.consume() {
            Some(Token::Lasagna) => Ok(None),
            Some(Token::Else) => {
                parser.tokens.expect_token(Token::Assign)?;
                let else_block = Some(Expression::parse_blockstatement(parser)?);
                parser.tokens.expect_token(Token::Lasagna)?;
                Ok(else_block)
            }
            Some(unexpected_token) => Err(ParseError::multiple_unexpected(
                Vec::from([Token::Lasagna, Token::Else]),
                Some(&unexpected_token),
            )),
            None => Err(ParseError::ExpectedToken),
        }?;
        Ok(alternative)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{BlockStatement, Operator, Program, Statement},
        expressions::expression_statement::ExpressionStatement,
        test_util::{
            create_identifierliteral, create_if_condition, create_infix_expression,
            has_parser_errors, parse_program,
        },
    };

    #[test]
    fn test_if_expression() {
        struct TestCase {
            input: String,
            expected: Statement,
        }
        let test_cases: [TestCase; 2] = [
            (
                "if x < y: x.~",
                create_if_condition(
                    create_infix_expression(
                        create_identifierliteral("x"),
                        create_identifierliteral("y"),
                        Operator::LessThan,
                    ),
                    BlockStatement {
                        statements: Vec::from([Statement::Expression(ExpressionStatement {
                            expression: create_identifierliteral("x"),
                        })]),
                    },
                    None,
                ),
            ),
            (
                "if x > y: x. else: y.~",
                create_if_condition(
                    create_infix_expression(
                        create_identifierliteral("x"),
                        create_identifierliteral("y"),
                        Operator::GreaterThan,
                    ),
                    BlockStatement {
                        statements: Vec::from([Statement::Expression(ExpressionStatement {
                            expression: create_identifierliteral("x"),
                        })]),
                    },
                    Some(BlockStatement {
                        statements: Vec::from([Statement::Expression(ExpressionStatement {
                            expression: create_identifierliteral("y"),
                        })]),
                    }),
                ),
            ),
        ]
        .map(|(input, expected)| TestCase {
            input: input.to_string(),
            expected,
        });

        for test_case in test_cases {
            let program: Program = parse_program(&test_case.input);

            if has_parser_errors(&program) {
                let test_input = test_case.input;
                println!("Program: {test_input}");
                panic!("Failed due to parse errors");
            }

            let statement = program.statements.first().expect("Should be one statement");

            assert_eq!(
                statement, &test_case.expected,
                "Parsed statement should match testcase"
            );
            assert_eq!(program.statements.len(), 1, "Should only parse 1 statement");
        }
    }
}
