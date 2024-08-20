use crate::parser::{
    ast::BlockStatement,
    lexer::token::{Precedence, Token, TokenKind},
    parse_errors::{ParseError, ParseErrorKind},
    Parser,
};

use super::expression::Expression;

#[derive(PartialEq, Debug, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl IfExpression {
    pub fn parse_if_expression(
        parser: &mut Parser,
    ) -> Result<(Expression, Vec<Token>), ParseError> {
        let mut tokens: Vec<Token> = Vec::new();
        let (condition, mut parsed_tokens) = Expression::parse(parser, Precedence::Lowest)?;
        tokens.append(&mut parsed_tokens);

        tokens.push(parser.lexer.expect_token(TokenKind::Colon)?);

        let (consequence, mut parsed_tokens) =
            Expression::parse_blockstatement(parser).map_err(handle_if_error())?;
        tokens.append(&mut parsed_tokens);

        let (alternative, mut parsed_tokens) = Self::parse_alternative(parser)?;
        tokens.append(&mut parsed_tokens);

        let if_expr = Expression::If(IfExpression {
            condition: Box::from(condition),
            consequence,
            alternative,
        });
        Ok((if_expr, tokens))
    }

    fn parse_alternative(
        parser: &mut Parser,
    ) -> Result<(Option<BlockStatement>, Vec<Token>), ParseError> {
        let mut tokens: Vec<Token> = Vec::new();
        let token = parser.lexer.expect()?;

        let alternative = match token.token_kind {
            TokenKind::Lasagna => Ok(None),
            TokenKind::Else => {
                tokens.push(parser.lexer.expect_token(TokenKind::Colon)?);

                let (else_block, mut parsed_tokens) =
                    Expression::parse_blockstatement(parser).map_err(handle_if_error())?;
                tokens.append(&mut parsed_tokens);

                tokens.push(parser.lexer.expect_token(TokenKind::Lasagna)?);
                Ok(Some(else_block))
            }
            _ => Err(ParseError::new(
                token.clone(),
                ParseErrorKind::UnfinishedIfStatement,
            )),
        }?;

        tokens.push(token);
        Ok((alternative, tokens))
    }
}

fn handle_if_error() -> impl FnOnce(crate::parser::StatementError) -> ParseError {
    |e| {
        let error = e.parse_error.clone();

        ParseError::new(
            e.parse_error.token.clone(),
            ParseErrorKind::IfBlockError(Box::new(error)),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::ast::{BlockStatement, Operator, StatementType},
        test_util,
    };

    #[test]
    fn test_if_expression() {
        struct TestCase {
            input: String,
            expected: StatementType,
        }
        let test_cases: [TestCase; 2] = [
            (
                "if x < y: x.~",
                test_util::create_if_condition(
                    test_util::create_infix_expression(
                        test_util::create_identifierliteral("x"),
                        test_util::create_identifierliteral("y"),
                        Operator::LessThan,
                    ),
                    BlockStatement {
                        statements: Vec::from([StatementType::Expression(
                            test_util::create_identifierliteral("x"),
                        )]),
                    },
                    None,
                ),
            ),
            (
                "if x > y: x. else: y.~",
                test_util::create_if_condition(
                    test_util::create_infix_expression(
                        test_util::create_identifierliteral("x"),
                        test_util::create_identifierliteral("y"),
                        Operator::GreaterThan,
                    ),
                    BlockStatement {
                        statements: Vec::from([StatementType::Expression(
                            test_util::create_identifierliteral("x"),
                        )]),
                    },
                    Some(BlockStatement {
                        statements: Vec::from([StatementType::Expression(
                            test_util::create_identifierliteral("y"),
                        )]),
                    }),
                ),
            ),
        ]
        .map(|(input, expected)| TestCase {
            input: input.to_string(),
            expected,
        });

        for test_case in test_cases {
            let statements = test_util::expect_parsed_program(&test_case.input);

            let statement = statements.first().expect("Should be one statement");

            assert_eq!(
                statement.statement_type, test_case.expected,
                "Parsed statement should match testcase"
            );
        }
    }
}
