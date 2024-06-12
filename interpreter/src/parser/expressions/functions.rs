use std::fmt::Display;

use tracing::{event, span, Level};

use crate::parser::{
    ast::{BlockStatement, Identifier},
    lexer::token::{Precedence, Token},
    parse_errors::{ParseError, TokenExpectation},
    Parser,
};

use super::expression::Expression;

#[derive(PartialEq, Debug)]
pub struct FunctionLiteral {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

#[derive(PartialEq, Debug)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let parameters: Vec<String> = self
            .parameters
            .iter()
            .map(|ident| ident.0.clone())
            .collect();
        write!(f, "fn({}): {}", parameters.join(","), self.body)
    }
}

impl FunctionLiteral {
    pub fn parse(parser: &mut Parser) -> Result<Expression, ParseError> {
        let function_span = span!(Level::DEBUG, "Function");
        let _enter = function_span.enter();

        event!(Level::DEBUG, "Parsing function");
        let parameters: Vec<Identifier> = Self::parse_function_parameters(parser)?;
        event!(Level::DEBUG, "Found parameters {parameters:?}");

        parser.tokens.expect_token(Token::Assign)?;

        let body: BlockStatement = Expression::parse_blockstatement(parser)?;
        parser.tokens.expect_token(Token::Lasagna)?;

        Ok(Expression::Function(FunctionLiteral { parameters, body }))
    }

    fn parse_function_parameters(parser: &mut Parser) -> Result<Vec<Identifier>, ParseError> {
        let mut parameters: Vec<Identifier> = Vec::from([]);
        while let Some(token) = parser.tokens.consume() {
            match token {
                Token::LParen | Token::Comma => match parser.tokens.peek() {
                    Some(Token::RParen) => {
                        parser.tokens.consume();
                        return Ok(parameters);
                    }
                    Some(_) => parameters.push(Expression::parse_literal(parser)?),
                    None => return Err(ParseError::ExpectedToken),
                },
                Token::RParen => return Ok(parameters),
                unexpected_token => {
                    return Err(ParseError::UnexpectedToken {
                        expected_token: TokenExpectation::MultipleExpectation(Vec::from([
                            Token::Comma,
                            Token::RParen,
                        ])),
                        found_token: Some(unexpected_token),
                    })
                }
            }
        }

        Ok(parameters)
    }
}

impl CallExpression {
    pub fn parse(parser: &mut Parser, function: Expression) -> Result<Expression, ParseError> {
        let call_span = span!(Level::DEBUG, "Call");
        let _enter = call_span.enter();

        Ok(Expression::Call(CallExpression {
            function: Box::from(function),
            arguments: Self::parse_function_arguments(parser)?,
        }))
    }

    fn parse_function_arguments(parser: &mut Parser) -> Result<Vec<Expression>, ParseError> {
        event!(Level::DEBUG, "Parsing function arguments");
        let mut parameters: Vec<Expression> = Vec::from([]);
        while let Some(token) = parser.tokens.peek() {
            match token {
                Token::RParen => {
                    parser.tokens.consume();
                    return Ok(parameters);
                }
                Token::Comma => {
                    parser.tokens.consume();
                }
                _ => {
                    let current_token = parser
                        .tokens
                        .consume()
                        .expect("Expected a token after peeking");

                    parameters.push(Expression::parse(
                        parser,
                        current_token,
                        Precedence::Lowest,
                    )?);
                }
            }
        }

        Ok(parameters)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{
        ast::{BlockStatement, Operator, Statement},
        expressions::{
            expression::Expression, expression_statement::ExpressionStatement,
            functions::CallExpression,
        },
        test_util,
    };

    #[test]
    fn test_call_expression() {
        test_util::setup_logger();

        let input = "add(1, 2 * 2, 4 + 5)";

        let program = test_util::parse_program(input);
        if test_util::has_parser_errors(&program) {
            panic!("Failed due to parse errors");
        }

        let binding = test_util::expect_parsed_program(input);
        let statement = binding.first().expect("Should parse one statement");

        let expected_arguments = Vec::from([
            Expression::IntegerLiteral(1),
            test_util::create_infix_expression(
                Expression::IntegerLiteral(2),
                Expression::IntegerLiteral(2),
                Operator::Multiply,
            ),
            test_util::create_infix_expression(
                Expression::IntegerLiteral(4),
                Expression::IntegerLiteral(5),
                Operator::Plus,
            ),
        ]);
        let expected_statement = Expression::Call(CallExpression {
            function: Box::from(test_util::create_identifierliteral("add")),
            arguments: expected_arguments,
        });
        assert_eq!(
            statement,
            &Statement::Expression(ExpressionStatement {
                expression: expected_statement
            }),
            "Parsed statement should match testcase"
        );
    }

    #[test]
    fn test_function_expression() {
        struct TestCase {
            input: String,
            expected: Statement,
        }
        let test_cases: [TestCase; 2] = [
            (
                "fn(x, y): x + y~",
                test_util::create_function_expression(
                    Vec::from(["x", "y"]),
                    BlockStatement {
                        statements: Vec::from([Statement::Expression(ExpressionStatement {
                            expression: test_util::create_infix_expression(
                                test_util::create_identifierliteral("x"),
                                test_util::create_identifierliteral("y"),
                                Operator::Plus,
                            ),
                        })]),
                    },
                ),
            ),
            (
                "fn(): x.y.~",
                test_util::create_function_expression(
                    Vec::from([]),
                    BlockStatement {
                        statements: Vec::from([
                            Statement::Expression(ExpressionStatement {
                                expression: test_util::create_identifierliteral("x"),
                            }),
                            Statement::Expression(ExpressionStatement {
                                expression: test_util::create_identifierliteral("y"),
                            }),
                        ]),
                    },
                ),
            ),
        ]
        .map(|(input, expected)| TestCase {
            input: input.to_string(),
            expected,
        });

        for test_case in test_cases {
            let binding = test_util::expect_parsed_program(&test_case.input);
            let statement = binding.first().expect("Should parse one statement");

            assert_eq!(
                statement, &test_case.expected,
                "Parsed statement should match testcase"
            );
        }
    }
}
