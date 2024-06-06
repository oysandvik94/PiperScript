use std::fmt::Display;

use tracing::{event, span, Level};

use crate::{
    ast::{BlockStatement, Identifier},
    lexer::token::{Precedence, Token},
    parse_errors::{ParseError, TokenExpectation},
    parser::Parser,
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
    use crate::{
        ast::{BlockStatement, Operator, Program, Statement},
        expressions::{
            expression::Expression, expression_statement::ExpressionStatement,
            functions::CallExpression,
        },
        test_util::{
            create_function_expression, create_identifierliteral, create_infix_expression,
            has_parser_errors, parse_program, setup_logger,
        },
    };

    #[test]
    fn test_call_expression() {
        setup_logger();

        let input = "add(1, 2 * 2, 4 + 5)";

        let program = parse_program(input);
        if has_parser_errors(&program) {
            panic!("Failed due to parse errors");
        }

        let statement = program
            .statements
            .first()
            .expect("Expected a statement to be parsed");

        let expected_arguments = Vec::from([
            Expression::IntegerLiteral(1),
            create_infix_expression(
                Expression::IntegerLiteral(2),
                Expression::IntegerLiteral(2),
                Operator::Multiply,
            ),
            create_infix_expression(
                Expression::IntegerLiteral(4),
                Expression::IntegerLiteral(5),
                Operator::Plus,
            ),
        ]);
        let expected_statement = Expression::Call(CallExpression {
            function: Box::from(create_identifierliteral("add")),
            arguments: expected_arguments,
        });
        assert_eq!(
            statement,
            &Statement::Expression(ExpressionStatement {
                expression: expected_statement
            }),
            "Parsed statement should match testcase"
        );
        assert_eq!(program.statements.len(), 1, "Should only parse 1 statement");
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
                create_function_expression(
                    Vec::from(["x", "y"]),
                    BlockStatement {
                        statements: Vec::from([Statement::Expression(ExpressionStatement {
                            expression: create_infix_expression(
                                create_identifierliteral("x"),
                                create_identifierliteral("y"),
                                Operator::Plus,
                            ),
                        })]),
                    },
                ),
            ),
            (
                "fn(): x.y.~",
                create_function_expression(
                    Vec::from([]),
                    BlockStatement {
                        statements: Vec::from([
                            Statement::Expression(ExpressionStatement {
                                expression: create_identifierliteral("x"),
                            }),
                            Statement::Expression(ExpressionStatement {
                                expression: create_identifierliteral("y"),
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
