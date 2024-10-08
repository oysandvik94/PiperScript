use std::fmt::Display;

use tracing::{event, span, Level};

use crate::{
    eval::objects::Listable,
    parser::{
        ast::{BlockStatement, Identifier},
        lexer::token::{Token, TokenKind},
        parse_errors::{ParseError, ParseErrorKind},
        Parser,
    },
};

use super::expression::Expression;

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionLiteral {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

#[derive(PartialEq, Debug, Clone)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}): {}",
            self.parameters.to_commaseperated_list(),
            self.body
        )
    }
}

impl FunctionLiteral {
    pub fn parse(parser: &mut Parser) -> Result<(Expression, Vec<Token>), ParseError> {
        let function_span = span!(Level::DEBUG, "Function");
        let _enter = function_span.enter();

        let mut tokens = Vec::new();

        event!(Level::DEBUG, "Parsing function");
        let parameters: Vec<Identifier> = Self::parse_function_parameters(parser)?;
        event!(Level::DEBUG, "Found parameters {parameters:?}");

        tokens.push(parser.lexer.expect_token(TokenKind::Colon)?);

        let (body, mut parsed_tokens) = Expression::parse_blockstatement(parser).map_err(|e| {
            let error = e.parse_error.clone();
            ParseError::new(
                e.parse_error.token.clone(),
                ParseErrorKind::FunctionBlockError(Box::new(error)),
            )
        })?;
        tokens.append(&mut parsed_tokens);
        tokens.push(parser.lexer.expect_token(TokenKind::Lasagna)?);

        let expression_func = Expression::Function(FunctionLiteral { parameters, body });
        Ok((expression_func, tokens))
    }

    fn parse_function_parameters(parser: &mut Parser) -> Result<Vec<Identifier>, ParseError> {
        let mut parameters: Vec<Identifier> = Vec::from([]);
        while let Some(token) = parser.lexer.consume() {
            match token.token_kind {
                TokenKind::LParen | TokenKind::Comma => match parser.lexer.peek() {
                    Some(Token {
                        token_kind: TokenKind::RParen,
                        location: _,
                    }) => {
                        parser.lexer.consume();
                        return Ok(parameters);
                    }
                    Some(_) => parameters.push(Expression::parse_literal(parser)?),
                    None => {
                        return Err(ParseError::new(
                            token.clone(),
                            ParseErrorKind::ExpectedToken,
                        ))
                    }
                },
                TokenKind::RParen => return Ok(parameters),
                _ => {
                    return Err(ParseError::new(
                        token.clone(),
                        ParseErrorKind::UnfinishedFunctionDeclaration,
                    ))
                }
            }
        }

        Ok(parameters)
    }
}

impl CallExpression {
    pub fn parse(
        parser: &mut Parser,
        function: Expression,
    ) -> Result<(Expression, Vec<Token>), ParseError> {
        let call_span = span!(Level::DEBUG, "Call");
        let _enter = call_span.enter();

        let (expression_list, tokens) =
            Expression::parse_expression_list(parser, &TokenKind::RParen)?;

        let call_expression = Expression::Call(CallExpression {
            function: Box::from(function),
            arguments: expression_list,
        });

        Ok((call_expression, tokens))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            ast::{BlockStatement, Operator, StatementType},
            expressions::{expression::Expression, functions::CallExpression},
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
            test_util::create_integer_infix_expression(2, 2, Operator::Multiply),
            test_util::create_integer_infix_expression(4, 5, Operator::Plus),
        ]);
        let expected_statement = Expression::Call(CallExpression {
            function: Box::from(test_util::create_identifierliteral("add")),
            arguments: expected_arguments,
        });
        assert_eq!(
            &statement.statement_type,
            &StatementType::Expression(expected_statement),
            "Parsed statement should match testcase"
        );
    }

    #[test]
    fn test_function_expression() {
        struct TestCase {
            input: String,
            expected: StatementType,
        }
        let test_cases: [TestCase; 2] = [
            (
                "fn(x, y): x + y~",
                test_util::create_function_expression(
                    Vec::from(["x", "y"]),
                    BlockStatement {
                        statements: Vec::from([StatementType::Expression(
                            test_util::create_infix_expression(
                                test_util::create_identifierliteral("x"),
                                test_util::create_identifierliteral("y"),
                                Operator::Plus,
                            ),
                        )]),
                    },
                ),
            ),
            (
                "fn(): x.y.~",
                test_util::create_function_expression(
                    Vec::from([]),
                    BlockStatement {
                        statements: Vec::from([
                            StatementType::Expression(test_util::create_identifierliteral("x")),
                            StatementType::Expression(test_util::create_identifierliteral("y")),
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
                &statement.statement_type, &test_case.expected,
                "Parsed statement should match testcase"
            );
        }
    }
}
