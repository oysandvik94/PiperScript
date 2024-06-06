use tracing::{event, span, Level};

use crate::{
    ast::{BlockStatement, Identifier, Operator, Statement},
    lexer::token::{HasInfix, Precedence, Token},
    parse_errors::{ParseError, TokenExpectation},
    parser::Parser,
};

#[derive(PartialEq, Debug)]
pub enum Expression {
    IdentifierLiteral(Identifier),
    IntegerLiteral(i32),
    BooleanLiteral(bool),
    PrefixExpression {
        right: Box<Expression>,
        operator: Operator,
    },
    InfixExpression {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: Operator,
    },
    IfExpression {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    FunctionLiteral {
        parameters: Vec<Identifier>,
        body: BlockStatement,
    },
    CallExpression {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

impl Expression {
    pub fn parse(
        parser: &mut Parser,
        current_token: Token,
        precedence: Precedence,
    ) -> Result<Expression, ParseError> {
        event!(
            Level::DEBUG,
            "Parsing expression starting with token {:?} and precedence {:?}",
            current_token,
            precedence
        );

        let mut left = Self::parse_prefix_expression(parser, &current_token)?;
        event!(Level::DEBUG, "Found prefix expression {:?}", left);

        while parser.tokens.next_token_has_infix()
            && precedence < parser.tokens.next_token_precedence()
        {
            let next_token = parser.tokens.expect()?;
            left = Self::parse_infix_expression(parser, left, &next_token)?;
        }

        event!(Level::DEBUG, "Completed parsing of expression: {:?}", left);

        Ok(left)
    }

    fn parse_prefix_expression(
        parser: &mut Parser,
        token: &Token,
    ) -> Result<Expression, ParseError> {
        match token {
            Token::Ident(literal) => Ok(Expression::IdentifierLiteral(Identifier(
                literal.to_string(),
            ))),
            Token::Int(integer_literal) => match integer_literal.parse::<i32>() {
                Ok(parsed_number) => Ok(Expression::IntegerLiteral(parsed_number)),
                Err(error) => Err(ParseError::ParseIntegerError(token.clone(), error)),
            },
            Token::Bang => Self::create_prefix_expression(parser, Operator::Bang),
            Token::Minus => Self::create_prefix_expression(parser, Operator::Minus),
            Token::LParen => Self::create_grouped_expression(parser),
            Token::If => Self::parse_if_expression(parser),
            Token::Func => Self::parse_function_literal(parser),
            Token::True => Ok(Expression::BooleanLiteral(true)),
            Token::False => Ok(Expression::BooleanLiteral(false)),
            unexpected_token => Err(ParseError::NoPrefixExpression(unexpected_token.clone())),
        }
    }

    fn parse_function_literal(parser: &mut Parser) -> Result<Expression, ParseError> {
        let function_span = span!(Level::DEBUG, "Function");
        let _enter = function_span.enter();

        event!(Level::DEBUG, "Parsing function");
        let parameters: Vec<Identifier> = Self::parse_function_parameters(parser)?;
        event!(Level::DEBUG, "Found parameters {parameters:?}");

        parser.tokens.expect_token(Token::Assign)?;

        let body: BlockStatement = Self::parse_blockstatement(parser)?;
        parser.tokens.expect_token(Token::Lasagna)?;

        Ok(Expression::FunctionLiteral { parameters, body })
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
                    Some(_) => parameters.push(Self::parse_literal(parser)?),
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

    fn parse_literal(parser: &mut Parser) -> Result<Identifier, ParseError> {
        match parser.tokens.consume() {
            Some(Token::Ident(literal)) => Ok(Identifier(literal)),
            Some(unexpected_token) => Err(ParseError::UnexpectedToken {
                expected_token: TokenExpectation::SingleExpectation(Token::Ident("".to_string())),
                found_token: Some(unexpected_token),
            }),
            None => Err(ParseError::ExpectedToken),
        }
    }

    fn create_prefix_expression(
        parser: &mut Parser,
        operator: Operator,
    ) -> Result<Expression, ParseError> {
        let token = match parser.tokens.consume() {
            Some(token) => Ok(token),
            None => Err(ParseError::NoPrefixPartner),
        }?;

        let right = Self::parse(parser, token, Precedence::Prefix)?;
        Ok(Expression::PrefixExpression {
            right: Box::new(right),
            operator,
        })
    }

    fn create_grouped_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
        let next_token = parser.tokens.expect()?;
        let grouped_expression = Self::parse(parser, next_token, Precedence::Lowest);
        parser.tokens.expect_token(Token::RParen)?;
        grouped_expression
    }

    fn parse_if_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
        let next_token = parser.tokens.expect()?;
        let condition = Self::parse(parser, next_token, Precedence::Lowest)?;

        parser.tokens.expect_token(Token::Assign)?;

        let consequence = Self::parse_blockstatement(parser)?;

        let alternative = match parser.tokens.consume() {
            Some(Token::Lasagna) => Ok(None),
            Some(Token::Else) => {
                parser.tokens.expect_token(Token::Assign)?;
                let else_block = Some(Self::parse_blockstatement(parser)?);
                parser.tokens.expect_token(Token::Lasagna)?;
                Ok(else_block)
            }
            Some(unexpected_token) => Err(ParseError::UnexpectedToken {
                expected_token: TokenExpectation::MultipleExpectation(
                    [Token::Lasagna, Token::Else].to_vec(),
                ),
                found_token: Some(unexpected_token.clone()),
            }),
            None => Err(ParseError::ExpectedToken),
        }?;

        Ok(Expression::IfExpression {
            condition: Box::from(condition),
            consequence,
            alternative,
        })
    }

    fn parse_blockstatement(parser: &mut Parser) -> Result<BlockStatement, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();
        while !parser.tokens.next_token_is(&Token::Lasagna)
            && !parser.tokens.next_token_is(&Token::Else)
        {
            statements.push(parser.parse_statement()?);
        }

        Ok(BlockStatement { statements })
    }

    fn parse_infix_expression(
        parser: &mut Parser,
        left: Expression,
        token: &Token,
    ) -> Result<Expression, ParseError> {
        event!(
            Level::DEBUG,
            "Parsing infix expression for token {:?}",
            token
        );
        match token.has_infix() {
            HasInfix::Arithmic(operator) => {
                let precedence = token.get_precedence();
                let next_token = parser.tokens.expect()?;
                let right = Self::parse(parser, next_token, precedence)?;

                Ok(Expression::InfixExpression {
                    left: Box::from(left),
                    right: Box::from(right),
                    operator,
                })
            }
            HasInfix::Call() => {
                let call_span = span!(Level::DEBUG, "Call");
                let _enter = call_span.enter();

                Ok(Expression::CallExpression {
                    function: Box::from(left),
                    arguments: Self::parse_function_arguments(parser)?,
                })
            }
            HasInfix::No(token) => Err(ParseError::NoInfixExpression(token.clone())),
        }
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

                    parameters.push(Self::parse(parser, current_token, Precedence::Lowest)?);
                }
            }
        }

        Ok(parameters)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{BlockStatement, Identifier, Operator, Program, Statement},
        expressions::{expression::Expression, expression_statement::ExpressionStatement},
        test_util::{
            create_function_expression, create_identifierliteral, create_if_condition,
            create_infix_expression, create_infix_test_case, create_prefix_test_case,
            has_parser_errors, parse_program, setup_logger,
        },
    };

    #[test]
    fn test_integer_expression() {
        let input: &str = "5.";

        let program: Program = parse_program(input);
        has_parser_errors(&program);

        let parsed_statement = program
            .statements
            .first()
            .expect("Should only have one statement");

        assert!(matches!(
            parsed_statement,
            Statement::Expression(ExpressionStatement {
                expression: Expression::IntegerLiteral(5)
            })
        ));
    }

    #[test]
    fn test_identifier_expression() {
        let input: &str = "foobar.";

        let program: Program = parse_program(input);
        has_parser_errors(&program);

        assert_eq!(
            1,
            program.statements.len(),
            "Should only have parsed one expression statement"
        );

        let parsed_statement = program.statements.first().expect("Already checked length");

        assert!(matches!(
            parsed_statement,
            Statement::Expression(ExpressionStatement { expression: Expression::IdentifierLiteral(
                    Identifier(ident)
                        ) }) if ident == "foobar"
        ));
    }

    #[test]
    fn test_boolean_expression() {
        let input: &str = "true.false.";

        let program: Program = parse_program(input);
        has_parser_errors(&program);

        assert_eq!(
            2,
            program.statements.len(),
            "Should only have parsed one expression statement"
        );

        let parsed_statement = program.statements.first().expect("Already checked length");

        assert!(matches!(
            parsed_statement,
            Statement::Expression(ExpressionStatement {
                expression: Expression::BooleanLiteral(true)
            })
        ));
        assert!(matches!(
            program.statements.get(1).unwrap(),
            Statement::Expression(ExpressionStatement {
                expression: Expression::BooleanLiteral(false)
            })
        ));
    }

    #[test]
    fn test_parse_prefix() {
        struct TestCase {
            input: String,
            statement: Statement,
        }

        let test_cases: [TestCase; 2] = [
            (
                "!5.",
                create_prefix_test_case(Expression::IntegerLiteral(5), Operator::Bang),
            ),
            (
                "-15.",
                create_prefix_test_case(Expression::IntegerLiteral(15), Operator::Minus),
            ),
        ]
        .map(|(input, statement)| TestCase {
            input: input.to_string(),
            statement,
        });

        for test_case in test_cases {
            let program: Program = parse_program(&test_case.input);
            has_parser_errors(&program);

            assert_eq!(program.statements.len(), 1, "Should only parse 1 statement");
            let statement = program.statements.first().expect("Should be one statement");

            assert_eq!(
                statement, &test_case.statement,
                "Parsed statement should match testcase"
            );
        }
    }

    #[test]
    fn test_parse_infix() {
        struct TestCase {
            input: String,
            statement: Statement,
        }

        use Expression::*;
        use Operator::*;

        let test_cases: [TestCase; 11] = [
            (
                "5 + 5",
                create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Plus),
            ),
            (
                "5 - 5",
                create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Minus),
            ),
            (
                "5 * 5",
                create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Multiply),
            ),
            (
                "5 / 5",
                create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), DividedBy),
            ),
            (
                "5 > 5",
                create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), GreaterThan),
            ),
            (
                "5 < 5",
                create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), LessThan),
            ),
            (
                "5 == 5",
                create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Equals),
            ),
            (
                "5 != 5",
                create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), NotEquals),
            ),
            (
                "true == true",
                create_infix_test_case(BooleanLiteral(true), BooleanLiteral(true), Equals),
            ),
            (
                "true != false",
                create_infix_test_case(BooleanLiteral(true), BooleanLiteral(false), NotEquals),
            ),
            (
                "false == false",
                create_infix_test_case(BooleanLiteral(false), BooleanLiteral(false), Equals),
            ),
        ]
        .map(|(input, statement)| TestCase {
            input: input.to_string(),
            statement,
        });

        for test_case in test_cases {
            let program: Program = parse_program(&test_case.input);
            has_parser_errors(&program);

            let statement = program.statements.first().expect("Should be one statement");

            assert_eq!(
                statement, &test_case.statement,
                "Parsed statement should match testcase"
            );
            assert_eq!(program.statements.len(), 1, "Should only parse 1 statement");
        }
    }

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
        let expected_statement = Expression::CallExpression {
            function: Box::from(create_identifierliteral("add")),
            arguments: expected_arguments,
        };
        assert_eq!(
            statement,
            &Statement::Expression(ExpressionStatement {
                expression: expected_statement
            }),
            "Parsed statement should match testcase"
        );
        assert_eq!(program.statements.len(), 1, "Should only parse 1 statement");
    }
}
