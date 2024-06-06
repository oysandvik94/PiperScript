use tracing::{event, span, Level};

use crate::{
    ast::{BlockStatement, Expression, Identifier, Operator, Program, Statement},
    lexer::{
        lexedtokens::LexedTokens,
        token::{HasInfix, Precedence, Token},
    },
    parse_errors::{ParseError, TokenExpectation},
};

pub struct Parser {
    tokens: LexedTokens,
}

impl Parser {
    pub fn new(tokens: LexedTokens) -> Parser {
        Parser { tokens }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = Vec::new();
        let mut parse_errors: Vec<ParseError> = Vec::new();

        while self.tokens.peek().is_some() {
            let statement_span = span!(Level::DEBUG, "Statement");
            let _enter = statement_span.enter();

            match self.parse_statement() {
                Ok(parsed_statement) => {
                    event!(Level::DEBUG, "Parsed statement: {parsed_statement:?}",);
                    statements.push(parsed_statement)
                }
                Err(parse_error) => {
                    event!(Level::DEBUG, "Error parsing statement: {parse_error:?}");
                    self.tokens.iterate_to_next_statement();
                    parse_errors.push(parse_error)
                }
            };
        }

        Program {
            statements,
            parse_errors,
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.tokens.peek() {
            Some(Token::Return) => self.parse_return_statement(),
            Some(Token::Let) => self.parse_assign_statement(),
            Some(_) => self.parse_expression_statement(),
            None => Err(ParseError::ExpectedToken),
        }
    }

    fn parse_assign_statement(&mut self) -> Result<Statement, ParseError> {
        self.tokens.expect_token(Token::Let)?;
        let identifier = self.tokens.expected_identifier()?;
        self.tokens.expect_token(Token::Assign)?;

        let next_token = self.tokens.expect()?;
        let expression = self.parse_expression(next_token, Precedence::Lowest)?;

        self.tokens.expect_optional_token(Token::Period);

        Ok(Statement::AssignStatement(identifier, expression))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        self.tokens.expect_token(Token::Return)?;
        let next_token = self.tokens.expect()?;
        let expression = self.parse_expression(next_token, Precedence::Lowest)?;

        self.tokens.expect_optional_token(Token::Period);

        Ok(Statement::ReturnStatement(expression))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let first_token = self.tokens.expect()?;
        event!(
            Level::DEBUG,
            "Parsing expression statement with starting token {first_token:?}"
        );

        let expression_statement_span = span!(Level::DEBUG, "Expression");
        let _enter = expression_statement_span.enter();

        let expression = self.parse_expression(first_token, Precedence::Lowest)?;

        self.tokens.expect_optional_token(Token::Period);

        Ok(Statement::ExpressionStatement(expression))
    }

    fn parse_expression(
        &mut self,
        current_token: Token,
        precedence: Precedence,
    ) -> Result<Expression, ParseError> {
        event!(
            Level::DEBUG,
            "Parsing expression starting with token {:?} and precedence {:?}",
            current_token,
            precedence
        );

        let mut left = self.parse_prefix_expression(&current_token)?;
        event!(Level::DEBUG, "Found prefix expression {:?}", left);

        while self.tokens.next_token_has_infix() && precedence < self.tokens.next_token_precedence()
        {
            let next_token = self.tokens.expect()?;
            left = self.parse_infix_expression(left, &next_token)?;
        }

        event!(Level::DEBUG, "Completed parsing of expression: {:?}", left);

        Ok(left)
    }

    fn parse_prefix_expression(&mut self, token: &Token) -> Result<Expression, ParseError> {
        match token {
            Token::Ident(literal) => Ok(Expression::IdentifierLiteral(Identifier(
                literal.to_string(),
            ))),
            Token::Int(integer_literal) => match integer_literal.parse::<i32>() {
                Ok(parsed_number) => Ok(Expression::IntegerLiteral(parsed_number)),
                Err(error) => Err(ParseError::ParseIntegerError(token.clone(), error)),
            },
            Token::Bang => self.create_prefix_expression(Operator::Bang),
            Token::Minus => self.create_prefix_expression(Operator::Minus),
            Token::LParen => self.create_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Func => self.parse_function_literal(),
            Token::True => Ok(Expression::BooleanLiteral(true)),
            Token::False => Ok(Expression::BooleanLiteral(false)),
            unexpected_token => Err(ParseError::NoPrefixExpression(unexpected_token.clone())),
        }
    }

    fn parse_infix_expression(
        &mut self,
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
                let next_token = self.tokens.expect()?;
                let right = self.parse_expression(next_token, precedence)?;

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
                    arguments: self.parse_function_arguments()?,
                })
            }
            HasInfix::No(token) => Err(ParseError::NoInfixExpression(token.clone())),
        }
    }

    fn create_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        let next_token = self.tokens.expect()?;
        let grouped_expression = self.parse_expression(next_token, Precedence::Lowest);
        self.tokens.expect_token(Token::RParen)?;
        grouped_expression
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParseError> {
        let function_span = span!(Level::DEBUG, "Function");
        let _enter = function_span.enter();

        event!(Level::DEBUG, "Parsing function");
        let parameters: Vec<Identifier> = self.parse_function_parameters()?;
        event!(Level::DEBUG, "Found parameters {parameters:?}");

        self.tokens.expect_token(Token::Assign)?;

        let body: BlockStatement = self.parse_blockstatement()?;
        self.tokens.expect_token(Token::Lasagna)?;

        Ok(Expression::FunctionLiteral { parameters, body })
    }

    fn parse_function_arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
        event!(Level::DEBUG, "Parsing function arguments");
        let mut parameters: Vec<Expression> = Vec::from([]);
        while let Some(token) = self.tokens.peek() {
            match token {
                Token::RParen => {
                    self.tokens.consume();
                    return Ok(parameters);
                }
                Token::Comma => {
                    self.tokens.consume();
                }
                _ => {
                    let current_token = self
                        .tokens
                        .consume()
                        .expect("Expected a token after peeking");

                    parameters.push(self.parse_expression(current_token, Precedence::Lowest)?);
                }
            }
        }

        Ok(parameters)
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, ParseError> {
        let mut parameters: Vec<Identifier> = Vec::from([]);
        while let Some(token) = self.tokens.consume() {
            match token {
                Token::LParen | Token::Comma => match self.tokens.peek() {
                    Some(Token::RParen) => {
                        self.tokens.consume();
                        return Ok(parameters);
                    }
                    Some(_) => parameters.push(self.parse_literal()?),
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

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        let next_token = self.tokens.expect()?;
        let condition = self.parse_expression(next_token, Precedence::Lowest)?;

        self.tokens.expect_token(Token::Assign)?;

        let consequence = self.parse_blockstatement()?;

        let alternative = match self.tokens.consume() {
            Some(Token::Lasagna) => Ok(None),
            Some(Token::Else) => {
                self.tokens.expect_token(Token::Assign)?;
                let else_block = Some(self.parse_blockstatement()?);
                self.tokens.expect_token(Token::Lasagna)?;
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

    fn parse_blockstatement(&mut self) -> Result<BlockStatement, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();
        while !self.tokens.next_token_is(&Token::Lasagna)
            && !self.tokens.next_token_is(&Token::Else)
        {
            statements.push(self.parse_statement()?);
        }

        Ok(BlockStatement { statements })
    }

    fn create_prefix_expression(&mut self, operator: Operator) -> Result<Expression, ParseError> {
        let token = match self.tokens.consume() {
            Some(token) => Ok(token),
            None => Err(ParseError::NoPrefixPartner),
        }?;

        let right = self.parse_expression(token, Precedence::Prefix)?;
        Ok(Expression::PrefixExpression {
            right: Box::new(right),
            operator,
        })
    }

    fn parse_literal(&mut self) -> Result<Identifier, ParseError> {
        match self.tokens.consume() {
            Some(Token::Ident(literal)) => Ok(Identifier(literal)),
            Some(unexpected_token) => Err(ParseError::UnexpectedToken {
                expected_token: TokenExpectation::SingleExpectation(Token::Ident("".to_string())),
                found_token: Some(unexpected_token),
            }),
            None => Err(ParseError::ExpectedToken),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        ast::{BlockStatement, Expression, Identifier, Operator, Program, Statement},
        test_util::{
            create_function_expression, create_identifierliteral, create_if_condition,
            create_infix_expression, create_infix_test_case, create_prefix_test_case,
            has_parser_errors, parse_program, setup_logger,
        },
    };

    #[test]
    fn parse_assign_statement() {
        let source_code = "
            let x: 5.
            let y: 10.
            let foobar: 54456.
        ";

        let program: Program = parse_program(source_code);

        has_parser_errors(&program);
        assert_eq!(
            program.statements.len(),
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
                test_let_statement(&program.statements[idx], ident, &expected_expression[idx])
            });
    }

    #[test]
    fn parse_return_statement() {
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
            &Statement::ReturnStatement(Expression::IntegerLiteral(5))
        );
        let second_statement = program.statements.get(1).expect("Should get statement");
        assert_eq!(
            second_statement,
            &Statement::ReturnStatement(create_identifierliteral("foobar"))
        );
    }

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
            Statement::ExpressionStatement(Expression::IntegerLiteral(5))
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
            Statement::ExpressionStatement(Expression::IdentifierLiteral(
                Identifier(ident)
            )) if ident == "foobar"
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
            Statement::ExpressionStatement(Expression::BooleanLiteral(true))
        ));
        assert!(matches!(
            program.statements.get(1).unwrap(),
            Statement::ExpressionStatement(Expression::BooleanLiteral(false))
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
                        statements: Vec::from([Statement::ExpressionStatement(
                            create_identifierliteral("x"),
                        )]),
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
                        statements: Vec::from([Statement::ExpressionStatement(
                            create_identifierliteral("x"),
                        )]),
                    },
                    Some(BlockStatement {
                        statements: Vec::from([Statement::ExpressionStatement(
                            create_identifierliteral("y"),
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
                        statements: Vec::from([Statement::ExpressionStatement(
                            create_infix_expression(
                                create_identifierliteral("x"),
                                create_identifierliteral("y"),
                                Operator::Plus,
                            ),
                        )]),
                    },
                ),
            ),
            (
                "fn(): x.y.~",
                create_function_expression(
                    Vec::from([]),
                    BlockStatement {
                        statements: Vec::from([
                            Statement::ExpressionStatement(create_identifierliteral("x")),
                            Statement::ExpressionStatement(create_identifierliteral("y")),
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
            &Statement::ExpressionStatement(expected_statement),
            "Parsed statement should match testcase"
        );
        assert_eq!(program.statements.len(), 1, "Should only parse 1 statement");
    }

    #[test]
    fn test_operator_precedence() {
        struct TestCase {
            input: String,
            expected: String,
        }
        let test_cases: [TestCase; 24] = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4. -5 * 5.", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == -3 < 4", "((5 > 4) == ((-3) < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ]
        .map(|(input, expected)| TestCase {
            input: input.to_string(),
            expected: expected.to_string(),
        });

        for testcase in test_cases {
            let actual = parse_program(&testcase.input);

            if has_parser_errors(&actual) {
                let expected = testcase.expected;
                println!("{expected}");
                panic!("Found parser errors");
            }

            assert_eq!(actual.to_string().replace('\n', ""), testcase.expected);
        }
    }

    fn test_let_statement(
        found: &Statement,
        expected_identifier: &Identifier,
        expected_expression: &Expression,
    ) {
        match found {
            Statement::AssignStatement(found_identfier, found_expression) => {
                assert_eq!(found_identfier, expected_identifier);
                assert_eq!(found_expression, expected_expression);
            }
            incorrect => panic!("Expected let-statement, but got {incorrect:?}"),
        };
    }
}
