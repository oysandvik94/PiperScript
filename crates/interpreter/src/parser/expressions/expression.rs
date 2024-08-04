use tracing::{event, Level};

use crate::parser::{
    ast::{BlockStatement, Identifier, Operator, PrefixOperator, Statement},
    lexer::token::{HasInfix, Precedence, Token},
    parse_errors::{ParseError, TokenExpectation},
    Parser,
};

use super::{
    arrays::ArrayLiteral,
    functions::{CallExpression, FunctionLiteral},
    if_expression::IfExpression,
};

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    IdentifierLiteral(Identifier),
    StringLiteral(String),
    IntegerLiteral(i32),
    BooleanLiteral(bool),
    Array(ArrayLiteral),
    HashLiteral(Vec<(Expression, Expression)>),
    Index {
        left: Box<Expression>,
        index: Box<Expression>,
    },
    Prefix {
        right: Box<Expression>,
        operator: PrefixOperator,
    },
    Infix {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: Operator,
    },
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
}

impl Expression {
    pub fn parse(
        parser: &mut Parser,
        current_token: &Token,
        precedence: Precedence,
    ) -> Result<Expression, ParseError> {
        event!(
            Level::DEBUG,
            "Parsing expression starting with token {:?} and precedence {:?}",
            current_token,
            precedence
        );

        let mut left = Self::parse_prefix_expression(parser, current_token)?;
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
            Token::Str(string_literal) => Ok(Expression::StringLiteral(string_literal.clone())),
            Token::Bang => Self::create_prefix_expression(parser, PrefixOperator::Bang),
            Token::Minus => Self::create_prefix_expression(parser, PrefixOperator::Minus),
            Token::LParen => Self::create_grouped_expression(parser),
            Token::If => IfExpression::parse_if_expression(parser),
            Token::Func => FunctionLiteral::parse(parser),
            Token::True => Ok(Expression::BooleanLiteral(true)),
            Token::False => Ok(Expression::BooleanLiteral(false)),
            Token::LBracket => ArrayLiteral::parse(parser),
            Token::LBrace => Ok(Expression::HashLiteral(Self::parse_hash_literal(parser)?)),
            unexpected_token => Err(ParseError::NoPrefixExpression(unexpected_token.clone())),
        }
    }

    pub fn parse_literal(parser: &mut Parser) -> Result<Identifier, ParseError> {
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
        operator: PrefixOperator,
    ) -> Result<Expression, ParseError> {
        let token = match parser.tokens.consume() {
            Some(token) => Ok(token),
            None => Err(ParseError::NoPrefixPartner),
        }?;

        let right = Self::parse(parser, &token, Precedence::Prefix)?;
        Ok(Expression::Prefix {
            right: Box::new(right),
            operator,
        })
    }

    fn create_grouped_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
        let next_token = parser.tokens.expect()?;
        let grouped_expression = Self::parse(parser, &next_token, Precedence::Lowest);
        parser.tokens.expect_token(Token::RParen)?;
        grouped_expression
    }

    pub fn parse_blockstatement(parser: &mut Parser) -> Result<BlockStatement, ParseError> {
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
                let right = Self::parse(parser, &next_token, precedence)?;

                Ok(Expression::Infix {
                    left: Box::from(left),
                    right: Box::from(right),
                    operator,
                })
            }
            HasInfix::Call() => CallExpression::parse(parser, left),
            HasInfix::Index() => Expression::parse_index_expression(parser, left),
            HasInfix::No(token) => Err(ParseError::NoInfixExpression(token.clone())),
        }
    }

    pub fn parse_expression_list(
        parser: &mut Parser,
        end: &Token,
    ) -> Result<Vec<Expression>, ParseError> {
        let mut parameters: Vec<Expression> = Vec::from([]);
        while let Some(token) = parser.tokens.peek() {
            match token {
                token if token == end => {
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
                        &current_token,
                        Precedence::Lowest,
                    )?);
                }
            }
        }

        Ok(parameters)
    }

    fn parse_index_expression(
        parser: &mut Parser,
        left: Expression,
    ) -> Result<Expression, ParseError> {
        let token = parser.tokens.expect()?;

        let index = Expression::parse(parser, &token, Precedence::Lowest)?;

        parser.tokens.expect_token(Token::RBracket)?;

        Ok(Expression::Index {
            left: Box::new(left),
            index: Box::new(index),
        })
    }

    pub fn parse_hash_literal(
        parser: &mut Parser,
    ) -> Result<Vec<(Expression, Expression)>, ParseError> {
        let mut keypairs: Vec<(Expression, Expression)> = Vec::new();
        while let Some(token) = parser.tokens.peek() {
            match token {
                Token::RBrace => {
                    parser.tokens.consume();
                    return Ok(keypairs);
                }
                Token::Comma => {
                    parser.tokens.consume();
                }
                _ => {
                    let current_token = parser.tokens.expect()?;
                    let key = Expression::parse(parser, &current_token, Precedence::Lowest)?;
                    event!(Level::DEBUG, "Parsed key expression {:?}", key);

                    parser.tokens.expect_token(Token::Colon)?;
                    let current_token = parser.tokens.expect()?;
                    let value = Expression::parse(parser, &current_token, Precedence::Lowest)?;
                    event!(Level::DEBUG, "Parsed value expression {:?}", value);
                    keypairs.push((key, value));
                }
            }
        }

        Ok(keypairs)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            ast::{Identifier, Operator, PrefixOperator, Statement},
            expressions::expression::Expression,
        },
        test_util,
    };

    #[test]
    fn test_string_expression() {
        let input: &str = "\"hellow world\"";

        let statements = test_util::expect_parsed_program(input);

        match statements.first().expect("Should only have one statement") {
            Statement::Expression(expression) => match expression {
                Expression::StringLiteral(string) => assert_eq!(string, "hellow world"),
                _ => panic!("Should have parsed a string expression"),
            },
            _ => panic!("Should have parsed an expression statement"),
        }
    }

    #[test]
    fn test_integer_expression() {
        let input: &str = "5.";

        let statements = test_util::expect_parsed_program(input);

        let parsed_statement = statements.first().expect("Should only have one statement");

        assert!(matches!(
            parsed_statement,
            Statement::Expression(Expression::IntegerLiteral(5))
        ));
    }

    #[test]
    fn test_identifier_expression() {
        let input: &str = "foobar.";

        let statements = test_util::expect_parsed_program(input);

        assert_eq!(
            1,
            statements.len(),
            "Should only have parsed one expression statement"
        );

        let parsed_statement = statements.first().expect("Already checked length");

        assert!(matches!(
            parsed_statement,
            Statement::Expression(Expression::IdentifierLiteral(
                    Identifier(ident)
                        )) if ident == "foobar"
        ));
    }

    #[test]
    fn test_boolean_expression() {
        let input: &str = "true.false.";

        let statements = test_util::expect_parsed_program(input);

        assert_eq!(
            2,
            statements.len(),
            "Should only have parsed one expression statement"
        );

        let parsed_statement = statements.first().expect("Already checked length");

        assert!(matches!(
            parsed_statement,
            Statement::Expression(Expression::BooleanLiteral(true))
        ));
        assert!(matches!(
            statements.get(1).unwrap(),
            Statement::Expression(Expression::BooleanLiteral(false))
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
                test_util::create_prefix_test_case(
                    Expression::IntegerLiteral(5),
                    PrefixOperator::Bang,
                ),
            ),
            (
                "-15.",
                test_util::create_prefix_test_case(
                    Expression::IntegerLiteral(15),
                    PrefixOperator::Minus,
                ),
            ),
        ]
        .map(|(input, statement)| TestCase {
            input: input.to_string(),
            statement,
        });

        for test_case in test_cases {
            let statements = test_util::expect_parsed_program(&test_case.input);

            assert_eq!(statements.len(), 1, "Should only parse 1 statement");
            let statement = statements.first().expect("Should be one statement");

            assert_eq!(
                statement, &test_case.statement,
                "Parsed statement should match testcase"
            );
        }
    }

    #[test]
    fn test_parse_infix() {
        use Expression::*;
        use Operator::*;

        let test_cases: Vec<(String, Statement)> = vec![
            (
                "5 + 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Plus),
            ),
            (
                "5 - 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Minus),
            ),
            (
                "5 * 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Multiply),
            ),
            (
                "5 / 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), DividedBy),
            ),
            (
                "5 > 5".to_string(),
                test_util::create_infix_test_case(
                    IntegerLiteral(5),
                    IntegerLiteral(5),
                    GreaterThan,
                ),
            ),
            (
                "5 < 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), LessThan),
            ),
            (
                "5 == 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Equals),
            ),
            (
                "5 != 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), NotEquals),
            ),
            (
                "true == true".to_string(),
                test_util::create_infix_test_case(
                    BooleanLiteral(true),
                    BooleanLiteral(true),
                    Equals,
                ),
            ),
            (
                "true != false".to_string(),
                test_util::create_infix_test_case(
                    BooleanLiteral(true),
                    BooleanLiteral(false),
                    NotEquals,
                ),
            ),
            (
                "false == false".to_string(),
                test_util::create_infix_test_case(
                    BooleanLiteral(false),
                    BooleanLiteral(false),
                    Equals,
                ),
            ),
        ];

        let asserter = |expected: &Statement, input: &String| {
            let statements = test_util::expect_parsed_program(input);

            let actual_statement = statements.first().expect("Should be one statement");

            assert_eq!(actual_statement, expected);
        };

        test_util::assert_list(test_cases, asserter);
    }

    #[test]
    fn can_parse_hash_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

        let statements = test_util::expect_parsed_program(input);
        let actual_statement = statements.first().expect("Should be one statement");

        match actual_statement {
            Statement::Expression(Expression::HashLiteral(pairs)) => {
                assert_eq!(pairs.len(), 3);

                let expected_pairs = vec![
                    (
                        Expression::StringLiteral("one".to_string()),
                        test_util::create_integer_infix_expression(0, 1, Operator::Plus),
                    ),
                    (
                        Expression::StringLiteral("two".to_string()),
                        test_util::create_integer_infix_expression(10, 8, Operator::Minus),
                    ),
                    (
                        Expression::StringLiteral("three".to_string()),
                        test_util::create_integer_infix_expression(15, 5, Operator::DividedBy),
                    ),
                ];

                for (actual, expected) in pairs.iter().zip(expected_pairs.iter()) {
                    assert_eq!(actual, expected);
                }
            }
            _ => panic!("Should have parsed a hash literal"),
        }
    }
    #[test]
    fn can_parse_hash_string_keys() {
        test_util::setup_logger();

        let input = r#"{"one": 1, "two": 2, "three": 3}"#;

        let statements = test_util::expect_parsed_program(input);
        let actual_statement = statements.first().expect("Should be one statement");

        match actual_statement {
            Statement::Expression(Expression::HashLiteral(pairs)) => {
                assert_eq!(pairs.len(), 3);

                let expected_pairs = vec![
                    (
                        Expression::StringLiteral("one".to_string()),
                        Expression::IntegerLiteral(1),
                    ),
                    (
                        Expression::StringLiteral("two".to_string()),
                        Expression::IntegerLiteral(2),
                    ),
                    (
                        Expression::StringLiteral("three".to_string()),
                        Expression::IntegerLiteral(3),
                    ),
                ];

                for (actual, expected) in pairs.iter().zip(expected_pairs.iter()) {
                    assert_eq!(actual, expected);
                }
            }
            _ => panic!("Should have parsed a hash literal"),
        }
    }

    #[test]
    fn can_parse_empty_hash() {
        let input = r#"{}"#;

        let statements = test_util::expect_parsed_program(input);
        let actual_statement = statements.first().expect("Should be one statement");

        match actual_statement {
            Statement::Expression(Expression::HashLiteral(pairs)) => {
                assert_eq!(pairs.len(), 0);
            }
            _ => panic!("Should have parsed a hash literal"),
        }
    }
}
