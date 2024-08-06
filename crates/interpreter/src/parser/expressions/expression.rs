use tracing::{event, Level};

use crate::parser::{
    ast::{BlockStatement, Identifier, Operator, PrefixOperator, Statement},
    lexer::token::{HasInfix, Precedence, Token, TokenKind},
    parse_errors::{ParseError, ParseErrorKind},
    Parser, StatementError,
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
    pub fn parse(parser: &mut Parser, precedence: Precedence) -> Result<Expression, ParseError> {
        let mut left = Self::parse_prefix_expression(parser)?;
        event!(Level::DEBUG, "Found prefix expression {:?}", left);

        while parser.lexer.next_token_has_infix()
            && precedence < parser.lexer.next_token_precedence()
        {
            let next_token = parser.lexer.expect()?;
            left = Self::parse_infix_expression(parser, left, &next_token)?;
        }

        event!(Level::DEBUG, "Completed parsing of expression: {:?}", left);

        Ok(left)
    }

    fn parse_prefix_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
        let token: &Token = parser.lexer.expect_peek()?;
        if !token.token_kind.valid_prefix() {
            return Err(ParseError::new(
                token.clone(),
                ParseErrorKind::NoPrefixExpression,
            ));
        }

        let token = parser.lexer.expect()?;
        match &token.token_kind {
            TokenKind::Ident(literal) => Ok(Expression::IdentifierLiteral(Identifier(
                literal.to_string(),
            ))),
            TokenKind::Int(integer_literal) => match integer_literal.parse::<i32>() {
                Ok(parsed_number) => Ok(Expression::IntegerLiteral(parsed_number)),
                Err(error) => Err(ParseError::new(
                    token.clone(),
                    ParseErrorKind::ParseIntegerError(error),
                )),
            },
            TokenKind::Str(string_literal) => Ok(Expression::StringLiteral(string_literal.clone())),
            TokenKind::Bang => Self::create_prefix_expression(parser, PrefixOperator::Bang),
            TokenKind::Minus => Self::create_prefix_expression(parser, PrefixOperator::Minus),
            TokenKind::LParen => Self::create_grouped_expression(parser),
            TokenKind::If => IfExpression::parse_if_expression(parser),
            TokenKind::Func => FunctionLiteral::parse(parser),
            TokenKind::True => Ok(Expression::BooleanLiteral(true)),
            TokenKind::False => Ok(Expression::BooleanLiteral(false)),
            TokenKind::LBracket => ArrayLiteral::parse(parser),
            TokenKind::LBrace => Ok(Expression::HashLiteral(Self::parse_hash_literal(parser)?)),
            _ => Err(ParseError::new(
                token.clone(),
                ParseErrorKind::NoPrefixExpression,
            )),
        }
    }

    pub fn parse_literal(parser: &mut Parser) -> Result<Identifier, ParseError> {
        let token = parser.lexer.expect()?;
        match token.token_kind {
            TokenKind::Ident(literal) => Ok(Identifier(literal)),
            _ => Err(ParseError::new(
                token.clone(),
                ParseErrorKind::UnexpectedToken(TokenKind::Ident("".to_owned())),
            )),
        }
    }

    fn create_prefix_expression(
        parser: &mut Parser,
        operator: PrefixOperator,
    ) -> Result<Expression, ParseError> {
        let right = Self::parse(parser, Precedence::Prefix)?;
        Ok(Expression::Prefix {
            right: Box::new(right),
            operator,
        })
    }

    fn create_grouped_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
        let grouped_expression = Self::parse(parser, Precedence::Lowest);
        parser.lexer.expect_token(TokenKind::RParen)?;
        grouped_expression
    }

    pub fn parse_blockstatement(parser: &mut Parser) -> Result<BlockStatement, StatementError> {
        let mut statements: Vec<Statement> = Vec::new();
        while !parser.lexer.next_token_is(&TokenKind::Lasagna)
            && !parser.lexer.next_token_is(&TokenKind::Else)
            && parser.lexer.has_next()
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
        match token.token_kind.has_infix() {
            HasInfix::Arithmic(operator) => {
                let precedence = token.token_kind.get_precedence();
                let right = Self::parse(parser, precedence)?;

                Ok(Expression::Infix {
                    left: Box::from(left),
                    right: Box::from(right),
                    operator,
                })
            }
            HasInfix::Call() => CallExpression::parse(parser, left),
            HasInfix::Index() => Expression::parse_index_expression(parser, left),
            HasInfix::No(_) => Err(ParseError::new(
                token.clone(),
                ParseErrorKind::NoInfixExpression,
            )),
        }
    }

    pub fn parse_expression_list(
        parser: &mut Parser,
        end: &TokenKind,
    ) -> Result<Vec<Expression>, ParseError> {
        let mut parameters: Vec<Expression> = Vec::from([]);
        while let Some(token) = parser.lexer.peek() {
            match &token.token_kind {
                token if token == end => {
                    parser.lexer.consume();
                    return Ok(parameters);
                }
                TokenKind::Comma => {
                    parser.lexer.consume();
                }
                _ => {
                    parameters.push(Expression::parse(parser, Precedence::Lowest)?);
                }
            }
        }

        Ok(parameters)
    }

    fn parse_index_expression(
        parser: &mut Parser,
        left: Expression,
    ) -> Result<Expression, ParseError> {
        let index = Expression::parse(parser, Precedence::Lowest)?;

        parser.lexer.expect_token(TokenKind::RBracket)?;

        Ok(Expression::Index {
            left: Box::new(left),
            index: Box::new(index),
        })
    }

    pub fn parse_hash_literal(
        parser: &mut Parser,
    ) -> Result<Vec<(Expression, Expression)>, ParseError> {
        let mut keypairs: Vec<(Expression, Expression)> = Vec::new();
        while let Some(token) = parser.lexer.peek() {
            match &token.token_kind {
                TokenKind::RBrace => {
                    parser.lexer.consume();
                    return Ok(keypairs);
                }
                TokenKind::Comma => {
                    parser.lexer.consume();
                }
                _ => {
                    let key = Expression::parse(parser, Precedence::Lowest)?;
                    event!(Level::DEBUG, "Parsed key expression {:?}", key);

                    parser.lexer.expect_token(TokenKind::Colon)?;
                    let value = Expression::parse(parser, Precedence::Lowest)?;
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
