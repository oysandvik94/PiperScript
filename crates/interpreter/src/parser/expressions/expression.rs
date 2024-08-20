use tracing::{event, Level};

use crate::parser::{
    ast::{BlockStatement, Identifier, Operator, PrefixOperator, StatementType},
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
    pub fn parse(
        parser: &mut Parser,
        precedence: Precedence,
    ) -> Result<(Expression, Vec<Token>), ParseError> {
        let (mut left, mut tokens) = Self::parse_prefix_expression(parser)?;
        event!(Level::DEBUG, "Found prefix expression {:?}", left);

        while parser.lexer.next_token_has_infix()
            && precedence < parser.lexer.next_token_precedence()
        {
            let next_token = parser.lexer.expect()?;

            let (new_left, mut infix_tokens) =
                Self::parse_infix_expression(parser, left, &next_token)?;
            left = new_left;
            tokens.append(&mut infix_tokens);

            tokens.push(next_token);
        }

        event!(Level::DEBUG, "Completed parsing of expression: {:?}", left);

        Ok((left, tokens))
    }

    fn parse_prefix_expression(
        parser: &mut Parser,
    ) -> Result<(Expression, Vec<Token>), ParseError> {
        let mut tokens: Vec<Token> = Vec::new();
        let token: &Token = parser.lexer.expect_peek()?;

        if !token.token_kind.valid_prefix() {
            return Err(ParseError::new(
                token.clone(),
                ParseErrorKind::NoPrefixExpression,
            ));
        }

        let token = parser.lexer.expect()?;
        let (expression, mut tokens) = match &token.token_kind {
            TokenKind::Ident(literal) => (
                Expression::IdentifierLiteral(Identifier(literal.to_string())),
                vec![],
            ),
            TokenKind::Int(integer_literal) => match integer_literal.parse::<i32>() {
                Ok(parsed_number) => (Expression::IntegerLiteral(parsed_number), vec![]),
                Err(error) => {
                    return Err(ParseError::new(
                        token.clone(),
                        ParseErrorKind::ParseIntegerError(error),
                    ))
                }
            },
            TokenKind::Str(string_literal) => {
                (Expression::StringLiteral(string_literal.clone()), vec![])
            }
            TokenKind::Bang => {
                let (expression, mut parsed_tokens) =
                    Self::create_prefix_expression(parser, PrefixOperator::Bang)?;
                tokens.append(&mut parsed_tokens);

                (expression, vec![])
            }
            TokenKind::Minus => {
                let (expression, mut parsed_tokens) =
                    Self::create_prefix_expression(parser, PrefixOperator::Minus)?;
                tokens.append(&mut parsed_tokens);

                (expression, vec![])
            }
            TokenKind::LParen => {
                let (expression, mut parsed_tokens) = Self::create_grouped_expression(parser)?;
                tokens.append(&mut parsed_tokens);

                (expression, vec![])
            }
            TokenKind::If => IfExpression::parse_if_expression(parser)?,
            TokenKind::Func => FunctionLiteral::parse(parser)?,
            TokenKind::True => (Expression::BooleanLiteral(true), vec![]),
            TokenKind::False => (Expression::BooleanLiteral(false), vec![]),
            TokenKind::LBracket => ArrayLiteral::parse(parser)?,
            TokenKind::LBrace => {
                let (expr, tokens) = Self::parse_hash_literal(parser)?;
                (Expression::HashLiteral(expr), tokens)
            }
            _ => {
                return Err(ParseError::new(
                    token.clone(),
                    ParseErrorKind::NoPrefixExpression,
                ))
            }
        };

        tokens.insert(0, token);

        Ok((expression, tokens))
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
    ) -> Result<(Expression, Vec<Token>), ParseError> {
        let (right, tokens) = Self::parse(parser, Precedence::Prefix)?;

        let prefix_expression = Expression::Prefix {
            right: Box::new(right),
            operator,
        };

        Ok((prefix_expression, tokens))
    }

    fn create_grouped_expression(
        parser: &mut Parser,
    ) -> Result<(Expression, Vec<Token>), ParseError> {
        let (expression, mut tokens) = Self::parse(parser, Precedence::Lowest)?;
        tokens.push(parser.lexer.expect_token(TokenKind::RParen)?);

        Ok((expression, tokens))
    }

    pub fn parse_blockstatement(
        parser: &mut Parser,
    ) -> Result<(BlockStatement, Vec<Token>), StatementError> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut statements: Vec<StatementType> = Vec::new();
        while !parser.lexer.next_token_is(&TokenKind::Lasagna)
            && !parser.lexer.next_token_is(&TokenKind::Else)
            && parser.lexer.has_next()
        {
            let (new_statement, mut parsed_tokens) = parser.parse_statement_type()?;
            tokens.append(&mut parsed_tokens);
            statements.push(new_statement);
        }

        let block_statement = BlockStatement { statements };
        Ok((block_statement, tokens))
    }

    fn parse_infix_expression(
        parser: &mut Parser,
        left: Expression,
        token: &Token,
    ) -> Result<(Expression, Vec<Token>), ParseError> {
        event!(
            Level::DEBUG,
            "Parsing infix expression for token {:?}",
            token
        );
        match token.token_kind.has_infix() {
            HasInfix::Arithmic(operator) => {
                let precedence = token.token_kind.get_precedence();
                let (right, tokens) = Self::parse(parser, precedence)?;

                let infix_expr = Expression::Infix {
                    left: Box::from(left),
                    right: Box::from(right),
                    operator,
                };
                Ok((infix_expr, tokens))
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
    ) -> Result<(Vec<Expression>, Vec<Token>), ParseError> {
        let mut parameters: Vec<Expression> = Vec::from([]);
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(token) = parser.lexer.peek() {
            match &token.token_kind {
                token if token == end => {
                    if let Some(token) = parser.lexer.consume() {
                        tokens.push(token);
                    }

                    return Ok((parameters, tokens));
                }
                TokenKind::Comma => {
                    if let Some(token) = parser.lexer.consume() {
                        tokens.push(token);
                    }
                }
                _ => {
                    let (expression, mut parsed_tokens) =
                        Expression::parse(parser, Precedence::Lowest)?;
                    tokens.append(&mut parsed_tokens);

                    parameters.push(expression);
                }
            }
        }

        Ok((parameters, tokens))
    }

    fn parse_index_expression(
        parser: &mut Parser,
        left: Expression,
    ) -> Result<(Expression, Vec<Token>), ParseError> {
        let (index, mut tokens) = Expression::parse(parser, Precedence::Lowest)?;

        let token = parser.lexer.expect_token(TokenKind::RBracket)?;
        tokens.push(token);

        let index_expr = Expression::Index {
            left: Box::new(left),
            index: Box::new(index),
        };

        Ok((index_expr, tokens))
    }

    pub fn parse_hash_literal(
        parser: &mut Parser,
    ) -> Result<(Vec<(Expression, Expression)>, Vec<Token>), ParseError> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut keypairs: Vec<(Expression, Expression)> = Vec::new();

        while let Some(token) = parser.lexer.peek() {
            match &token.token_kind {
                TokenKind::RBrace => {
                    tokens.push(parser.lexer.expect()?);
                    return Ok((keypairs, tokens));
                }
                TokenKind::Comma => {
                    tokens.push(parser.lexer.expect()?);
                }
                _ => {
                    let (key, mut parsed_tokens) = Expression::parse(parser, Precedence::Lowest)?;
                    tokens.append(&mut parsed_tokens);
                    event!(Level::DEBUG, "Parsed key expression {:?}", key);

                    tokens.push(parser.lexer.expect_token(TokenKind::Colon)?);

                    let (value, mut parsed_tokens) = Expression::parse(parser, Precedence::Lowest)?;
                    tokens.append(&mut parsed_tokens);
                    event!(Level::DEBUG, "Parsed value expression {:?}", value);
                    keypairs.push((key, value));
                }
            }
        }

        Ok((keypairs, tokens))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            ast::{Identifier, Operator, PrefixOperator, StatementType},
            expressions::expression::Expression,
            lexer::token::{Token, TokenKind},
        },
        test_util,
    };

    #[test]
    fn test_string_expression() {
        let input: &str = "\"hellow world\"";

        let statements = test_util::expect_parsed_program(input);

        match &statements
            .first()
            .expect("Should only have one statement")
            .statement_type
        {
            StatementType::Expression(expression) => match expression {
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
            &parsed_statement.statement_type,
            StatementType::Expression(Expression::IntegerLiteral(5))
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
            &parsed_statement.statement_type,
            StatementType::Expression(Expression::IdentifierLiteral(
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
            &parsed_statement.statement_type,
            StatementType::Expression(Expression::BooleanLiteral(true))
        ));
        assert!(matches!(
            &statements.get(1).unwrap().statement_type,
            StatementType::Expression(Expression::BooleanLiteral(false))
        ));
    }

    #[test]
    fn test_parse_prefix() {
        struct TestCase {
            input: String,
            statement: StatementType,
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
                &statement.statement_type, &test_case.statement,
                "Parsed statement should match testcase"
            );
        }
    }

    #[test]
    fn test_parse_infix() {
        use Expression::*;
        use Operator::*;

        let test_cases: Vec<(String, StatementType, Vec<TokenKind>)> = vec![
            (
                "5 + 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Plus),
                vec![
                    TokenKind::Int("5".to_owned()),
                    TokenKind::Add,
                    TokenKind::Int("5".to_owned()),
                ],
            ),
            (
                "5 - 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Minus),
                vec![
                    TokenKind::Int("5".to_owned()),
                    TokenKind::Minus,
                    TokenKind::Int("5".to_owned()),
                ],
            ),
            (
                "5 * 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Multiply),
                vec![
                    TokenKind::Int("5".to_owned()),
                    TokenKind::Asterix,
                    TokenKind::Int("5".to_owned()),
                ],
            ),
            (
                "5 / 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), DividedBy),
                vec![
                    TokenKind::Int("5".to_owned()),
                    TokenKind::Slash,
                    TokenKind::Int("5".to_owned()),
                ],
            ),
            (
                "5 > 5".to_string(),
                test_util::create_infix_test_case(
                    IntegerLiteral(5),
                    IntegerLiteral(5),
                    GreaterThan,
                ),
                vec![
                    TokenKind::Int("5".to_owned()),
                    TokenKind::GreaterThan,
                    TokenKind::Int("5".to_owned()),
                ],
            ),
            (
                "5 < 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), LessThan),
                vec![
                    TokenKind::Int("5".to_owned()),
                    TokenKind::LessThan,
                    TokenKind::Int("5".to_owned()),
                ],
            ),
            (
                "5 == 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Equals),
                vec![
                    TokenKind::Int("5".to_owned()),
                    TokenKind::Equal,
                    TokenKind::Int("5".to_owned()),
                ],
            ),
            (
                "5 != 5".to_string(),
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), NotEquals),
                vec![
                    TokenKind::Int("5".to_owned()),
                    TokenKind::NotEqual,
                    TokenKind::Int("5".to_owned()),
                ],
            ),
            (
                "true == true".to_string(),
                test_util::create_infix_test_case(
                    BooleanLiteral(true),
                    BooleanLiteral(true),
                    Equals,
                ),
                vec![TokenKind::True, TokenKind::Equal, TokenKind::True],
            ),
            (
                "true != false".to_string(),
                test_util::create_infix_test_case(
                    BooleanLiteral(true),
                    BooleanLiteral(false),
                    NotEquals,
                ),
                vec![TokenKind::True, TokenKind::NotEqual, TokenKind::False],
            ),
            (
                "false == false".to_string(),
                test_util::create_infix_test_case(
                    BooleanLiteral(false),
                    BooleanLiteral(false),
                    Equals,
                ),
                vec![TokenKind::False, TokenKind::Equal, TokenKind::False],
            ),
        ];

        {
            test_cases
                .iter()
                .for_each(|(input, expected_ast, expected_tokens)| {
                    let statements = test_util::expect_parsed_program(input);

                    let actual_statement = statements.first().expect("Should be one statement");

                    assert_eq!(&actual_statement.statement_type, expected_ast);

                    let mut actual_token_kinds: Vec<TokenKind> = actual_statement
                        .tokens
                        .iter()
                        .map(|token| token.token_kind.clone())
                        .collect();

                    let mut expected_tokens = expected_tokens.to_vec();

                    actual_token_kinds.sort();
                    expected_tokens.sort();

                    assert_eq!(
                        expected_tokens, actual_token_kinds,
                        "Failed parsing tokens for {input}"
                    );
                });
        };
    }

    #[test]
    fn can_parse_hash_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

        let statements = test_util::expect_parsed_program(input);
        let actual_statement = statements.first().expect("Should be one statement");

        match &actual_statement.statement_type {
            StatementType::Expression(Expression::HashLiteral(pairs)) => {
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

        match &actual_statement.statement_type {
            StatementType::Expression(Expression::HashLiteral(pairs)) => {
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

        match &actual_statement.statement_type {
            StatementType::Expression(Expression::HashLiteral(pairs)) => {
                assert_eq!(pairs.len(), 0);
            }
            _ => panic!("Should have parsed a hash literal"),
        }
    }
}
