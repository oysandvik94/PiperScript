use tracing::{event, Level};

use crate::parser::{
    ast::{BlockStatement, Identifier, Operator, Statement},
    lexer::token::{HasInfix, Precedence, Token},
    parse_errors::{ParseError, TokenExpectation},
    Parser,
};

use super::{
    functions::{CallExpression, FunctionLiteral},
    if_expression::IfExpression,
};

#[derive(PartialEq, Debug)]
pub enum Expression {
    IdentifierLiteral(Identifier),
    IntegerLiteral(i32),
    BooleanLiteral(bool),
    Prefix {
        right: Box<Expression>,
        operator: Operator,
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
            Token::If => IfExpression::parse_if_expression(parser),
            Token::Func => FunctionLiteral::parse(parser),
            Token::True => Ok(Expression::BooleanLiteral(true)),
            Token::False => Ok(Expression::BooleanLiteral(false)),
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
        operator: Operator,
    ) -> Result<Expression, ParseError> {
        let token = match parser.tokens.consume() {
            Some(token) => Ok(token),
            None => Err(ParseError::NoPrefixPartner),
        }?;

        let right = Self::parse(parser, token, Precedence::Prefix)?;
        Ok(Expression::Prefix {
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
                let right = Self::parse(parser, next_token, precedence)?;

                Ok(Expression::Infix {
                    left: Box::from(left),
                    right: Box::from(right),
                    operator,
                })
            }
            HasInfix::Call() => CallExpression::parse(parser, left),
            HasInfix::No(token) => Err(ParseError::NoInfixExpression(token.clone())),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{
        ast::{Identifier, Operator, Program, Statement},
        expressions::{expression::Expression, expression_statement::ExpressionStatement},
        test_util,
    };

    #[test]
    fn test_integer_expression() {
        let input: &str = "5.";

        let program: Program = test_util::parse_program(input);
        test_util::has_parser_errors(&program);

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

        let program: Program = test_util::parse_program(input);
        test_util::has_parser_errors(&program);

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

        let program: Program = test_util::parse_program(input);
        test_util::has_parser_errors(&program);

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
                test_util::create_prefix_test_case(Expression::IntegerLiteral(5), Operator::Bang),
            ),
            (
                "-15.",
                test_util::create_prefix_test_case(Expression::IntegerLiteral(15), Operator::Minus),
            ),
        ]
        .map(|(input, statement)| TestCase {
            input: input.to_string(),
            statement,
        });

        for test_case in test_cases {
            let program: Program = test_util::parse_program(&test_case.input);
            test_util::has_parser_errors(&program);

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
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Plus),
            ),
            (
                "5 - 5",
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Minus),
            ),
            (
                "5 * 5",
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Multiply),
            ),
            (
                "5 / 5",
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), DividedBy),
            ),
            (
                "5 > 5",
                test_util::create_infix_test_case(
                    IntegerLiteral(5),
                    IntegerLiteral(5),
                    GreaterThan,
                ),
            ),
            (
                "5 < 5",
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), LessThan),
            ),
            (
                "5 == 5",
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), Equals),
            ),
            (
                "5 != 5",
                test_util::create_infix_test_case(IntegerLiteral(5), IntegerLiteral(5), NotEquals),
            ),
            (
                "true == true",
                test_util::create_infix_test_case(
                    BooleanLiteral(true),
                    BooleanLiteral(true),
                    Equals,
                ),
            ),
            (
                "true != false",
                test_util::create_infix_test_case(
                    BooleanLiteral(true),
                    BooleanLiteral(false),
                    NotEquals,
                ),
            ),
            (
                "false == false",
                test_util::create_infix_test_case(
                    BooleanLiteral(false),
                    BooleanLiteral(false),
                    Equals,
                ),
            ),
        ]
        .map(|(input, statement)| TestCase {
            input: input.to_string(),
            statement,
        });

        for test_case in test_cases {
            let program: Program = test_util::parse_program(&test_case.input);
            test_util::has_parser_errors(&program);

            let statement = program.statements.first().expect("Should be one statement");

            assert_eq!(
                statement, &test_case.statement,
                "Parsed statement should match testcase"
            );
            assert_eq!(program.statements.len(), 1, "Should only parse 1 statement");
        }
    }
}
