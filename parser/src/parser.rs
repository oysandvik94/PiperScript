use std::{iter::Peekable, vec::IntoIter};

use lexer::token::Token;

use crate::{
    ast::{Expression, Identifier, Operator, Program, Statement},
    parse_errors::ParseError,
};

pub struct Parser {
    token_iter: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            token_iter: tokens.clone().into_iter().peekable(),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = Vec::new();
        let mut parse_errors: Vec<ParseError> = Vec::new();

        while self.token_iter.peek().is_some() {
            match self.parse_statement() {
                Ok(parsed_statement) => statements.push(parsed_statement),
                Err(parse_error) => parse_errors.push(parse_error),
            };
        }

        Program {
            statements,
            parse_errors,
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.token_iter.next() {
            Some(Token::Lasagna) => self.parse_assign_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(token) => self.parse_expression_statement(token),
            None => self.handle_error(ParseError::ExpectedToken),
        }
    }

    fn expected_identifier(&mut self) -> Result<Identifier, ParseError> {
        let peeked_token_is_ident: Result<Identifier, ParseError> = match self.token_iter.peek() {
            Some(peeked_token) => Identifier::parse_from_token(peeked_token),
            None => Err(ParseError::ExpectedToken),
        };

        match peeked_token_is_ident {
            Ok(parsed_identifier) => {
                self.token_iter.next();
                Ok(parsed_identifier)
            }
            Err(parse_error) => self.handle_error(parse_error),
        }
    }

    fn parse_assign_statement(&mut self) -> Result<Statement, ParseError> {
        let identifier: Identifier = self.expected_identifier()?;

        self.expect_peek(Token::Assign)?;

        // TODO: skip over expressions until we know how to handle them
        loop {
            if let Some(token) = self.token_iter.peek() {
                if *token == Token::Lasagna {
                    break;
                }
            }

            self.token_iter.next();
        }

        self.expect_peek(Token::Lasagna)?;
        let assign_statement = Statement::AssignStatement(identifier, Expression::TodoExpression);
        Ok(assign_statement)
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        // TODO: skip over expressions until we know how to handle them
        self.iterate_to_next_statement();

        Ok(Statement::ReturnStatement(Expression::TodoExpression))
    }

    fn parse_expression_statement(
        &mut self,
        current_token: Token,
    ) -> Result<Statement, ParseError> {
        let expression = self.parse_expression(current_token)?;

        Ok(Statement::ExpressionStatement(expression))
    }

    fn parse_expression(&mut self, current_token: Token) -> Result<Expression, ParseError> {
        self.parse_prefix_expression(&current_token)
    }

    fn parse_prefix_expression(&mut self, token: &Token) -> Result<Expression, ParseError> {
        match token {
            Token::Ident(literal) => Ok(Expression::IdentifierExpression(Identifier(
                literal.to_string(),
            ))),
            Token::Int(integer_literal) => match integer_literal.parse::<i32>() {
                Ok(parsed_number) => Ok(Expression::IntegerExpression(parsed_number)),
                Err(error) => Err(ParseError::ParseIntegerError(error)),
            },
            Token::Bang => self.create_prefix_expression(Operator::Bang),
            Token::Minus => self.create_prefix_expression(Operator::Minus),
            unexpected_token => Err(ParseError::NoPrefixExpression(unexpected_token.clone())),
        }
    }

    fn create_prefix_expression(&mut self, operator: Operator) -> Result<Expression, ParseError> {
        let token = match self.token_iter.next() {
            Some(token) => Ok(token),
            None => Err(ParseError::NoPrefixPartner),
        }?;

        let right = self.parse_expression(token)?;
        Ok(Expression::PrefixExpression {
            right: Box::new(right),
            operator,
        })
    }

    fn expect_peek(&mut self, expected_token_type: Token) -> Result<Token, ParseError> {
        match self.token_iter.next_if_eq(&expected_token_type) {
            Some(token) => Ok(token),
            None => {
                let next_token = self.token_iter.peek().cloned();
                self.handle_error(ParseError::UnexpectedToken {
                    expected_token: expected_token_type,
                    found_token: next_token,
                })
            }
        }
    }

    fn handle_error<T>(&mut self, parse_error: ParseError) -> Result<T, ParseError> {
        self.iterate_to_next_statement();

        Err(parse_error)
    }

    fn iterate_to_next_statement(&mut self) {
        for token in self.token_iter.by_ref() {
            if token == Token::Lasagna {
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use lexer::lexer::generate_tokens;

    use crate::{
        ast::{Expression, Identifier, Program, Statement},
        parser::{ParseError, Parser},
        test_util::{check_parser_errors, parse_program},
    };

    use super::Token;

    #[test]
    fn parse_assign_statement() {
        let source_code = "
            ~x: 5~
            ~y: 10~
            ~foobar: 54456~
        ";

        let tokens: Vec<Token> = generate_tokens(source_code);
        let mut parser: Parser = Parser::new(tokens);
        let program: Program = parser.parse_program();

        check_parser_errors(&program);
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

        expected_identifiers
            .iter()
            .enumerate()
            .for_each(|(idx, ident)| test_let_statement(&program.statements[idx], ident));
    }

    #[test]
    fn parse_return_statement() {
        let source_code = "
            return 5~
            return foobar~
        ";

        let program: Program = parse_program(source_code);

        check_parser_errors(&program);
        assert_eq!(
            program.statements.len(),
            2,
            "Program should be parsed to 3 statements"
        );

        program
            .statements
            .iter()
            .for_each(|ident| assert!(matches!(ident, Statement::ReturnStatement(_))));
    }

    #[test]
    fn test_return_statement() {
        let return_statement = "return foo";

        let program: Program = parse_program(return_statement);

        assert_eq!(1, program.statements.len());
        assert_eq!(
            Statement::ReturnStatement(Expression::TodoExpression),
            *program
                .statements
                .first()
                .expect("Should retrieve first and only statement")
        );
    }

    #[test]
    fn parse_errors() {
        let source_code = "
            ~x 5~
            ~: 10~
            ~ 54456~
        ";

        let program: Program = parse_program(source_code);

        assert_eq!(program.parse_errors.len(), 3, "Should have 3 errors");
        program.parse_errors.iter().for_each(|parse_error| {
            assert!(matches!(
                parse_error,
                ParseError::UnexpectedToken {
                    expected_token: _,
                    found_token: _
                }
            ))
        });
    }

    fn test_let_statement(found: &Statement, expected_identifier: &Identifier) {
        match found {
            Statement::AssignStatement(found_identfier, _) => {
                assert_eq!(found_identfier, expected_identifier)
            }
            incorrect => panic!("Expected let-statement, but got {incorrect:?}"),
        };
    }
}
