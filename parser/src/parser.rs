use std::{fmt::Display, iter::Peekable, vec::IntoIter};

use lexer::token::Token;

use crate::{
    ast::{Expression, Identifier, Program, Statement},
    expression_parser::PrefixParser,
};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected_token: Token,
        found_token: Option<Token>,
    },
    ExpectedToken,
    UnknownToken(Token),
    ExpressionError(String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected_token,
                found_token,
            } => write!(f, "Expected token of type {expected_token:?}, but received token of type {found_token:?}"),
            ParseError::ExpectedToken => write!(f, "Expected to receive a token, but no token was received"),
            ParseError::UnknownToken(token) => write!(f, "Received unknown token of type {token:?}, don't know how to handle it"),
            ParseError::ExpressionError(error) => write!(f, "{error}")
        }
    }
}

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
            // TODO: matche better
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

    fn parse_expression_statement(&mut self, first_token: Token) -> Result<Statement, ParseError> {
        let expression = self.parse_expression(first_token)?;

        Ok(Statement::ExpressionStatement(expression))
    }

    fn parse_expression(&mut self, first_token: Token) -> Result<Expression, ParseError> {
        match first_token.parse(self) {
            Some(prefix) => Ok(prefix),
            None => self.handle_error(ParseError::ExpressionError(
                "Could not parse prefix. Not sure what to do here yet".to_string(),
            )),
        }
    }

    fn expect_peek(&mut self, expected_token_type: Token) -> Result<Token, ParseError> {
        match self
            .token_iter
            .next_if_eq(&expected_token_type)
        {
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
