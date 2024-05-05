use std::{iter::Peekable, vec::IntoIter};

use lexer::token::{Token, TokenType};

use crate::ast::{Expression, Identifier, Program, Statement};

pub enum ParseError {
    UnexpectedToken {
        expected_token: TokenType,
        found_token: Option<Token>,
    },
    ExpectedToken,
    UnknownToken(Token),
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

        Program { statements, parse_errors }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.token_iter.next() {
            Some(token) if token.token_type == TokenType::Assign => self.parse_assign_statement(),
            Some(unknown_token) => Err(ParseError::UnknownToken(unknown_token)),
            None => Err(ParseError::ExpectedToken),
        }
    }

    fn parse_assign_statement(&mut self) -> Result<Statement, ParseError> {
        let identifier: Identifier = Identifier(self.expect_peek(TokenType::Ident)?.literal);
        let assign_statement = Statement::AssignStatement(identifier, Expression::TodoExpression);

        Ok(assign_statement)
    }

    fn expect_peek(&mut self, expected_token_type: TokenType) -> Result<Token, ParseError> {
        match self
            .token_iter
            .next_if(|x| x.token_type == expected_token_type)
        {
            Some(token) => Ok(token),
            None => Err(ParseError::UnexpectedToken {
                expected_token: expected_token_type,
                found_token: self.token_iter.peek().cloned(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use lexer::lexer::generate_tokens;

    use crate::{
        ast::{Identifier, Program, Statement},
        parser::Parser,
    };

    use super::Token;

    #[test]
    fn parse_let_statement() {
        let source_code = "
            ~x: 5~
            ~y: 10~
            ~foobar: 54456~
        ";

        let tokens: Vec<Token> = generate_tokens(source_code);
        let mut parser: Parser = Parser::new(tokens);
        let program: Program = parser.parse_program();

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

    fn test_let_statement(found: &Statement, expected_identifier: &Identifier) {
        match found {
            Statement::AssignStatement(found_identfier, _) => {
                assert_eq!(found_identfier, expected_identifier)
            }
            incorrect => panic!("Expected let-statement, but got {incorrect:?}"),
        };
    }
}
