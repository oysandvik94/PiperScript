use std::{iter::Peekable, str::Chars, vec::IntoIter};

use crate::{ast::Identifier, parse_errors::ParseError};

use super::token::{ParsedMultipartToken, ParsedToken, Token};

pub struct LexedTokens {
    token_iter: Peekable<IntoIter<Token>>,
}

impl From<&str> for LexedTokens {
    fn from(source_code: &str) -> Self {
        let mut code_iter = source_code.chars().peekable();

        let mut tokens: Vec<Token> = Vec::new();
        while let Some(current_char) = code_iter.next() {
            if current_char.is_whitespace() {
                continue;
            }

            let lexed_token: Token = match Token::from(current_char) {
                ParsedToken::CompleteToken(token) => token,
                ParsedToken::PossibleMultipart(first_part) => {
                    match Token::lex_second_part(first_part, code_iter.peek().cloned()) {
                        ParsedMultipartToken::Multipart(token) => {
                            code_iter.next();
                            token
                        }
                        ParsedMultipartToken::OnlyOnePart(token) => token,
                    }
                }
                ParsedToken::AlphabeticStart => {
                    let literal: String =
                        read_literal(&mut code_iter, current_char, |char| char.is_alphabetic());

                    Token::parse_keyword(&literal)
                }
                ParsedToken::NumericStart => {
                    let literal: String =
                        read_literal(&mut code_iter, current_char, |char| char.is_numeric());

                    Token::Int(literal)
                }
            };

            tokens.push(lexed_token);
        }

        LexedTokens {
            token_iter: tokens.into_iter().peekable(),
        }
    }
}

impl LexedTokens {
    pub fn consume(&mut self) -> Option<Token> {
        self.token_iter.next()
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.token_iter.peek()
    }

    pub fn iterate_to_next_statement(&mut self) {
        for token in self.token_iter.by_ref() {
            if token == Token::Lasagna {
                break;
            }
        }
    }

    pub fn expect_peek(&mut self, expected_token_type: Token) -> Result<Token, ParseError> {
        match self.token_iter.next_if_eq(&expected_token_type) {
            Some(token) => Ok(token),
            None => {
                let next_token = self.token_iter.peek().cloned();
                Err(ParseError::UnexpectedToken {
                    expected_token: expected_token_type,
                    found_token: next_token,
                })
            }
        }
    }

    pub fn expected_identifier(&mut self) -> Result<Identifier, ParseError> {
        match self.token_iter.peek() {
            Some(peeked_token) => {
                let parsed_identifier = Identifier::parse_from_token(peeked_token)?;
                self.consume();
                Ok(parsed_identifier)
            }
            None => Err(ParseError::ExpectedToken),
        }
    }
}

fn read_literal<F>(iterator: &mut Peekable<Chars>, first_char: char, read_until: F) -> String
where
    F: Fn(&char) -> bool,
{
    let mut literal = String::from(first_char);

    while let Some(c) = iterator.peek().cloned().filter(|c| read_until(c)) {
        literal.push(c);
        iterator.next();
    }

    literal
}

#[cfg(test)]
mod tests {
    use crate::lexer::{lexedtokens::LexedTokens, token::Token};

    #[test]
    fn parse_sympols() {
        let source_code = "
            !+:}{)(][~
        ";

        let expected_tokens = vec![
            Token::Bang,
            Token::Add,
            Token::Assign,
            Token::RBrace,
            Token::LBrace,
            Token::RParen,
            Token::LParen,
            Token::RBracket,
            Token::LBracket,
            Token::Lasagna,
        ];

        let mut found_tokens: LexedTokens = LexedTokens::from(source_code);
        let mut expected_iter = expected_tokens.into_iter();
        while let Some(token) = found_tokens.consume() {
            let expected_token = expected_iter.next().unwrap();
            assert_eq!(token, expected_token);
        }
    }

    #[test]
    fn parse_identifier() {
        let source_code = "
            foo
        ";

        let expected_tokens = [Token::Ident("foo".to_string())];

        let mut found_tokens: LexedTokens = LexedTokens::from(source_code);

        assert_eq!(
            found_tokens.token_iter.len(),
            expected_tokens.len(),
            "List of expected tokens should be the same as found tokens"
        );
        expected_tokens.iter().enumerate().for_each(|(idx, token)| {
            assert_eq!(
                token,
                &found_tokens.token_iter.nth(idx).expect("Should have token"),
                "Token in position {idx} was not parsed"
            )
        });
    }

    #[test]
    fn parse_code() {
        let source_code = "
            ~foo: 5~
            foo + 6
            ~fooFunc(x, y):
                ~res x+y
                return res
            ~

            ~if(5 < 10):
                5 + 6
                6 + 7
                return true
            ~else:
!
                return false
            ~
            ==
            !=
        ";

        let expected_tokens = vec![
            Token::Lasagna,
            Token::Ident("foo".to_string()),
            Token::Assign,
            Token::Int("5".to_string()),
            Token::Lasagna,
            Token::Ident("foo".to_string()),
            Token::Add,
            Token::Int("6".to_string()),
            Token::Lasagna,
            Token::Ident("fooFunc".to_string()),
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::Assign,
            Token::Lasagna,
            Token::Ident("res".to_string()),
            Token::Ident("x".to_string()),
            Token::Add,
            Token::Ident("y".to_string()),
            Token::Return,
            Token::Ident("res".to_string()),
            Token::Lasagna,
            Token::Lasagna,
            Token::If,
            Token::LParen,
            Token::Int("5".to_string()),
            Token::LessThan,
            Token::Int("10".to_string()),
            Token::RParen,
            Token::Assign,
            Token::Int("5".to_string()),
            Token::Add,
            Token::Int("6".to_string()),
            Token::Int("6".to_string()),
            Token::Add,
            Token::Int("7".to_string()),
            Token::Return,
            Token::True,
            Token::Lasagna,
            Token::Else,
            Token::Assign,
            Token::Bang,
            Token::Return,
            Token::False,
            Token::Lasagna,
            Token::Equal,
            Token::NotEqual,
        ];

        let mut found_tokens: LexedTokens = LexedTokens::from(source_code);
        let mut expected_iter = expected_tokens.into_iter();
        while let Some(token) = found_tokens.consume() {
            let expected_token = expected_iter.next().unwrap();
            assert_eq!(token, expected_token);
        }
        //assert_eq!(
        //    expected_tokens.len(),
        //    found_tokens.token_iter.len(),
        //    "List of expected tokens should be the same as found tokens"
        //);

        //expected_tokens.iter().enumerate().for_each(|(idx, token)| {
        //    let found_token = &found_tokens.token_iter.nth(idx).expect("Should be a token");
        //    assert_eq!(
        //        token, found_token,
        //        "Expected {token:?} to be {found_token:?} at index {idx}"
        //    )
        //});
    }
}
