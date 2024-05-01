use std::{iter::Peekable, str::Chars};

use token::{Token, TokenType};
use TokenType::*;

mod token;

#[derive(Debug)]
pub struct Lexer<'a> {
    source_code: &'a str,
}

impl Lexer<'_> {
    pub fn generate_tokens(&self) -> Vec<Token> {
        let mut code_iter = self.source_code.chars().peekable();

        let mut tokens: Vec<Token> = Vec::new();
        while let Some(current_char) = code_iter.next() {
            if current_char.is_whitespace() {
                continue;
            }

            let lexed_token = match current_char {
                '!' => match code_iter.peek() {
                    Some('=') => Token {
                        token_type: NotEqual,
                        literal: {
                            current_char.to_string()
                                + &code_iter.next().expect("Should be equals char").to_string()
                        },
                    },
                    _ => Token::new(Bang, current_char),
                },
                '=' => match code_iter.peek() {
                    Some('=') => Token {
                        token_type: Equal,
                        literal: {
                            current_char.to_string()
                                + &code_iter.next().expect("Should be equals char").to_string()
                        },
                    },
                    _ => Token::new(Illegal, current_char),
                },
                '+' => Token::new(Add, current_char),
                ':' => Token::new(Assign, current_char),
                '}' => Token::new(RBrace, current_char),
                '{' => Token::new(LBrace, current_char),
                ')' => Token::new(RParen, current_char),
                '(' => Token::new(LParen, current_char),
                ']' => Token::new(RBracket, current_char),
                '[' => Token::new(LBracket, current_char),
                '<' => Token::new(LessThan, current_char),
                '>' => Token::new(GreaterThan, current_char),
                ',' => Token::new(Comma, current_char),
                '~' => Token::new(Tilde, current_char),
                numeric_char if numeric_char.is_numeric() => {
                    let mut literal: String = String::from(current_char);

                    let is_numeric = |c: &char| c.is_numeric();
                    while let Some(c) = read_target(&mut code_iter, is_numeric) {
                        literal.push(c);
                    }

                    Token {
                        token_type: Int,
                        literal,
                    }
                }
                alphabetic_char if alphabetic_char.is_alphabetic() => {
                    let next_alphabetic = |x: &mut Peekable<Chars>| -> Option<char> {
                        match x.peek() {
                            Some(c) => match c.is_alphabetic() {
                                true => Some(x.next().expect("Expected next character")),
                                false => None,
                            },
                            None => None,
                        }
                    };

                    let mut literal: String = String::from(current_char);
                    while let Some(c) = next_alphabetic(&mut code_iter) {
                        literal.push(c);
                    }

                    Token {
                        token_type: TokenType::parse_keyword(&literal),
                        literal,
                    }
                }
                _ => Token::new(Illegal, current_char),
            };

            tokens.push(lexed_token);
        }

        tokens
    }

    pub fn new(source_code: &str) -> Lexer {
        Lexer { source_code }
    }
}

fn read_target<F>(iterator: &mut Peekable<Chars>, f: F) -> Option<char>
where
    F: Fn(&char) -> bool,
{
    match iterator.peek() {
        Some(c) => match f(c) {
            true => Some(iterator.next().expect("Expected next character")),
            false => None,
        },
        None => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        token::{Token, TokenType},
        Lexer,
    };
    use TokenType::*;

    fn create_token(token_type: TokenType, literal: &str) -> Token {
        Token {
            token_type,
            literal: literal.to_string(),
        }
    }

    #[test]
    fn parse_sympols() {
        let source_code = "
            !+:}{)(][~
        ";

        let expected_tokens = vec![
            create_token(Bang, "!"),
            create_token(Add, "+"),
            create_token(Assign, ":"),
            create_token(RBrace, "}"),
            create_token(LBrace, "{"),
            create_token(RParen, ")"),
            create_token(LParen, "("),
            create_token(RBracket, "]"),
            create_token(LBracket, "["),
            create_token(Tilde, "~"),
        ];

        let lexer = Lexer::new(source_code);
        let found_tokens = lexer.generate_tokens();

        assert_eq!(
            found_tokens.len(),
            expected_tokens.len(),
            "List of expected tokens should be the same as found tokens"
        );
        expected_tokens.iter().enumerate().for_each(|(idx, token)| {
            assert_eq!(
                token, &found_tokens[idx],
                "Token in position {idx} was not parsed"
            )
        });
    }

    #[test]
    fn parse_identifier() {
        let source_code = "
            foo
        ";

        let expected_tokens = [create_token(Ident, "foo")];

        let lexer = Lexer::new(source_code);
        let found_tokens = lexer.generate_tokens();

        assert_eq!(
            found_tokens.len(),
            expected_tokens.len(),
            "List of expected tokens should be the same as found tokens"
        );
        expected_tokens.iter().enumerate().for_each(|(idx, token)| {
            assert_eq!(
                token, &found_tokens[idx],
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
            // Line 1
            create_token(Tilde, "~"),
            create_token(Ident, "foo"),
            create_token(Assign, ":"),
            create_token(Int, "5"),
            create_token(Tilde, "~"),
            // Line 2
            create_token(Ident, "foo"),
            create_token(Add, "+"),
            create_token(Int, "6"),
            // Line 3
            create_token(Tilde, "~"),
            create_token(Ident, "fooFunc"),
            create_token(LParen, "("),
            create_token(Ident, "x"),
            create_token(Comma, ","),
            create_token(Ident, "y"),
            create_token(RParen, ")"),
            create_token(Assign, ":"),
            // Line 4
            create_token(Tilde, "~"),
            create_token(Ident, "res"),
            create_token(Ident, "x"),
            create_token(Add, "+"),
            create_token(Ident, "y"),
            // Line 5
            create_token(Return, "return"),
            create_token(Ident, "res"),
            // Line 6
            create_token(Tilde, "~"),
            // Line 7
            create_token(Tilde, "~"),
            create_token(If, "if"),
            create_token(LParen, "("),
            create_token(Int, "5"),
            create_token(LessThan, "<"),
            create_token(Int, "10"),
            create_token(RParen, ")"),
            create_token(Assign, ":"),
            create_token(Int, "5"),
            create_token(Add, "+"),
            create_token(Int, "6"),
            create_token(Int, "6"),
            create_token(Add, "+"),
            create_token(Int, "7"),
            create_token(Return, "return"),
            create_token(True, "true"),
            // Line 13
            create_token(Tilde, "~"),
            create_token(Else, "else"),
            create_token(Assign, ":"),
            // Line 14
            create_token(Bang, "!"),
            create_token(Return, "return"),
            create_token(False, "false"),
            // Line 15
            create_token(Tilde, "~"),
            create_token(Equal, "=="),
            create_token(NotEqual, "!="),
        ];

        let lexer = Lexer::new(source_code);
        let found_tokens = lexer.generate_tokens();

        expected_tokens.iter().enumerate().for_each(|(idx, token)| {
            assert_eq!(
                token, &found_tokens[idx],
                "Token in position {idx} was not parsed"
            )
        });

        assert_eq!(
            found_tokens.len(),
            expected_tokens.len(),
            "List of expected tokens should be the same as found tokens"
        );
    }
}
