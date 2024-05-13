use std::{iter::Peekable, str::Chars};

use crate::token::Token;

pub fn generate_tokens(source_code: &str) -> Vec<Token> {
    let mut code_iter = source_code.chars().peekable();

    let mut tokens: Vec<Token> = Vec::new();
    while let Some(current_char) = code_iter.next() {
        if current_char.is_whitespace() {
            continue;
        }

        let lexed_token = match current_char {
            '!' => match code_iter.peek() {
                Some('=') => {
                    code_iter.next();
                    Token::NotEqual
                }
                _ => Token::Bang,
            },
            '=' => match code_iter.peek() {
                Some('=') => {
                    code_iter.next();
                    Token::Equal
                }
                _ => Token::Illegal,
            },
            '+' => Token::Add,
            '-' => Token::Minus,
            ':' => Token::Assign,
            '}' => Token::RBrace,
            '{' => Token::LBrace,
            ')' => Token::RParen,
            '(' => Token::LParen,
            ']' => Token::RBracket,
            '[' => Token::LBracket,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            ',' => Token::Comma,
            '~' => Token::Lasagna,
            numeric_char if numeric_char.is_numeric() => {
                let literal: String =
                    read_literal(&mut code_iter, numeric_char, |char| char.is_numeric());
                Token::Int(literal)
            }
            alphabetic_char if alphabetic_char.is_alphabetic() => {
                let literal: String =
                    read_literal(&mut code_iter, alphabetic_char, |char| char.is_alphabetic());
                Token::parse_keyword(&literal)
            }
            _ => Token::Illegal,
        };

        tokens.push(lexed_token);
    }

    tokens
}

fn read_literal<F>(iterator: &mut Peekable<Chars>, first_char: char, read_until: F) -> String
where
    F: Fn(&char) -> bool,
{
    let mut literal: String = String::from(first_char);
    while let Some(c) = expect_peek(iterator, &read_until) {
        literal.push(c);
    }

    literal
}

fn expect_peek<F>(iterator: &mut Peekable<Chars>, f: F) -> Option<char>
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
    use crate::{lexer::generate_tokens, token::Token};

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

        let found_tokens = generate_tokens(source_code);

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

        let expected_tokens = [Token::Ident("foo".to_string())];

        let found_tokens = generate_tokens(source_code);

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

        let found_tokens = generate_tokens(source_code);

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
