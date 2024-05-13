#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Bang,
    Add,
    Minus,
    Assign,
    RBrace,
    LBrace,
    RParen,
    LParen,
    RBracket,
    LBracket,
    Illegal,
    Lasagna,
    Equal,
    NotEqual,
    Ident(String),
    Int(String),
    Comma,
    Return,
    If,
    LessThan,
    GreaterThan,
    True,
    False,
    Else,
}

use Token::*;

impl Token {
    pub fn parse_keyword(literal_keyword: &str) -> Token {
        match literal_keyword {
            "return" => Return,
            "true" => True,
            "false" => False,
            "else" => Else,
            "if" => If,
            identifier_literal => Ident(identifier_literal.to_string()),
        }
    }
}
