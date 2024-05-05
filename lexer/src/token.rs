#[derive(PartialEq, Debug, Clone)]
pub enum TokenType {
    Bang,
    Add,
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
    Ident,
    Int,
    Comma,
    Return,
    If,
    LessThan,
    GreaterThan,
    True,
    False,
    Else,
}

use TokenType::*;

impl TokenType {
    pub fn parse_keyword(literal_keyword: &str) -> TokenType {
        match literal_keyword {
            "return" => Return,
            "true" => True,
            "false" => False,
            "else" => Else,
            "if" => If,
            _ => Ident,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: char) -> Token {
        Token {
            token_type,
            literal: literal.to_string(),
        }
    }
}
