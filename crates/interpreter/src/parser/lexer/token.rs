#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Let,
    Bang,
    Add,
    Minus,
    Colon,
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
    Str(String),
    Comma,
    Return,
    If,
    Func,
    LessThan,
    GreaterThan,
    Slash,
    True,
    False,
    Else,
    Period,
    Asterix,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

pub enum HasInfix {
    Arithmic(Operator),
    Call(),
    Index(),
    No(Token),
}

pub enum FirstPart {
    Bang,
    Equal,
}

use Token::*;

use crate::parser::ast::Operator;

impl Token {
    pub fn parse_keyword(literal_keyword: &str) -> Token {
        match literal_keyword {
            "return" => Return,
            "let" => Let,
            "true" => True,
            "false" => False,
            "else" => Else,
            "if" => If,
            "fn" => Func,
            identifier_literal => Ident(identifier_literal.to_string()),
        }
    }

    pub fn get_precedence(&self) -> Precedence {
        match self {
            LBracket => Precedence::Index,
            LParen => Precedence::Call,
            Equal | NotEqual => Precedence::Equals,
            LessThan | GreaterThan => Precedence::LessGreater,
            Add | Minus => Precedence::Sum,
            Slash | Asterix => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }

    pub fn has_infix(&self) -> HasInfix {
        match self {
            Token::NotEqual => HasInfix::Arithmic(Operator::NotEquals),
            Token::Add => HasInfix::Arithmic(Operator::Plus),
            Token::Minus => HasInfix::Arithmic(Operator::Minus),
            Token::Equal => HasInfix::Arithmic(Operator::Equals),
            Token::LessThan => HasInfix::Arithmic(Operator::LessThan),
            Token::GreaterThan => HasInfix::Arithmic(Operator::GreaterThan),
            Token::Slash => HasInfix::Arithmic(Operator::DividedBy),
            Token::Asterix => HasInfix::Arithmic(Operator::Multiply),
            Token::LParen => HasInfix::Call(),
            Token::LBracket => HasInfix::Index(),
            unexpected_token => HasInfix::No(unexpected_token.clone()),
        }
    }
}
