#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Let,
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
}

pub enum HasInfix {
    Arithmic(Operator),
    Call(),
    No(Token),
}

pub enum ParsedToken {
    CompleteToken(Token),
    PossibleMultipart(FirstPart),
    NumericStart,
    AlphabeticStart,
}

pub enum FirstPart {
    Bang,
    Equal,
}
pub enum ParsedMultipartToken {
    OnlyOnePart(Token),
    Multipart(Token),
}

use Token::*;

use crate::ast::Operator;

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

    pub fn from(char: char) -> ParsedToken {
        use ParsedToken::*;
        match char {
            '!' => PossibleMultipart(FirstPart::Bang),
            '=' => PossibleMultipart(FirstPart::Equal),
            '+' => CompleteToken(Token::Add),
            '-' => CompleteToken(Token::Minus),
            ':' => CompleteToken(Token::Assign),
            '}' => CompleteToken(Token::RBrace),
            '{' => CompleteToken(Token::LBrace),
            ')' => CompleteToken(Token::RParen),
            '(' => CompleteToken(Token::LParen),
            ']' => CompleteToken(Token::RBracket),
            '[' => CompleteToken(Token::LBracket),
            '<' => CompleteToken(Token::LessThan),
            '>' => CompleteToken(Token::GreaterThan),
            ',' => CompleteToken(Token::Comma),
            '.' => CompleteToken(Token::Period),
            '~' => CompleteToken(Token::Lasagna),
            '/' => CompleteToken(Token::Slash),
            '*' => CompleteToken(Token::Asterix),
            numeric_char if numeric_char.is_numeric() => NumericStart,
            alphabetic_char if alphabetic_char.is_alphabetic() => AlphabeticStart,
            _ => CompleteToken(Token::Illegal),
        }
    }

    pub fn get_precedence(&self) -> Precedence {
        match self {
            LParen => Precedence::Call,
            Equal | NotEqual => Precedence::Equals,
            LessThan | GreaterThan => Precedence::LessGreater,
            Add | Minus => Precedence::Sum,
            Slash | Asterix => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }

    pub fn lex_second_part(
        first_char: FirstPart,
        second_char: Option<char>,
    ) -> ParsedMultipartToken {
        use FirstPart::*;
        use ParsedMultipartToken::*;
        match first_char {
            Bang => match second_char {
                Some('=') => Multipart(Token::NotEqual),
                _ => OnlyOnePart(Token::Bang),
            },
            Equal => match second_char {
                Some('=') => Multipart(Token::Equal),
                _ => OnlyOnePart(Token::Illegal),
            },
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
            unexpected_token => HasInfix::No(unexpected_token.clone()),
        }
    }
}
