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
    Period,
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
            numeric_char if numeric_char.is_numeric() => NumericStart,
            alphabetic_char if alphabetic_char.is_alphabetic() => AlphabeticStart,
            _ => CompleteToken(Token::Illegal),
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
}
