#[derive(PartialEq, Clone)]
pub struct Token {
    pub token_kind: TokenKind,
    pub location: Location,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token: {:?} Line: {}",
            self.token_kind, self.location.line_number
        )
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Location {
    pub line_number: u16,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind {
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
    EOF,
    NewLine,
    Space,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Let => write!(f, "let"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Add => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::Illegal => write!(f, "illegal"),
            TokenKind::Lasagna => write!(f, "lasagna"),
            TokenKind::Equal => write!(f, "=="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::Ident(s) => write!(f, "{}", s),
            TokenKind::Int(i) => write!(f, "{}", i),
            TokenKind::Str(s) => write!(f, "\"{}\"", s),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Return => write!(f, "return"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Func => write!(f, "func"),
            TokenKind::LessThan => write!(f, "<"),
            TokenKind::GreaterThan => write!(f, ">"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Period => write!(f, "."),
            TokenKind::Asterix => write!(f, "*"),
            TokenKind::EOF => write!(f, "EOF"),
            TokenKind::NewLine => write!(f, "\\n"),
            TokenKind::Space => write!(f, " "),
        }
    }
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
    No(TokenKind),
}

pub enum FirstPart {
    Bang,
    Equal,
}

use std::fmt::{Debug, Display};

use TokenKind::*;

use crate::parser::ast::Operator;

impl Token {
    pub fn is_whitespace(&self) -> bool {
        matches!(self.token_kind, TokenKind::Space | TokenKind::NewLine)
    }
}

impl TokenKind {
    pub fn parse_keyword(literal_keyword: &str) -> TokenKind {
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
            TokenKind::NotEqual => HasInfix::Arithmic(Operator::NotEquals),
            TokenKind::Add => HasInfix::Arithmic(Operator::Plus),
            TokenKind::Minus => HasInfix::Arithmic(Operator::Minus),
            TokenKind::Equal => HasInfix::Arithmic(Operator::Equals),
            TokenKind::LessThan => HasInfix::Arithmic(Operator::LessThan),
            TokenKind::GreaterThan => HasInfix::Arithmic(Operator::GreaterThan),
            TokenKind::Slash => HasInfix::Arithmic(Operator::DividedBy),
            TokenKind::Asterix => HasInfix::Arithmic(Operator::Multiply),
            TokenKind::LParen => HasInfix::Call(),
            TokenKind::LBracket => HasInfix::Index(),
            unexpected_token => HasInfix::No(unexpected_token.clone()),
        }
    }

    pub(crate) fn is_beginning_of_statement(&self) -> bool {
        match self {
            Let => true,
            Bang => true,
            Add => false,
            Minus => true,
            Colon => false,
            RBrace => false,
            LBrace => true,
            RParen => false,
            LParen => true,
            RBracket => false,
            LBracket => true,
            Illegal => true,
            Lasagna => false,
            Equal => false,
            NotEqual => false,
            Ident(_) => true,
            Int(_) => true,
            Str(_) => true,
            Comma => false,
            Return => true,
            If => true,
            Func => true,
            LessThan => false,
            GreaterThan => false,
            Slash => false,
            True => true,
            False => true,
            Else => true,
            Period => false,
            Asterix => false,
            EOF => false,
            NewLine => false,
            Space => false,
        }
    }

    pub fn valid_prefix(&self) -> bool {
        matches!(
            self,
            TokenKind::Ident(_)
                | TokenKind::Int(_)
                | TokenKind::Str(_)
                | TokenKind::Bang
                | TokenKind::Minus
                | TokenKind::LParen
                | TokenKind::If
                | TokenKind::Func
                | TokenKind::True
                | TokenKind::False
                | TokenKind::LBracket
                | TokenKind::LBrace
        )
    }
}
