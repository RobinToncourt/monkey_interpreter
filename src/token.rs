use std::{fmt::Display, mem::Discriminant};

pub type TokenType = Discriminant<Token>;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,

    // Identifier + literals.
    Ident(String),
    Int(String),
    Str(String),

    // Operators.
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,

    // Comparators.
    Equal,
    NotEqual,
    GreaterThan,
    LesserThan,

    // Delimiters.
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords.
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    pub fn empty_ident() -> Self {
        Self::Ident(String::new())
    }

    pub fn empty_int() -> Self {
        Self::Int(String::new())
    }

    pub fn empty_str() -> Self {
        Self::Str(String::new())
    }

    pub fn lookup_ident(ident: &str) -> Self {
        match ident {
            "let" => Self::Let,
            "fn" => Self::Function,
            "true" => Self::True,
            "false" => Self::False,
            "if" => Self::If,
            "else" => Self::Else,
            "return" => Self::Return,
            _ => Self::Ident(ident.to_string()),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Illegal => write!(f, "ILLEGAL"),
            Self::Ident(name) => write!(f, "{name}"),
            Self::Int(value) => write!(f, "{value}"),
            Self::Str(s) => write!(f, "{s}"),
            Self::Assign => write!(f, "="),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Bang => write!(f, "!"),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::GreaterThan => write!(f, ">"),
            Self::LesserThan => write!(f, "<"),
            //Self::GreaterEqual => write!(f, ">="),
            //Self::LesserEqual => write!(f, "<="),
            Self::Comma => write!(f, ","),
            Self::Semicolon => write!(f, ";"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::Function => write!(f, "function"),
            Self::Let => write!(f, "let"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Return => write!(f, "return"),
        }
    }
}
