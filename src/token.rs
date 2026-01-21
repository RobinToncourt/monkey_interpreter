use std::fmt::Display;

pub type TokenType = String;

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
    GreaterEqual,
    LesserEqual,

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

    pub fn get_type(&self) -> TokenType {
        match self {
            Self::Illegal => String::from("Illegal"),
            Self::Ident(_) => String::from("Ident"),
            Self::Int(_) => String::from("Int"),
            Self::Str(_) => String::from("Str"),
            Self::Assign => String::from("Assign"),
            Self::Plus => String::from("Plus"),
            Self::Minus => String::from("Minus"),
            Self::Asterisk => String::from("Asterisk"),
            Self::Slash => String::from("Slash"),
            Self::Bang => String::from("Bang"),
            Self::Equal => String::from("Equal"),
            Self::NotEqual => String::from("NotEqual"),
            Self::GreaterThan => String::from("GreaterThan"),
            Self::LesserThan => String::from("LesserThan"),
            Self::GreaterEqual => String::from("GreaterEqual"),
            Self::LesserEqual => String::from("LesserEqual"),
            Self::Comma => String::from("Comma"),
            Self::Semicolon => String::from("Semicolon"),
            Self::LParen => String::from("LParen"),
            Self::RParen => String::from("RParen"),
            Self::LBrace => String::from("LBrace"),
            Self::RBrace => String::from("RBrace"),
            Self::Function => String::from("Function"),
            Self::Let => String::from("Let"),
            Self::True => String::from("True"),
            Self::False => String::from("False"),
            Self::If => String::from("If"),
            Self::Else => String::from("Else"),
            Self::Return => String::from("Return"),
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
            Self::GreaterEqual => write!(f, ">="),
            Self::LesserEqual => write!(f, "<="),
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
