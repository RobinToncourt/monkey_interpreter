use std::string::IntoChars;

use crate::token::Token;

pub struct Lexer {
    input: IntoChars,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut new = Self {
            input: input.into_chars(),
            ch: '\0',
        };
        new.read_char();

        new
    }

    fn peek_next(&mut self) -> Option<char> {
        self.input.clone().peekable().peek().copied()
    }

    fn read_char(&mut self) {
        self.ch = self.input.next().unwrap_or('\0');
    }

    fn skip_whitespaces(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        // 10 because identifier are usually under 10 characters
        // and below 20 so only one reallocation.
        let mut ident = String::with_capacity(10);

        while is_letter(self.ch) {
            ident.push(self.ch);
            self.read_char();
        }

        ident
    }

    fn read_int(&mut self) -> String {
        let mut int = String::with_capacity(5);

        while is_digit(self.ch) {
            int.push(self.ch);
            self.read_char();
        }

        int
    }

    fn read_string(&mut self) -> String {
        let mut result = String::with_capacity(20);

        // Advance from `"` char.
        self.read_char();

        while self.ch != '"' && self.ch != '\0' {
            result.push(self.ch);
            self.read_char();
        }

        result
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespaces();

        let ch = self.ch;

        let token = match ch {
            '=' => {
                if Some('=') == self.peek_next() {
                    self.read_char();
                    Some(Token::Equal)
                } else {
                    Some(Token::Assign)
                }
            }
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            '*' => Some(Token::Asterisk),
            '/' => Some(Token::Slash),
            '!' => {
                if Some('=') == self.peek_next() {
                    self.read_char();
                    Some(Token::NotEqual)
                } else {
                    Some(Token::Bang)
                }
            }
            '<' => Some(Token::LesserThan),
            '>' => Some(Token::GreaterThan),
            ';' => Some(Token::Semicolon),
            '(' => Some(Token::LParen),
            ')' => Some(Token::RParen),
            ',' => Some(Token::Comma),
            '{' => Some(Token::LBrace),
            '}' => Some(Token::RBrace),
            '"' => {
                let s = self.read_string();
                Some(Token::Str(s))
            }
            '\0' => None,
            _ => {
                if is_letter(ch) {
                    let ident = self.read_identifier();

                    return Some(Token::lookup_ident(&ident));
                } else if is_digit(ch) {
                    let int = self.read_int();

                    return Some(Token::Int(int));
                }

                Some(Token::Illegal)
            }
        };

        self.read_char();
        token
    }
}

#[inline]
fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

#[inline]
fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

#[cfg(test)]
mod lexer_test {
    use super::Lexer;
    use crate::token::Token;

    #[test]
    fn test_next_token_single_char_token() {
        const INPUT: &str = "=+(){},;";

        let expected_tokens: Vec<Token> = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
        ];

        test_lexer(INPUT.to_owned(), expected_tokens);
    }

    #[test]
    fn test_next_token_multiple_char_token() {
        const INPUT: &str = "let five = 5;\
        let ten = 10;\
        let add = fn(x, y) {\
        x + y;\
        };\
        let result = add(five, ten);";

        let expected_tokens: Vec<Token> = vec![
            Token::Let,
            Token::Ident("five".to_owned()),
            Token::Assign,
            Token::Int("5".to_owned()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_owned()),
            Token::Assign,
            Token::Int("10".to_owned()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_owned()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_owned()),
            Token::Comma,
            Token::Ident("y".to_owned()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_owned()),
            Token::Plus,
            Token::Ident("y".to_owned()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_owned()),
            Token::Assign,
            Token::Ident("add".to_owned()),
            Token::LParen,
            Token::Ident("five".to_owned()),
            Token::Comma,
            Token::Ident("ten".to_owned()),
            Token::RParen,
            Token::Semicolon,
        ];

        test_lexer(INPUT.to_owned(), expected_tokens);
    }

    #[test]
    fn test_next_token_single_char_token_2() {
        const INPUT: &str = "!-/*5;\
        5 < 10 > 5;";

        let expected_tokens: Vec<Token> = vec![
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".to_owned()),
            Token::Semicolon,
            Token::Int("5".to_owned()),
            Token::LesserThan,
            Token::Int("10".to_owned()),
            Token::GreaterThan,
            Token::Int("5".to_owned()),
            Token::Semicolon,
        ];

        test_lexer(INPUT.to_owned(), expected_tokens);
    }

    #[test]
    fn test_next_token_multiple_char_token_2() {
        const INPUT: &str = "if (5 < 10) {\
            return true;\
        } else {\
            return false;\
        }";

        let expected_tokens: Vec<Token> = vec![
            Token::If,
            Token::LParen,
            Token::Int("5".to_owned()),
            Token::LesserThan,
            Token::Int("10".to_owned()),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
        ];

        test_lexer(INPUT.to_owned(), expected_tokens);
    }

    #[test]
    fn test_next_token_assign_or_equals() {
        const INPUT: &str = "10 == 10;\
        10 != 5;";

        let expected_tokens: Vec<Token> = vec![
            Token::Int("10".to_owned()),
            Token::Equal,
            Token::Int("10".to_owned()),
            Token::Semicolon,
            Token::Int("10".to_owned()),
            Token::NotEqual,
            Token::Int("5".to_owned()),
            Token::Semicolon,
        ];

        test_lexer(INPUT.to_owned(), expected_tokens);
    }

    #[test]
    fn test_next_token() {
        const INPUT: &str = r#"
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            "foobar"
            "foo bar"
        "#;

        let expected_tokens: Vec<Token> = vec![
            Token::Let,
            Token::Ident("five".to_owned()),
            Token::Assign,
            Token::Int("5".to_owned()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_owned()),
            Token::Assign,
            Token::Int("10".to_owned()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_owned()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_owned()),
            Token::Comma,
            Token::Ident("y".to_owned()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_owned()),
            Token::Plus,
            Token::Ident("y".to_owned()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_owned()),
            Token::Assign,
            Token::Ident("add".to_owned()),
            Token::LParen,
            Token::Ident("five".to_owned()),
            Token::Comma,
            Token::Ident("ten".to_owned()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".to_owned()),
            Token::Semicolon,
            Token::Int("5".to_owned()),
            Token::LesserThan,
            Token::Int("10".to_owned()),
            Token::GreaterThan,
            Token::Int("5".to_owned()),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int("5".to_owned()),
            Token::LesserThan,
            Token::Int("10".to_owned()),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int("10".to_owned()),
            Token::Equal,
            Token::Int("10".to_owned()),
            Token::Semicolon,
            Token::Int("10".to_owned()),
            Token::NotEqual,
            Token::Int("9".to_owned()),
            Token::Semicolon,
            Token::Str("foobar".to_owned()),
            Token::Str("foo bar".to_owned()),
        ];

        test_lexer(INPUT.to_owned(), expected_tokens);
    }

    fn test_lexer(input: String, expected_tokens: Vec<Token>) {
        let mut lexer = Lexer::new(input.to_owned());

        for (i, expected_token) in expected_tokens.into_iter().enumerate() {
            let token = lexer.next();

            assert_eq!(token.unwrap(), expected_token, "Failed at index: {i}.");
        }
    }
}
