use std::{collections::HashMap, mem::discriminant};

use log_call_macro::log_call;

use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::{Token, TokenType},
};

type PrefixParseFn = fn(parser: &mut Parser) -> Result<Expression, ()>;
type InfixParseFn = fn(parser: &mut Parser, left_side: Expression) -> Result<Expression, ()>;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    fn from_token(token: &Token) -> Self {
        match token {
            Token::Equal | Token::NotEqual => Self::Equals,
            Token::LesserThan | Token::GreaterThan => Self::LessGreater,
            Token::Plus | Token::Minus => Self::Sum,
            Token::Asterisk | Token::Slash => Self::Product,
            _ => Self::Lowest,
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    errors: Vec<String>,

    cur_token: Option<Token>,
    peek_token: Option<Token>,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        // TODO: move to a function that uses a match.
        let prefix_parse_fns: HashMap<TokenType, PrefixParseFn> = [
            (
                discriminant(&Token::empty_ident()),
                Self::parse_identifier as PrefixParseFn,
            ),
            (
                discriminant(&Token::empty_int()),
                Self::parse_integer_literal as PrefixParseFn,
            ),
            (
                discriminant(&Token::Bang),
                Self::parse_prefix_expression as PrefixParseFn,
            ),
            (
                discriminant(&Token::Minus),
                Self::parse_prefix_expression as PrefixParseFn,
            ),
        ]
        .into();

        // TODO: move to a function that uses a match.
        let infix_parse_fns: HashMap<TokenType, InfixParseFn> = [
            (
                discriminant(&Token::Plus),
                Self::parse_infix_expression as InfixParseFn,
            ),
            (
                discriminant(&Token::Minus),
                Self::parse_infix_expression as InfixParseFn,
            ),
            (
                discriminant(&Token::Slash),
                Self::parse_infix_expression as InfixParseFn,
            ),
            (
                discriminant(&Token::Asterisk),
                Self::parse_infix_expression as InfixParseFn,
            ),
            (
                discriminant(&Token::Equal),
                Self::parse_infix_expression as InfixParseFn,
            ),
            (
                discriminant(&Token::NotEqual),
                Self::parse_infix_expression as InfixParseFn,
            ),
            (
                discriminant(&Token::LesserThan),
                Self::parse_infix_expression as InfixParseFn,
            ),
            (
                discriminant(&Token::GreaterThan),
                Self::parse_infix_expression as InfixParseFn,
            ),
        ]
        .into();

        let mut parser = Self {
            lexer,
            errors: Vec::new(),
            cur_token: None,
            peek_token: None,
            prefix_parse_fns,
            infix_parse_fns,
        };

        // Read two tokens to initialize fields.
        parser.next_token();
        parser.next_token();

        parser
    }

    fn get_errors(&self) -> &[String] {
        &self.errors
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token.is_some() {
            if let Ok(statement) = self.parse_statement() {
                program.add_statement(statement);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Statement, ()> {
        // TODO: @refactor; This function is called only when `self.cur_token` == `Option::Some`.
        let Some(cur_token) = self.cur_token.clone() else {
            self.errors.push("No token to parse.".to_owned());
            return Err(());
        };

        match cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ()> {
        if !matches!(self.peek_token, Some(Token::Ident(_))) {
            self.peek_errors("Token::Ident");
            return Err(());
        }
        self.next_token();

        let Token::Ident(name) = self.cur_token.clone().unwrap() else {
            panic!("Not an `Token::Ident`.")
        };

        if !matches!(self.peek_token, Some(Token::Assign)) {
            self.peek_errors("Token::Assign");
            return Err(());
        }
        self.next_token();

        // Skip to end of expression.
        while !matches!(self.peek_token, Some(Token::Semicolon)) {
            self.next_token();
        }

        // Skip `Token::Semicolon`.
        while matches!(self.peek_token, Some(Token::Semicolon)) {
            self.next_token();
        }

        Ok(Statement::Let {
            name,
            value: Box::new(Expression::None),
        })
    }

    #[allow(clippy::unnecessary_wraps)] // TODO: remove once it returns expression.
    fn parse_return_statement(&mut self) -> Result<Statement, ()> {
        self.next_token();

        // Skip to end of expression.
        while !matches!(self.peek_token, Some(Token::Semicolon)) {
            self.next_token();
        }

        // Skip `Token::Semicolon`.
        while matches!(self.peek_token, Some(Token::Semicolon)) {
            self.next_token();
        }

        Ok(Statement::Return {
            return_value: Box::new(Expression::None),
        })
    }

    #[log_call]
    fn parse_expression_statement(&mut self) -> Result<Statement, ()> {
        let expression = self.parse_expression(Precedence::Lowest);

        // We ignore `Token::Semicolon`, they are not required.
        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expression(expression?))
    }

    #[log_call]
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ()> {
        let Some(prefix) = self
            .prefix_parse_fns
            .get(&self.cur_token.as_ref().map(discriminant).unwrap())
        else {
            self.no_prefix_parse_fn_error(&self.cur_token.clone().unwrap());
            return Err(());
        };

        let mut left_expression = prefix(self)?;

        while !matches!(self.peek_token, Some(Token::Semicolon))
            && precedence < self.peek_token_precedence()
        {
            let Some(peek_token) = self.peek_token.as_ref().map(discriminant) else {
                return Ok(left_expression);
            };

            if !self.infix_parse_fns.contains_key(&peek_token) {
                return Ok(left_expression);
            }

            self.next_token();

            let infix = self.infix_parse_fns.get(&peek_token).unwrap();

            left_expression = infix(self, left_expression)?;
        }

        Ok(left_expression)
    }

    fn parse_identifier(&mut self) -> Result<Expression, ()> {
        // TODO: @refactor; This function is called only on `Token::Ident`.
        let Token::Ident(name) = self.cur_token.clone().unwrap() else {
            self.errors.push(format!(
                "Not an `Token::Ident`, got '{:?}'.",
                self.cur_token
            ));
            return Err(());
        };
        Ok(Expression::Identifier(name))
    }

    #[log_call]
    fn parse_integer_literal(&mut self) -> Result<Expression, ()> {
        let Token::Int(value) = self.cur_token.clone().unwrap() else {
            self.errors
                .push(format!("Not an `Token::Int`, got '{:?}'.", self.cur_token));
            return Err(());
        };

        let Ok(value) = value.parse::<i64>() else {
            self.errors
                .push(format!("Could no parse as `i64`, got '{value:?}'."));
            return Err(());
        };

        Ok(Expression::Integer(value))
    }

    #[log_call]
    fn parse_prefix_expression(&mut self) -> Result<Expression, ()> {
        let token = self.cur_token.as_ref().unwrap();
        let operator = match token {
            Token::Bang | Token::Minus => token.to_string(),
            _ => return Err(()),
        };

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix {
            operator,
            right: Box::new(right),
        })
    }

    #[log_call]
    fn parse_infix_expression(&mut self, left_expression: Expression) -> Result<Expression, ()> {
        let precedence = Precedence::from_token(self.cur_token.as_ref().unwrap());
        let operator = self.cur_token.as_ref().unwrap().to_string();

        self.next_token();

        let right_expression = self.parse_expression(precedence)?;
        Ok(Expression::Infix {
            left: Box::new(left_expression),
            operator,
            right: Box::new(right_expression),
        })
    }

    fn peek_token_precedence(&self) -> Precedence {
        self.peek_token
            .as_ref()
            .map_or(Precedence::Lowest, Precedence::from_token)
    }

    fn peek_errors(&mut self, expected_token: &str) {
        let msg = format!(
            "Expected next token to be '{expected_token}', got '{:?}' instead.",
            &self.peek_token
        );
        self.errors.push(msg);
    }

    fn no_prefix_parse_fn_error(&mut self, token: &Token) {
        self.errors
            .push(format!("No prefix parse function found for '{token}'."));
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::ast::{Expression, Statement};

    #[test]
    fn test_parse_let_statements() {
        const INPUT: &str = "let x = 5;\
        let y = 10;\
        let foo_bar = 838383;";

        let expected_statements = vec![
            Statement::Let {
                name: "x".to_owned(),
                value: Box::new(Expression::None),
            },
            Statement::Let {
                name: "y".to_owned(),
                value: Box::new(Expression::None),
            },
            Statement::Let {
                name: "foo_bar".to_owned(),
                value: Box::new(Expression::None),
            },
        ];

        test_parser(INPUT.to_owned(), expected_statements);
    }

    #[test]
    fn test_parse_return_statement() {
        const INPUT: &str = "return 5;\
        return 10;\
        return 993322;";

        let expected_statements = vec![
            Statement::Return {
                return_value: Box::new(Expression::None),
            },
            Statement::Return {
                return_value: Box::new(Expression::None),
            },
            Statement::Return {
                return_value: Box::new(Expression::None),
            },
        ];

        test_parser(INPUT.to_owned(), expected_statements);
    }

    fn test_parser(input: String, expected_statements: Vec<Statement>) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), expected_statements.len());

        let program_iter = program.into_iter();

        let expected_statement_iter = expected_statements.into_iter();
        let joined_iter = program_iter.zip(expected_statement_iter);
        for (i, (statement, expected_statement)) in joined_iter.enumerate() {
            assert_eq!(statement, expected_statement, "Invalid input at: {i}.");
        }
    }

    #[test]
    fn test_identifier_expression() {
        const INPUT: &str = "foobar;";

        let lexer = Lexer::new(INPUT.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), 1);

        let statement = program.into_iter().next().unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
        };

        let Expression::Identifier(identifier) = expression else {
            panic!("expression is not `Expression::Identifier`, got: '{expression:?}'.")
        };

        assert_eq!(identifier, "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        const INPUT: &str = "5;";

        let lexer = Lexer::new(INPUT.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), 1);

        let statement = program.into_iter().next().unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
        };

        let Expression::Integer(integer) = expression else {
            panic!("expression is not `Expression::Integer`, got: '{expression:?}'.")
        };

        assert_eq!(integer, 5);
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        const PREFIX_TESTS: [(&str, &str, i64); 2] = [("!5;", "!", 5), ("-15;", "-", 15)];

        for (input, expected_operator, expected_integer_value) in PREFIX_TESTS {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(parser.get_errors());

            assert_eq!(program.statements_len(), 1);

            let statement = program.into_iter().next().unwrap();
            let Statement::Expression(expression) = statement else {
                panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
            };

            let Expression::Prefix { operator, right } = expression else {
                panic!("expression is not `Expression::Prefix`, got: '{expression:?}'.")
            };

            assert_eq!(operator, expected_operator);
            test_integer_literal(right, expected_integer_value);
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        const INFIX_TESTS: [(&str, i64, &str, i64); 8] = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, expected_left, expected_operator, expected_right) in INFIX_TESTS {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(parser.get_errors());

            assert_eq!(program.statements_len(), 1);

            let statement = program.into_iter().next().unwrap();
            let Statement::Expression(expression) = statement else {
                panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
            };

            let Expression::Infix {
                left,
                operator,
                right,
            } = expression
            else {
                panic!("expression is not `Expression::Infix`, got: '{expression:?}'.")
            };

            test_integer_literal(left, expected_left);
            assert_eq!(operator, expected_operator);
            test_integer_literal(right, expected_right);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        const TEST: [(&str, &str); 13] = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (input, expected) in TEST {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(parser.get_errors());

            assert_eq!(program.to_string(), expected);
        }
    }

    fn test_integer_literal(expression: Box<Expression>, expected_integer_value: i64) {
        let Expression::Integer(integer) = *expression else {
            panic!("expression in not `Expression::Integer`, got: '{expression:?}'.")
        };

        assert_eq!(integer, expected_integer_value);
    }

    fn check_parser_errors(errors: &[String]) {
        assert!(errors.is_empty(), "\t{}", errors.join("\n\t"));
    }

    #[test]
    fn test_precedence_order() {
        assert_eq!(Precedence::Lowest, Precedence::Lowest);
        assert!(Precedence::Lowest < Precedence::Equals);
        assert!(Precedence::Equals < Precedence::LessGreater);
        assert!(Precedence::LessGreater < Precedence::Sum);
        assert!(Precedence::Sum < Precedence::Product);
        assert!(Precedence::Product < Precedence::Prefix);
        assert!(Precedence::Prefix < Precedence::Call);
    }
}
