use std::collections::HashMap;

use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::{Token, TokenType},
};

type PrefixParseFn = fn(parser: &mut Parser) -> Expression;
type InfixParseFn = fn(parser: &mut Parser, left_side: Expression) -> Expression;

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

pub struct Parser {
    lexer: Lexer,
    errors: Vec<String>,

    cur_token: Option<Token>,
    peek_token: Option<Token>,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

fn parse_identifier_2(parser: &mut Parser) -> Expression {
    Expression::Identifier(parser.cur_token.clone().unwrap())
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let prefix_parse_fns: HashMap<TokenType, PrefixParseFn> = [(
            Token::Ident(String::new()).get_type(),
            Self::parse_identifier as PrefixParseFn,
        )]
        .into();

        let mut parser = Self {
            lexer,
            errors: Vec::new(),
            cur_token: None,
            peek_token: None,
            prefix_parse_fns,
            infix_parse_fns: HashMap::new(),
        };

        // Read two tokens to initialize fields.
        parser.next_token();
        parser.next_token();

        parser
    }

    fn get_errors(&self) -> &[String] {
        &self.errors
    }

    fn register_prefix(&mut self, token_type: TokenType, prefix_parse_fn: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, prefix_parse_fn);
    }

    fn register_infix(&mut self, token_type: TokenType, infix_parse_fn: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, infix_parse_fn);
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

        let name = self.cur_token.clone().unwrap();

        if !matches!(self.peek_token, Some(Token::Assign)) {
            self.peek_errors("Token::Assign");
            return Err(());
        }
        self.next_token();

        // Skip to end of expression.
        while !matches!(self.peek_token, Some(Token::Semicolon)) {
            self.next_token();
        }

        Ok(Statement::Let {
            name,
            value: Box::new(Expression::None),
        })
    }

    #[allow(clippy::unnecessary_wraps)]
    fn parse_return_statement(&mut self) -> Result<Statement, ()> {
        self.next_token();

        // Skip to end of expression.
        while !matches!(self.peek_token, Some(Token::Semicolon)) {
            self.next_token();
        }

        Ok(Statement::Return {
            return_value: Box::new(Expression::None),
        })
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ()> {
        let expression = self.parse_expression(Precedence::Lowest);

        // We ignore `Token::Semicolon`, they are not required.
        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expression(expression?))
    }

    fn parse_expression(&mut self, _precedence: Precedence) -> Result<Expression, ()> {
        let Some(prefix) = self
            .prefix_parse_fns
            .get(&self.cur_token.as_ref().map(Token::get_type).unwrap())
        else {
            return Err(());
        };

        let left_expression = prefix(self);

        Ok(left_expression)
    }

    /// Should not be called if `Self::cur_token` is `Option::None`.
    fn parse_identifier(&mut self) -> Expression {
        Expression::Identifier(self.cur_token.clone().unwrap())
    }

    fn peek_errors(&mut self, expected_token: &str) {
        let msg = format!(
            "Expected next token to be '{expected_token}', got '{:?}' instead.",
            &self.peek_token
        );
        self.errors.push(msg);
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
                name: Token::Ident("x".to_owned()),
                value: Box::new(Expression::None),
            },
            Statement::Let {
                name: Token::Ident("y".to_owned()),
                value: Box::new(Expression::None),
            },
            Statement::Let {
                name: Token::Ident("foo_bar".to_owned()),
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

        assert_eq!(identifier, Token::Ident("foobar".to_owned()));
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

    fn check_parser_errors(errors: &[String]) {
        assert!(errors.is_empty(), "\t{}", errors.join("\n\t"));
    }
}
