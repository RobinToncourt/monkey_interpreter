use std::mem::discriminant;

use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::Token,
};

type PrefixParseFn = fn(parser: &mut Parser) -> Result<Expression, ()>;
type InfixParseFn =
    fn(parser: &mut Parser, previous_expression: Expression) -> Result<Expression, ()>;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Dot,
    Call,
    Index,
    Assign,
}

impl Precedence {
    fn from_token(token: &Token) -> Self {
        match token {
            Token::Equal | Token::NotEqual => Self::Equals,
            Token::LesserThan | Token::GreaterThan => Self::LessGreater,
            Token::Plus | Token::Minus => Self::Sum,
            Token::Asterisk | Token::Slash => Self::Product,
            Token::Dot => Self::Dot,
            Token::LParen => Self::Call,
            Token::LBracket => Self::Index,
            Token::Assign => Self::Assign,
            _ => Self::Lowest,
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    errors: Vec<String>,

    cur_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            errors: Vec::new(),
            cur_token: None,
            peek_token: None,
        };

        // Read two tokens to initialize fields.
        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn get_errors(&self) -> &[String] {
        &self.errors
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
            Token::For => self.parse_for_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ()> {
        if !self.expect_peek(&Token::empty_ident()) {
            return Err(());
        }

        let Token::Ident(name) = self.cur_token.clone().unwrap() else {
            panic!("Not an `Token::Ident`.")
        };

        if !self.expect_peek(&Token::Assign) {
            return Err(());
        }

        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Let {
            name,
            expression: Box::new(expression?),
        })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ()> {
        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Return {
            return_expression: Box::new(return_value?),
        })
    }

    fn parse_for_statement(&mut self) -> Result<Statement, ()> {
        if !self.expect_peek(&Token::LParen) {
            return Err(());
        }

        self.next_token();

        let initializer = self.parse_statement();

        if self.cur_token != Some(Token::Semicolon) {
            return Err(());
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest);

        self.next_token();

        if self.cur_token != Some(Token::Semicolon) {
            return Err(());
        }

        self.next_token();

        let step = self.parse_statement();

        self.next_token();

        if self.cur_token != Some(Token::RParen) {
            return Err(());
        }

        if !self.expect_peek(&Token::LBrace) {
            return Err(());
        }

        let body = self.parse_block_statement();

        Ok(Statement::For {
            initializer: Box::new(initializer?),
            condition: Box::new(condition?),
            step: Box::new(step?),
            body: Box::new(body?),
        })
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ()> {
        let expression = self.parse_expression(Precedence::Lowest);

        // We ignore `Token::Semicolon`, they are not required.
        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expression(Box::new(expression?)))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ()> {
        // TODO: handle case where `Self::cur_token` is `Option::None`,
        // TODO: can append when there is no `Token::Semicolon` at the end.
        let Some(prefix) = Self::get_prefix_parse_function(self.cur_token.as_ref().unwrap()) else {
            self.no_prefix_parse_fn_error(&self.cur_token.clone().unwrap());
            return Err(());
        };

        let mut left_expression = prefix(self)?;

        while self.peek_token != Some(Token::Semicolon) && precedence < self.peek_token_precedence()
        {
            let Some(infix) = Self::get_infix_parse_function(self.peek_token.as_ref().unwrap())
            else {
                return Ok(left_expression);
            };

            self.next_token();

            left_expression = infix(self, left_expression)?;
        }

        Ok(left_expression)
    }

    fn get_prefix_parse_function(token: &Token) -> Option<PrefixParseFn> {
        match token {
            Token::Ident(_) => Some(Self::parse_identifier as PrefixParseFn),
            Token::Int(_) => Some(Self::parse_integer_literal as PrefixParseFn),
            Token::Str(_) => Some(Self::parse_string_literal as PrefixParseFn),
            Token::Bang | Token::Minus => Some(Self::parse_prefix_expression as PrefixParseFn),
            Token::True | Token::False => Some(Self::parse_boolean as PrefixParseFn),
            Token::LParen => Some(Self::parse_grouped_expression as PrefixParseFn),
            Token::LBracket => Some(Self::parse_array_literal as PrefixParseFn),
            Token::LBrace => Some(Self::parse_hash_literal as PrefixParseFn),
            Token::If => Some(Self::parse_if_expression as PrefixParseFn),
            Token::Function => Some(Self::parse_function_literal as PrefixParseFn),
            _ => None,
        }
    }

    fn get_infix_parse_function(token: &Token) -> Option<InfixParseFn> {
        match token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Equal
            | Token::NotEqual
            | Token::LesserThan
            | Token::GreaterThan => Some(Self::parse_infix_expression as InfixParseFn),
            Token::Dot => Some(Self::parse_float_literal as InfixParseFn),
            Token::LParen => Some(Self::parse_call_expression as InfixParseFn),
            Token::LBracket => Some(Self::parse_index_expression as InfixParseFn),
            Token::Assign => Some(Self::parse_assign_expression as InfixParseFn),
            _ => None,
        }
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

    fn parse_boolean(&mut self) -> Result<Expression, ()> {
        match &self.cur_token {
            Some(Token::True) => Ok(Expression::Boolean(true)),
            Some(Token::False) => Ok(Expression::Boolean(false)),
            _ => {
                self.errors.push(format!(
                    "Not an `Token::Ident`, got '{:?}'.",
                    self.cur_token
                ));
                Err(())
            }
        }
    }

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

    // `InfixParseFn` signature forces the pass-by-value.
    #[allow(clippy::needless_pass_by_value)]
    fn parse_float_literal(&mut self, left_expression: Expression) -> Result<Expression, ()> {
        let precedence = Precedence::from_token(self.cur_token.as_ref().unwrap());

        self.next_token();

        let right_expression = self.parse_expression(precedence)?;

        let Expression::Integer(left_integer) = left_expression else {
            self.errors.push(format!(
                "Not an `Expression::Integer`, got '{left_expression:?}'."
            ));
            return Err(());
        };

        let Expression::Integer(right_integer) = right_expression else {
            self.errors.push(format!(
                "Not an `Expression::Integer`, got '{right_expression:?}'."
            ));
            return Err(());
        };

        // Should always succeed.
        let float = format!("{left_integer}.{right_integer}")
            .parse::<f64>()
            .unwrap();
        Ok(Expression::Float(float))
    }

    fn parse_string_literal(&mut self) -> Result<Expression, ()> {
        let Token::Str(value) = self.cur_token.clone().unwrap() else {
            self.errors
                .push(format!("Not an `Token::Str`, got '{:?}'.", self.cur_token));
            return Err(());
        };

        Ok(Expression::String(value))
    }

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

    fn parse_grouped_expression(&mut self) -> Result<Expression, ()> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(&Token::RParen) {
            return Err(());
        }

        expression
    }

    fn parse_array_literal(&mut self) -> Result<Expression, ()> {
        let elements = self.parse_expression_list(&Token::RBracket)?;
        Ok(Expression::Array(elements))
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ()> {
        if !self.expect_peek(&Token::LParen) {
            return Err(());
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(&Token::RParen) {
            return Err(());
        }

        if !self.expect_peek(&Token::LBrace) {
            return Err(());
        }

        let consequences = self.parse_block_statement();

        let mut alternatives = Option::<Box<Statement>>::None;
        if self.peek_token == Some(Token::Else) {
            self.next_token();

            if !self.expect_peek(&Token::LBrace) {
                return Err(());
            }

            alternatives = Some(Box::new(self.parse_block_statement()?));
        }

        Ok(Expression::If {
            condition: Box::new(condition?),
            consequences: Box::new(consequences?),
            alternatives,
        })
    }

    #[allow(clippy::unnecessary_wraps)] // TODO: remove once it returns expression.
    fn parse_block_statement(&mut self) -> Result<Statement, ()> {
        let mut statements = Vec::<Statement>::new();

        self.next_token();

        while let Some(token) = self.cur_token.as_ref()
            && !matches!(token, Token::RBrace)
        {
            let statement = self.parse_statement();

            // TODO: do not ignore `Result::Err`.
            if let Ok(statement) = statement {
                statements.push(statement);
            }

            self.next_token();
        }

        Ok(Statement::Block(statements))
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ()> {
        if !self.expect_peek(&Token::LParen) {
            return Err(());
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(&Token::LBrace) {
            return Err(());
        }

        let body = self.parse_block_statement()?;

        Ok(Expression::Function {
            parameters: parameters?,
            body: Box::new(body),
        })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Expression>, ()> {
        let mut parameters = Vec::<Expression>::new();

        if self.peek_token == Some(Token::RParen) {
            self.next_token();
            return Ok(parameters);
        }

        self.next_token();

        if let Some(Token::Ident(value)) = self.cur_token.take() {
            let ident = Expression::Identifier(value);
            parameters.push(ident);
        }

        while self.peek_token == Some(Token::Comma) {
            self.next_token();
            self.next_token();

            if let Some(Token::Ident(value)) = self.cur_token.take() {
                let ident = Expression::Identifier(value);
                parameters.push(ident);
            }
        }

        if !self.expect_peek(&Token::RParen) {
            return Err(());
        }

        Ok(parameters)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ()> {
        let arguments = self.parse_expression_list(&Token::RParen)?;
        Ok(Expression::Call {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, ()> {
        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&Token::RBracket) {
            return Err(());
        }

        Ok(Expression::Indexing {
            left: Box::new(left),
            index: Box::new(index),
        })
    }

    fn parse_assign_expression(&mut self, identifier: Expression) -> Result<Expression, ()> {
        self.next_token();

        let Expression::Identifier(identifier) = identifier else {
            self.errors.push(format!(
                "Not an `Expression::Identifier`, got '{identifier:?}'."
            ));
            return Err(());
        };

        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Ok(Expression::Assign {
            name: identifier,
            expression: Box::new(expression?),
        })
    }

    fn parse_expression_list(&mut self, end_token: &Token) -> Result<Vec<Expression>, ()> {
        let mut result = Vec::<Expression>::new();

        if self.peek_token == Some(end_token.clone()) {
            self.next_token();
            return Ok(result);
        }

        self.next_token();
        result.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token == Some(Token::Comma) {
            self.next_token();
            self.next_token();
            result.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.expect_peek(end_token) {
            return Err(());
        }

        Ok(result)
    }

    fn parse_hash_literal(&mut self) -> Result<Expression, ()> {
        let mut pairs = Vec::<(Expression, Expression)>::new();

        while self.peek_token != Some(Token::RBrace) {
            self.next_token();

            let key = self.parse_expression(Precedence::Lowest)?;

            if !self.expect_peek(&Token::Colon) {
                return Err(());
            }

            self.next_token();

            let value = self.parse_expression(Precedence::Lowest)?;

            pairs.push((key, value));

            if self.peek_token != Some(Token::RBrace) && !self.expect_peek(&Token::Comma) {
                return Err(());
            }
        }

        if !self.expect_peek(&Token::RBrace) {
            return Err(());
        }

        Ok(Expression::Hash(pairs))
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }

    fn peek_token_precedence(&self) -> Precedence {
        self.peek_token
            .as_ref()
            .map_or(Precedence::Lowest, Precedence::from_token)
    }

    /// Returns if the `Self::peek_token` == `token`,
    /// advances tokens if true.
    fn expect_peek(&mut self, token: &Token) -> bool {
        if let Some(peek) = self.peek_token.as_ref()
            && discriminant(peek) == discriminant(token)
        {
            self.next_token();
            true
        } else {
            self.peek_errors(&token.to_string());
            false
        }
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
            .push(format!("No prefix parse function found for '{token:?}'."));
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::ast::{Expression, Statement};
    use std::any::Any;

    #[test]
    fn test_parse_let_statements() {
        const INPUT: &str = "let x = 5;\
        let y = 10;\
        let foo_bar = 838383;\
        let float = 4.5;";

        let expected_lets: Vec<(&str, Box<dyn Any>)> = vec![
            ("x", Box::new(5_i64)),
            ("y", Box::new(10_i64)),
            ("foo_bar", Box::new(838383_i64)),
            ("float", Box::new(4.5_f64)),
        ];

        let lexer = Lexer::new(INPUT.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), expected_lets.len());

        let program_iter = program.into_iter();

        let expected_statement_iter = expected_lets.into_iter();
        let joined_iter = program_iter.zip(expected_statement_iter);
        for (statement, (expected_name, expected_expression)) in joined_iter {
            if let Statement::Let { name, expression } = statement {
                assert_eq!(name, expected_name);
                test_literal_expression(expression, expected_expression);
            }
        }
    }

    #[test]
    fn test_parse_let_statements_2() {
        let tests: [(&str, &str, Box<dyn Any>); 3] = [
            ("let x = 5;", "x", Box::new(5_i64)),
            ("let y = true;", "y", Box::new(true)),
            ("let foobar = y;", "foobar", Box::new("y")),
        ];

        for (input, expected_identifier, expected_value) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(parser.get_errors());

            assert_eq!(program.statements_len(), 1);

            let statement = program.into_iter().next().unwrap();
            test_let_statements(statement, expected_identifier, expected_value);
        }
    }

    fn test_let_statements(
        statement: Statement,
        expected_identifier: &str,
        expected_value: Box<dyn Any>,
    ) {
        let Statement::Let { name, expression } = statement else {
            panic!("statement in not `Statement::Let`, got: '{statement:?}'.")
        };

        assert_eq!(name, expected_identifier);
        test_literal_expression(expression, expected_value);
    }

    #[test]
    fn test_parse_return_statement() {
        const INPUT: &str = "return 5;\
        return 10;\
        return 993322;\
        return 42.0";

        let expected_returns: Vec<Box<dyn Any>> = vec![
            Box::new(5_i64),
            Box::new(10_i64),
            Box::new(993322_i64),
            Box::new(42.0_f64),
        ];

        let lexer = Lexer::new(INPUT.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), expected_returns.len());

        let program_iter = program.into_iter();

        let expected_statement_iter = expected_returns.into_iter();
        let joined_iter = program_iter.zip(expected_statement_iter);
        for (statement, expected_return) in joined_iter {
            if let Statement::Return { return_expression } = statement {
                test_literal_expression(return_expression, expected_return);
            }
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

        let Expression::Identifier(identifier) = *expression else {
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

        let Expression::Integer(integer) = *expression else {
            panic!("expression is not `Expression::Integer`, got: '{expression:?}'.")
        };

        assert_eq!(integer, 5);
    }

    #[test]
    fn test_float_literal_expression() {
        const INPUT: &str = "5.0;";

        let lexer = Lexer::new(INPUT.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), 1);

        let statement = program.into_iter().next().unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
        };

        let Expression::Float(float) = *expression else {
            panic!("expression is not `Expression::Float`, got: '{expression:?}'.")
        };

        assert_eq!(float, 5.0);
    }

    #[test]
    fn test_boolean_expression() {
        const INPUT: &str = "true;";

        let lexer = Lexer::new(INPUT.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), 1);

        let statement = program.into_iter().next().unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
        };

        let Expression::Boolean(boolean) = *expression else {
            panic!("expression is not `Expression::Boolean`, got: '{expression:?}'.")
        };

        assert_eq!(boolean, true);
    }

    #[test]
    fn test_if_expression() {
        const INPUT: &str = "if (x < y) { x }";

        let lexer = Lexer::new(INPUT.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), 1);

        let statement = program.into_iter().next().unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
        };

        let Expression::If {
            condition,
            consequences,
            alternatives,
        } = *expression
        else {
            panic!("expression is not `Expression::If`, got: '{expression:?}'.")
        };

        test_infix_expression(condition, Box::new("x"), "<", Box::new("y"));

        let Statement::Block(consequences) = *consequences else {
            panic!("consequences are not `Statement::Block`, got: '{consequences:?}.")
        };

        assert_eq!(consequences.len(), 1);

        let consequence = consequences.into_iter().next().unwrap();

        let Statement::Expression(expression) = consequence else {
            panic!("consequence is not `Statement::Expression`, got: '{consequence:?}'.")
        };

        test_identifier(expression, "x");

        assert!(alternatives.is_none());
    }

    #[test]
    fn test_if_else_expression() {
        const INPUT: &str = "if (x < y) { x } else { y }";

        let lexer = Lexer::new(INPUT.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), 1);

        let statement = program.into_iter().next().unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
        };

        let Expression::If {
            condition,
            consequences,
            alternatives,
        } = *expression
        else {
            panic!("expression is not `Expression::If`, got: '{expression:?}'.")
        };

        test_infix_expression(condition, Box::new("x"), "<", Box::new("y"));

        let Statement::Block(consequences) = *consequences else {
            panic!("consequences are not `Statement::Block`, got: '{consequences:?}.")
        };

        assert_eq!(consequences.len(), 1);

        let consequence = consequences.into_iter().next().unwrap();

        let Statement::Expression(expression) = consequence else {
            panic!("consequence is not `Statement::Expression`, got: '{consequence:?}'.")
        };

        test_identifier(expression, "x");

        assert!(alternatives.is_some());
        let alternatives = alternatives.unwrap();

        let Statement::Block(alternatives) = *alternatives else {
            panic!("alternatives are not `Statement::Block`, got: '{alternatives:?}.")
        };

        assert_eq!(alternatives.len(), 1);

        let alternatives = alternatives.into_iter().next().unwrap();

        let Statement::Expression(expression) = alternatives else {
            panic!("alternatives is not `Statement::Expression`, got: '{alternatives:?}.")
        };

        test_identifier(expression, "y");
    }

    #[test]
    fn test_function_literal() {
        const INPUT: &str = "fn (x, y) { x + y }";

        let lexer = Lexer::new(INPUT.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), 1);

        let statement = program.into_iter().next().unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
        };

        let Expression::Function { parameters, body } = *expression else {
            panic!("expression is not `Expression::Function`, got: '{expression:?}'.")
        };

        assert_eq!(parameters.len(), 2);

        let mut parameters = parameters.into_iter();
        test_literal_expression(Box::new(parameters.next().unwrap()), Box::new("x"));
        test_literal_expression(Box::new(parameters.next().unwrap()), Box::new("y"));

        let Statement::Block(body) = *body else {
            panic!("body is not `Statement::Block`, got: '{body:?}'.")
        };

        assert_eq!(body.len(), 1);

        let mut body = body.into_iter();
        let Statement::Expression(expression) = body.next().unwrap() else {
            panic!("body[0] is not `Statement::Expression`, got: '{body:?}'.")
        };

        test_infix_expression(expression, Box::new("x"), "+", Box::new("y"));
    }

    #[test]
    fn test_function_parameters_parsing() {
        const INPUT: [(&str, &[&str]); 3] = [
            ("fn () {}", &[]),
            ("fn (x) {}", &["x"]),
            ("fn (x, y, z) {}", &["x", "y", "z"]),
        ];

        for (input, expected_parameters) in INPUT {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(parser.get_errors());

            let statement = program.into_iter().next().unwrap();
            let Statement::Expression(expression) = statement else {
                panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
            };

            let Expression::Function {
                parameters,
                body: _,
            } = *expression
            else {
                panic!("expression is not `Expression::Function`, got: '{expression:?}'.")
            };

            assert_eq!(parameters.len(), expected_parameters.len());

            let zip = parameters.into_iter().zip(expected_parameters);
            for (parameter, expected_parameter) in zip.into_iter() {
                test_literal_expression(Box::new(parameter), Box::new(*expected_parameter));
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        const INPUT: &str = "add(1, 2 * 3, 4 + 5, 6.7);";

        let lexer = Lexer::new(INPUT.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), 1);

        let statement = program.into_iter().next().unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
        };

        let Expression::Call {
            function,
            arguments,
        } = *expression
        else {
            panic!("expression is not `Expression::Call`, got: '{expression:?}'.")
        };

        test_identifier(function, "add");

        assert_eq!(arguments.len(), 4);

        let mut arguments = arguments.into_iter();
        test_literal_expression(Box::new(arguments.next().unwrap()), Box::new(1_i64));
        test_infix_expression(
            Box::new(arguments.next().unwrap()),
            Box::new(2_i64),
            "*",
            Box::new(3_i64),
        );
        test_infix_expression(
            Box::new(arguments.next().unwrap()),
            Box::new(4_i64),
            "+",
            Box::new(5_i64),
        );
        test_literal_expression(Box::new(arguments.next().unwrap()), Box::new(6.7));
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests: [(&str, &str, Box<dyn Any>); 5] = [
            ("!5;", "!", Box::new(5_i64)),
            ("-15;", "-", Box::new(15_i64)),
            ("-4.5;", "-", Box::new(4.5_f64)),
            ("!true;", "!", Box::new(true)),
            ("!false;", "!", Box::new(false)),
        ];

        for (input, expected_operator, expected_integer_value) in prefix_tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(parser.get_errors());

            assert_eq!(program.statements_len(), 1);

            let statement = program.into_iter().next().unwrap();
            let Statement::Expression(expression) = statement else {
                panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
            };

            test_prefix_expression(expression, expected_operator, expected_integer_value);
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests: [(&str, Box<dyn Any>, &str, Box<dyn Any>); 12] = [
            ("5 + 5;", Box::new(5_i64), "+", Box::new(5_i64)),
            ("5 - 5;", Box::new(5_i64), "-", Box::new(5_i64)),
            ("5 * 5;", Box::new(5_i64), "*", Box::new(5_i64)),
            ("5 / 5;", Box::new(5_i64), "/", Box::new(5_i64)),
            ("5 > 5;", Box::new(5_i64), ">", Box::new(5_i64)),
            ("5 < 5;", Box::new(5_i64), "<", Box::new(5_i64)),
            ("5 == 5;", Box::new(5_i64), "==", Box::new(5_i64)),
            ("5 != 5;", Box::new(5_i64), "!=", Box::new(5_i64)),
            ("4.5 != 5.4;", Box::new(4.5_f64), "!=", Box::new(5.4_f64)),
            ("true == true", Box::new(true), "==", Box::new(true)),
            ("true != false", Box::new(true), "!=", Box::new(false)),
            ("false == false", Box::new(false), "==", Box::new(false)),
        ];

        for (input, expected_left, expected_operator, expected_right) in infix_tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(parser.get_errors());

            assert_eq!(program.statements_len(), 1);

            let statement = program.into_iter().next().unwrap();
            let Statement::Expression(expression) = statement else {
                panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
            };

            test_infix_expression(expression, expected_left, expected_operator, expected_right);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests: Vec<(&str, &str)> = vec![
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
            (
                "3.4 + 5.6 * 7.8 == 9.1 * 2.3 + 4.5 * 6.7",
                "((3.4 + (5.6 * 7.8)) == ((9.1 * 2.3) + (4.5 * 6.7)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a, b, 1.1, 2.2 * 3.3, 4.4 + 5.5, add(6.6, 7.7 * 8.8))",
                "add(a, b, 1.1, (2.2 * 3.3), (4.4 + 5.5), add(6.6, (7.7 * 8.8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, expected) in tests.into_iter() {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(parser.get_errors());

            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn test_string_literal_expression() {
        const INPUT: &str = r#""Hello World!";"#;

        let lexer = Lexer::new(INPUT.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), 1);

        let statement = program.into_iter().next().unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
        };

        let Expression::String(value) = *expression else {
            panic!("expression is not `Expression::String`, got: '{expression:?}'.")
        };

        assert_eq!(value, String::from("Hello World!"));
    }

    #[test]
    fn test_parsing_array_literals() {
        const INPUT: &str = "[1, 2 * 2, 3 + 3]";

        let lexer = Lexer::new(INPUT.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), 1);

        let statement = program.into_iter().next().unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
        };

        let Expression::Array(values) = *expression else {
            panic!("expression is not `Expression::Array`, got: '{expression:?}'.")
        };

        assert_eq!(values.len(), 3);

        let mut values = values.into_iter();
        test_integer_literal(Box::new(values.next().unwrap()), 1);
        test_infix_expression(
            Box::new(values.next().unwrap()),
            Box::new(2_i64),
            "*",
            Box::new(2_i64),
        );
        test_infix_expression(
            Box::new(values.next().unwrap()),
            Box::new(3_i64),
            "+",
            Box::new(3_i64),
        );
    }

    #[test]
    fn test_parsing_index_expression() {
        const INPUT: &str = "my_array[1 + 1]";

        let lexer = Lexer::new(INPUT.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        println!("{program}");

        assert_eq!(program.statements_len(), 1);

        let statement = program.into_iter().next().unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
        };

        let Expression::Indexing { left, index } = *expression else {
            panic!("expression is not `Expression::Indexing`, got: '{expression:?}'.")
        };

        test_identifier(left, "my_array");
        test_infix_expression(index, Box::new(1_i64), "+", Box::new(1_i64));
    }

    #[test]
    fn test_parsing_hash_literals_string_keys() {
        let input = r#"{ "one": 1, "two": 2, "three": 3 }"#;

        let expected_pairs: Vec<(&str, Box<dyn Any>)> = vec![
            ("one", Box::new(1_i64)),
            ("two", Box::new(2_i64)),
            ("three", Box::new(3_i64)),
        ];

        let map = check_hash_literal(input);

        assert_eq!(map.len(), expected_pairs.len());

        for ((key, value), (expected_key, expected_value)) in map.into_iter().zip(expected_pairs) {
            test_string_literal(Box::new(key), expected_key);
            test_literal_expression(Box::new(value), expected_value);
        }
    }

    #[test]
    fn test_parsing_hash_literals_integer_keys() {
        let input = r#"{ 1: "one", 2: "two", 3: "three" }"#;

        let expected_pairs: Vec<(i64, &str)> =
            vec![(1_i64, "one"), (2_i64, "two"), (3_i64, "three")];

        let map = check_hash_literal(input);

        assert_eq!(map.len(), expected_pairs.len());

        for ((key, value), (expected_key, expected_value)) in map.into_iter().zip(expected_pairs) {
            test_integer_literal(Box::new(key), expected_key);
            test_string_literal(Box::new(value), expected_value);
        }
    }

    #[test]
    fn test_parsing_hash_literals_boolean_keys() {
        let input = r#"{ true: 1, false: 0 }"#;

        let expected_pairs: Vec<(bool, i64)> = vec![(true, 1), (false, 0)];

        let map = check_hash_literal(input);

        assert_eq!(map.len(), expected_pairs.len());

        for ((key, value), (expected_key, expected_value)) in map.into_iter().zip(expected_pairs) {
            test_boolean_literal(Box::new(key), expected_key);
            test_integer_literal(Box::new(value), expected_value);
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}";

        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), 1);

        let statement = program.into_iter().next().unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
        };

        let Expression::Hash(pairs) = *expression else {
            panic!("expression is not `Expression::Hash`, got: '{expression:?}'.")
        };

        assert_eq!(pairs.len(), 0);
    }

    #[test]
    fn test_parsing_hash_literals_with_expressions() {
        let input = r#"{ "one": 0 + 1, "two": 10 - 8, "three": 15 / 5 }"#;

        let map = check_hash_literal(input);

        assert_eq!(map.len(), 3);

        let expected_infix: Vec<(&str, Box<dyn Any>, &str, Box<dyn Any>)> = vec![
            ("one", Box::new(0_i64), "+", Box::new(1_i64)),
            ("two", Box::new(10_i64), "-", Box::new(8_i64)),
            ("three", Box::new(15_i64), "/", Box::new(5_i64)),
        ];

        for ((key, value), (expected_key, expected_left, expected_operator, expected_right)) in
            map.into_iter().zip(expected_infix)
        {
            test_string_literal(Box::new(key), expected_key);
            test_infix_expression(
                Box::new(value),
                expected_left,
                expected_operator,
                expected_right,
            );
        }
    }

    fn test_prefix_expression(
        expression: Box<Expression>,
        expected_operator: &str,
        expected_right: Box<dyn Any>,
    ) {
        let Expression::Prefix { operator, right } = *expression else {
            panic!("expression is not `Expression::Prefix`, got: '{expression:?}'.")
        };

        assert_eq!(operator, expected_operator);
        test_literal_expression(right, expected_right);
    }

    fn test_infix_expression(
        expression: Box<Expression>,
        expected_left: Box<dyn Any>,
        expected_operator: &str,
        expected_right: Box<dyn Any>,
    ) {
        let Expression::Infix {
            left,
            operator,
            right,
        } = *expression
        else {
            panic!("expression is not `Expression::Infix`, got: '{expression:?}'.")
        };

        test_literal_expression(left, expected_left);
        assert_eq!(operator, expected_operator);
        test_literal_expression(right, expected_right);
    }

    fn check_hash_literal(input: &str) -> Vec<(Expression, Expression)> {
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser.get_errors());

        assert_eq!(program.statements_len(), 1);

        let statement = program.into_iter().next().unwrap();
        let Statement::Expression(expression) = statement else {
            panic!("program.statement[0] is not `Statement::Expression`, got: '{statement:?}'.")
        };

        let Expression::Hash(pairs) = *expression else {
            panic!("expression is not `Expression::Hash`, got: '{expression:?}'.")
        };

        pairs
    }

    fn test_literal_expression(expression: Box<Expression>, expected: Box<dyn Any>) {
        if expected.is::<bool>() {
            test_boolean_literal(expression, *expected.downcast_ref::<bool>().unwrap());
        } else if expected.is::<i64>() {
            test_integer_literal(expression, *expected.downcast_ref::<i64>().unwrap());
        } else if expected.is::<f64>() {
            test_float_literal(expression, *expected.downcast_ref::<f64>().unwrap());
        } else if expected.is::<&str>() {
            test_identifier(expression, expected.downcast_ref::<&str>().unwrap());
        } else {
            panic!("expected type not handled: '{:?}'.", expected.type_id())
        }
    }

    fn test_boolean_literal(expression: Box<Expression>, expected: bool) {
        let Expression::Boolean(boolean) = *expression else {
            panic!("expression is not `Expression::Boolean`, got: '{expression:?}'.")
        };

        assert_eq!(boolean, expected);
    }

    fn test_integer_literal(expression: Box<Expression>, expected_integer_value: i64) {
        let Expression::Integer(integer) = *expression else {
            panic!("expression in not `Expression::Integer`, got: '{expression:?}'.")
        };

        assert_eq!(integer, expected_integer_value);
    }

    fn test_string_literal(expression: Box<Expression>, expected_string_value: &str) {
        let Expression::String(string) = *expression else {
            panic!("expression is not `Expression::String`, got: '{expression:?}'.")
        };

        assert_eq!(string, expected_string_value);
    }

    fn test_float_literal(expression: Box<Expression>, expected_float_value: f64) {
        let Expression::Float(float) = *expression else {
            panic!("expression in not `Expression::Integer`, got: '{expression:?}'.")
        };

        assert_eq!(float, expected_float_value);
    }

    fn test_identifier(expression: Box<Expression>, expected_identifier: &str) {
        let Expression::Identifier(identifier) = *expression else {
            panic!("expression in not `Expression::Identifier`, got: '{expression:?}'.")
        };

        assert_eq!(identifier, expected_identifier);
    }

    fn check_parser_errors(errors: &[String]) {
        assert!(errors.is_empty(), "\t{}", errors.join("\n\t"));
    }
}
