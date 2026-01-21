use std::{fmt::Display, vec::IntoIter};

use crate::token::Token;

pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }

    pub fn statements_len(&self) -> usize {
        self.statements.len()
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    pub fn into_iter(self) -> IntoIter<Statement> {
        self.statements.into_iter()
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(ToString::to_string)
                .collect::<String>()
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let {
        name: Token,
        value: Box<Expression>,
    },
    Return {
        return_value: Box<Expression>,
    },
    /// This is to be able to expressions as statements.
    Expression(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let { name, value } => {
                write!(f, "let {name} = {value};")
            }
            Self::Return { return_value } => {
                write!(f, "return {return_value};")
            }
            Self::Expression(expression) => {
                write!(f, "{expression};")
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    None, // TODO: remove at the end.
    /// A `Token::Ident` can produce a value when at the right of a `Token::Assign`.
    Identifier(Token),
    If {
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Box<Statement>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "NONE"),
            Self::Identifier(token) => write!(f, "{token}"),
            Self::If {
                condition: _,
                consequence: _,
                alternative: _,
            } => {
                write!(f, "")
            }
        }
    }
}

#[cfg(test)]
mod ast_tests {
    use super::*;

    #[test]
    fn test_to_string() {
        let program = Program {
            statements: vec![Statement::Let {
                name: Token::Ident("myVar".to_owned()),
                value: Box::new(Expression::Identifier(Token::Ident(
                    "anotherVar".to_owned(),
                ))),
            }],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }
}
