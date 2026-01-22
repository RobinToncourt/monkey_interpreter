use std::{fmt::Display, vec::IntoIter};

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
        name: String,
        value: Box<Expression>,
    },
    Return {
        return_value: Box<Expression>,
    },
    /// This is to be able to use expressions as statements.
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
                write!(f, "{expression}")
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    None, // TODO: remove at the end.
    /// A `Token::Ident` can produce a value when at the right of a `Token::Assign`.
    Identifier(String),
    Integer(i64),
    Prefix {
        operator: String,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "NONE"),
            Self::Identifier(name) => write!(f, "{name}"),
            Self::Integer(value) => write!(f, "{value}"),
            Self::Prefix { operator, right } => write!(f, "({operator}{right})"),
            Self::Infix {
                left,
                operator,
                right,
            } => {
                write!(f, "({left} {operator} {right})")
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
                name: "myVar".to_owned(),
                value: Box::new(Expression::Identifier("anotherVar".to_owned())),
            }],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }
}
