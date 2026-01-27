use std::{fmt::Display, vec::IntoIter};

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl From<Program> for Node {
    fn from(program: Program) -> Self {
        Node::Program(program)
    }
}

impl From<Statement> for Node {
    fn from(statement: Statement) -> Self {
        Node::Statement(statement)
    }
}

impl From<Expression> for Node {
    fn from(expression: Expression) -> Self {
        Node::Expression(expression)
    }
}

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

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
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
        return_expression: Box<Expression>,
    },
    /// This is to be able to use expressions as statements.
    Expression(Box<Expression>),
    Block(Vec<Statement>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let { name, value } => write!(f, "let {name} = {value};"),
            Self::Return { return_expression } => write!(f, "return {return_expression};"),
            Self::Expression(expression) => write!(f, "{expression}"),
            Self::Block(statements) => {
                let output = statements
                    .iter()
                    .map(ToString::to_string)
                    .collect::<String>();
                write!(f, "{output}")
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    /// A `Token::Ident` can produce a value when at the right of a `Token::Assign`.
    Identifier(String),
    Integer(i64),
    Boolean(bool),
    Prefix {
        operator: String,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequences: Box<Statement>,
        alternatives: Option<Box<Statement>>,
    },
    Function {
        parameters: Vec<Expression>,
        body: Box<Statement>,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(name) => write!(f, "{name}"),
            Self::Integer(value) => write!(f, "{value}"),
            Self::Boolean(value) => write!(f, "{value}"),
            Self::Prefix { operator, right } => write!(f, "({operator}{right})"),
            Self::Infix {
                left,
                operator,
                right,
            } => {
                write!(f, "({left} {operator} {right})")
            }
            Self::If {
                condition,
                consequences,
                alternatives,
            } => {
                write!(f, "if{condition} {consequences}")?;

                if let Some(alternative) = alternatives {
                    write!(f, "else {alternative}")?;
                }

                Ok(())
            }
            Self::Function { parameters, body } => {
                let parameters = parameters
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "fn({parameters}){body}")
            }
            Self::Call {
                function,
                arguments,
            } => {
                let args = arguments
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{function}({args})")
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
