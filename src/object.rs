use crate::{
    ast::{Expression, Statement},
    environment::SharedEnv,
};

#[derive(Debug, Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    Function {
        parameters: Vec<Expression>,
        body: Statement,
        env: SharedEnv,
    },
    ReturnValue(Box<Object>),
    Error(String),
}

impl Object {
    pub const fn null_type_str() -> &'static str {
        "Null"
    }

    pub const fn integer_type_str() -> &'static str {
        "Integer"
    }

    pub const fn boolean_type_str() -> &'static str {
        "Boolean"
    }

    pub fn get_type(&self) -> String {
        match self {
            Self::Null => String::from(Self::null_type_str()),
            Self::Integer(_) => String::from(Self::integer_type_str()),
            Self::Boolean(_) => String::from(Self::boolean_type_str()),
            Self::Function { .. } => String::from("Function"),
            Self::ReturnValue(_) => String::from("ReturnValue"),
            Self::Error(_) => String::from("Error"),
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Self::Null => String::from("null"),
            Self::Integer(i) => format!("{i}"),
            Self::Boolean(b) => format!("{b}"),
            Self::Function {
                parameters,
                body,
                env: _,
            } => {
                let parameters = parameters
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("fn({parameters}) {{\n\t{body}\n}}")
            }
            Self::ReturnValue(value) => value.inspect(),
            Self::Error(s) => format!("ERROR: {s}"),
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Self::Null)
    }
}
