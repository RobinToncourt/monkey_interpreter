use std::{
    cell::RefCell,
    collections::HashMap,
    fs::File,
    hash::{DefaultHasher, Hash, Hasher},
    rc::Rc,
};

use crate::{
    ast::{Expression, Statement},
    builtins::BuiltInFunction,
    environment::SharedEnv,
};

#[derive(Debug)]
pub struct NoHashVariant;

// unpredictable_function_pointer_comparisons: BuiltInFunction shouldn't be compared.
#[allow(unpredictable_function_pointer_comparisons)]
#[derive(Debug, Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Array(Vec<Object>),
    Hash(HashMap<u64, (Object, Object)>),
    Function {
        parameters: Vec<Expression>,
        body: Statement,
        env: SharedEnv,
    },
    File(Rc<RefCell<File>>),
    BuiltIn(BuiltInFunction),
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

    pub const fn float_type_str() -> &'static str {
        "Float"
    }

    pub const fn string_type_str() -> &'static str {
        "String"
    }

    pub const fn boolean_type_str() -> &'static str {
        "Boolean"
    }

    pub fn get_type(&self) -> String {
        match self {
            Self::Null => String::from(Self::null_type_str()),
            Self::Integer(_) => String::from(Self::integer_type_str()),
            Self::Float(_) => String::from(Self::float_type_str()),
            Self::String(_) => String::from(Self::string_type_str()),
            Self::Boolean(_) => String::from(Self::boolean_type_str()),
            Self::Array(_) => String::from("Array"),
            Self::Hash(_) => String::from("Hash"),
            Self::Function { .. } => String::from("Function"),
            Self::File(_) => String::from("File"),
            Self::BuiltIn(_) => String::from("BuiltIn"),
            Self::ReturnValue(_) => String::from("ReturnValue"),
            Self::Error(_) => String::from("Error"),
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Self::Null => String::from("null"),
            Self::Integer(i) => format!("{i}"),
            Self::Float(f) => format!("{f}"),
            Self::String(s) => s.clone(),
            Self::Boolean(b) => format!("{b}"),
            Self::Array(elements) => {
                let elements = elements
                    .iter()
                    .map(Self::inspect)
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("[{elements}]")
            }
            Self::Hash(map) => {
                let pairs = map
                    .iter()
                    .map(|(_hash, (k, v))| format!("{}: {}", k.inspect(), v.inspect()))
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("{{{pairs}}}")
            }
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
            Self::File(_) => String::from("File"),
            Self::BuiltIn(_) => String::from("builtin function"),
            Self::ReturnValue(value) => value.inspect(),
            Self::Error(s) => format!("ERROR: {s}"),
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Self::Null)
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error(_))
    }

    pub fn get_hash(&self) -> Result<u64, NoHashVariant> {
        let mut default_hasher = DefaultHasher::new();

        match self {
            Self::Integer(i) => i.hash(&mut default_hasher),
            Self::Float(f) => f.to_bits().hash(&mut default_hasher),
            Self::Boolean(b) => b.hash(&mut default_hasher),
            Self::String(s) => s.hash(&mut default_hasher),
            _ => return Err(NoHashVariant),
        }

        Ok(default_hasher.finish())
    }
}

impl FromIterator<Object> for Object {
    fn from_iter<T: IntoIterator<Item = Object>>(iter: T) -> Self {
        Self::Array(iter.into_iter().collect())
    }
}

#[cfg(test)]
mod object_tests {
    use super::*;

    #[test]
    fn test_string_get_hash() {
        let hello1 = Object::String(String::from("Hello World"));
        let hello2 = Object::String(String::from("Hello World"));
        let diff1 = Object::String(String::from("My name is Johnny"));
        let diff2 = Object::String(String::from("My name is Johnny"));

        assert_eq!(hello1.get_hash().unwrap(), hello2.get_hash().unwrap());
        assert_eq!(diff1.get_hash().unwrap(), diff2.get_hash().unwrap());
        assert_ne!(hello1.get_hash().unwrap(), diff1.get_hash().unwrap());
    }

    #[test]
    fn test_integer_get_hash() {
        let one1 = Object::Integer(1);
        let one2 = Object::Integer(1);
        let diff1 = Object::Integer(42);
        let diff2 = Object::Integer(42);

        assert_eq!(one1.get_hash().unwrap(), one2.get_hash().unwrap());
        assert_eq!(diff1.get_hash().unwrap(), diff2.get_hash().unwrap());
        assert_ne!(one1.get_hash().unwrap(), diff1.get_hash().unwrap());
    }

    #[test]
    fn test_boolean_get_hash() {
        let t1 = Object::Boolean(true);
        let t2 = Object::Boolean(true);
        let f1 = Object::Boolean(false);
        let f2 = Object::Boolean(false);

        assert_eq!(t1.get_hash().unwrap(), t2.get_hash().unwrap());
        assert_eq!(f1.get_hash().unwrap(), f2.get_hash().unwrap());
        assert_ne!(t1.get_hash().unwrap(), f1.get_hash().unwrap());
    }

    #[test]
    fn test_diff_get_hash() {
        let s = Object::String(String::from("1"));
        let i = Object::Integer(1);
        let b = Object::Boolean(true);

        assert_ne!(s.get_hash().unwrap(), i.get_hash().unwrap());
        assert_ne!(s.get_hash().unwrap(), b.get_hash().unwrap());

        assert_ne!(i.get_hash().unwrap(), s.get_hash().unwrap());
        assert_ne!(i.get_hash().unwrap(), b.get_hash().unwrap());

        assert_ne!(b.get_hash().unwrap(), s.get_hash().unwrap());
        assert_ne!(b.get_hash().unwrap(), i.get_hash().unwrap());
    }
}
