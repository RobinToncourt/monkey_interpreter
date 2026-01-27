#[derive(Debug)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Null => String::from("null"),
            Self::Integer(i) => format!("{i}"),
            Self::Boolean(b) => format!("{b}"),
            Self::ReturnValue(value) => value.inspect(),
        }
    }
}
