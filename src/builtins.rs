use crate::object::{BuiltInFunction, Object};

pub fn get_builtins(name: &str) -> Option<BuiltInFunction> {
    match name {
        "len" => Some(len),
        _ => None,
    }
}

fn len(arguments: &[Object]) -> Object {
    if arguments.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1.",
            arguments.len()
        ));
    }

    if let Object::String(string) = &arguments[0] {
        #[allow(clippy::cast_possible_wrap)]
        return Object::Integer(string.len() as i64);
    }

    Object::Error(format!(
        "argument to 'len' not supported, got '{}'.",
        arguments[0].get_type()
    ))
}
