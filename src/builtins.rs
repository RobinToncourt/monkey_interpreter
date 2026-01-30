use crate::object::Object;

pub type BuiltInFunction = fn(arguments: &[Object]) -> Object;

pub fn get_builtins(name: &str) -> Option<BuiltInFunction> {
    match name {
        "exit" => Some(exit),
        "len" => Some(len),
        "int" => Some(int),
        "float" => Some(float),
        "boolean" => Some(boolean),
        "string" => Some(string),
        "is_error" => Some(is_error),
        _ => None,
    }
}

fn exit(arguments: &[Object]) -> Object {
    if arguments.len() > 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=0 or 1.",
            arguments.len()
        ));
    }

    let exit_value = arguments.first().unwrap_or(&Object::Integer(0));
    let Object::Integer(exit_value) = exit_value else {
        return Object::Error(format!(
            "argument to 'exit' not supported, expected Integer got '{}'.",
            arguments[0].get_type()
        ));
    };

    #[allow(clippy::cast_possible_truncation)]
    std::process::exit(*exit_value as i32);
}

fn len(arguments: &[Object]) -> Object {
    if arguments.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1.",
            arguments.len()
        ));
    }

    match &arguments[0] {
        #[allow(clippy::cast_possible_wrap)]
        Object::String(string) => Object::Integer(string.len() as i64),
        #[allow(clippy::cast_possible_wrap)]
        Object::Array(array) => Object::Integer(array.len() as i64),
        _ => Object::Error(format!(
            "argument to 'len' not supported, expected 'String' or 'Array' got '{}'.",
            arguments[0].get_type()
        )),
    }
}

fn int(arguments: &[Object]) -> Object {
    if arguments.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1.",
            arguments.len()
        ));
    }

    match &arguments[0] {
        Object::Null => Object::Integer(0),
        Object::Integer(value) => Object::Integer(*value),
        #[allow(clippy::cast_possible_truncation)]
        Object::Float(value) => Object::Integer(*value as i64),
        Object::String(value) => {
            let Ok(value) = value.parse::<i64>() else {
                return Object::Error(format!("could not parse: '{value}' to Integer."));
            };
            Object::Integer(value)
        }
        Object::Boolean(bool) => Object::Integer(i64::from(*bool)),
        _ => Object::Error(format!(
            "argument to 'int' not supported, expected 'Integer', 'Float', 'Boolean' or 'String' got '{}'.",
            arguments[0].get_type()
        )),
    }
}

fn float(arguments: &[Object]) -> Object {
    if arguments.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1.",
            arguments.len()
        ));
    }

    match &arguments[0] {
        Object::Null => Object::Float(0.0),
        #[allow(clippy::cast_precision_loss)]
        Object::Integer(value) => Object::Float(*value as f64),
        Object::Float(value) => Object::Float(*value),
        Object::String(value) => {
            let Ok(value) = value.parse::<f64>() else {
                return Object::Error(format!("could not parse: '{value}' to Float."));
            };
            Object::Float(value)
        }
        Object::Boolean(bool) => Object::Float(f64::from(*bool)),
        _ => Object::Error(format!(
            "argument to 'float' not supported, expected 'Integer', 'Float', 'Boolean' or 'String' got '{}'.",
            arguments[0].get_type()
        )),
    }
}

fn boolean(arguments: &[Object]) -> Object {
    if arguments.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1.",
            arguments.len()
        ));
    }

    match &arguments[0] {
        Object::Null => Object::Boolean(false),
        Object::Integer(value) => Object::Boolean(*value != 0),
        #[allow(clippy::cast_possible_truncation)]
        Object::Float(value) => Object::Boolean(*value != 0.0),
        Object::String(value) => {
            let Ok(value) = value.parse::<bool>() else {
                return Object::Error(format!("could not parse: '{value}' to boolean."));
            };
            Object::Boolean(value)
        }
        Object::Boolean(bool) => Object::Boolean(*bool),
        _ => Object::Error(format!(
            "argument to 'boolean' not supported, expected 'Integer', 'Float', 'Boolean' or 'String' got '{}'.",
            arguments[0].get_type()
        )),
    }
}

fn string(arguments: &[Object]) -> Object {
    if arguments.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got={}, want=1.",
            arguments.len()
        ));
    }

    Object::String(arguments[0].inspect())
}

fn is_error(arguments: &[Object]) -> Object {
    Object::Boolean(arguments.iter().any(Object::is_error))
}
