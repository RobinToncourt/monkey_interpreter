use crate::object::Object;

pub type BuiltInFunction = fn(arguments: &[Object]) -> Object;

pub fn get_builtins(name: &str) -> Option<BuiltInFunction> {
    match name {
        "exit" => Some(exit),
        "len" => Some(len),
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

    if let Object::String(string) = &arguments[0] {
        #[allow(clippy::cast_possible_wrap)]
        return Object::Integer(string.len() as i64);
    }

    Object::Error(format!(
        "argument to 'len' not supported, expected 'String' got '{}'.",
        arguments[0].get_type()
    ))
}
