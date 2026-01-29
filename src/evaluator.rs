use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Expression, Node, Program, Statement},
    builtins::get_builtins,
    environment::{Environment, SharedEnv},
    object::Object,
};

const FLOAT_COMP_ERROR_MARGIN: f64 = 0.01;

pub fn eval<T>(node: T, env: &SharedEnv) -> Object
where
    T: Into<Node>,
{
    match node.into() {
        Node::Program(program) => eval_program(&program, env),
        Node::Statement(statement) => eval_statement(&statement, env),
        Node::Expression(expression) => eval_expression(&expression, env),
    }
}

fn eval_program(program: &Program, env: &SharedEnv) -> Object {
    let mut result = Object::Null;

    for statement in program.get_statements() {
        result = eval_statement(statement, env);

        if let Object::ReturnValue(return_value) = result {
            return *return_value;
        } else if matches!(result, Object::Error(_)) {
            return result;
        }
    }

    result
}

fn eval_statement(statement: &Statement, env: &SharedEnv) -> Object {
    match statement {
        Statement::Let { name, expression } => eval_let_statement(name, expression, env),
        Statement::Return { return_expression } => eval_return_statement(return_expression, env),
        Statement::Expression(expression) => eval_expression(expression, env),
        Statement::Block(statements) => eval_block_statement(statements, env),
    }
}

fn eval_let_statement(name: &str, expression: &Expression, env: &SharedEnv) -> Object {
    let value = eval_expression(expression, env);

    if is_error(&value) {
        return value;
    }

    env.borrow_mut().set(name.to_owned(), value);

    Object::Null
}

fn eval_return_statement(return_expression: &Expression, env: &SharedEnv) -> Object {
    let return_value = eval_expression(return_expression, env);

    if is_error(&return_value) {
        return return_value;
    }

    Object::ReturnValue(Box::new(return_value))
}

fn eval_expression(expression: &Expression, env: &SharedEnv) -> Object {
    match expression {
        Expression::Identifier(name) => eval_identifier(name, env),
        Expression::Integer(i) => Object::Integer(*i),
        Expression::Float(f) => Object::Float(*f),
        Expression::String(value) => Object::String(value.clone()),
        Expression::Boolean(b) => Object::Boolean(*b),
        Expression::Prefix { operator, right } => {
            let right = eval_expression(right, env);

            if is_error(&right) {
                return right;
            }

            eval_prefix_expression(operator, &right)
        }
        Expression::Infix {
            left,
            operator,
            right,
        } => {
            let left = eval_expression(left, env);

            if is_error(&left) {
                return left;
            }

            let right = eval_expression(right, env);

            if is_error(&right) {
                return right;
            }

            eval_infix_expression(&left, operator, &right)
        }
        Expression::If {
            condition,
            consequences,
            alternatives,
        } => eval_if_expression(condition, consequences, alternatives.as_deref(), env),
        Expression::Function { parameters, body } => {
            eval_function_expression(parameters, body, env)
        }
        Expression::Call {
            function,
            arguments,
        } => eval_call_expression(function, arguments, env),
    }
}

fn eval_block_statement(statements: &[Statement], env: &SharedEnv) -> Object {
    let mut result = Object::Null;

    for statement in statements {
        result = eval_statement(statement, env);

        if matches!(result, Object::ReturnValue(_)) || matches!(result, Object::Error(_)) {
            return result;
        }
    }

    result
}

fn eval_prefix_expression(operator: &str, right: &Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::Error(format!("unknown operator: {operator}{}", right.inspect())),
    }
}

fn eval_bang_operator_expression(right: &Object) -> Object {
    match right {
        Object::Null => Object::Boolean(true),
        Object::Integer(_) | Object::Float(_) => Object::Boolean(false),
        Object::Boolean(bool) => Object::Boolean(!bool),
        _ => unreachable!(),
    }
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Object {
    match right {
        Object::Integer(value) => Object::Integer(-*value),
        Object::Float(value) => Object::Float(-*value),
        Object::Null | Object::Boolean(_) => {
            Object::Error(format!("unknown operator: -{}", right.get_type()))
        }
        _ => unreachable!(),
    }
}

fn eval_infix_expression(left: &Object, operator: &str, right: &Object) -> Object {
    match (left, right) {
        (Object::Integer(left_value), Object::Integer(right_value)) => {
            eval_integer_infix_expression(*left_value, operator, *right_value)
        }
        (Object::Float(left_value), Object::Float(right_value)) => {
            eval_float_infix_expression(*left_value, operator, *right_value)
        }
        (Object::String(left_str), Object::String(right_str)) => {
            eval_string_infix_expression(left_str, operator, right_str)
        }
        (Object::Boolean(left_bool), Object::Boolean(right_bool)) => {
            eval_boolean_infix_expression(*left_bool, operator, *right_bool)
        }
        (Object::Null, Object::Null) => eval_null_infix_expression(operator),
        _ => Object::Error(format!(
            "type mismatch: {} {operator} {}",
            left.get_type(),
            right.get_type()
        )),
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequences: &Statement,
    alternatives: Option<&Statement>,
    env: &SharedEnv,
) -> Object {
    let condition = eval_expression(condition, env);

    if is_error(&condition) {
        return condition;
    }

    if is_truthy(&condition) {
        eval_statement(consequences, env)
    } else {
        alternatives.map_or(Object::Null, |alternatives| {
            eval_statement(alternatives, env)
        })
    }
}

fn eval_function_expression(
    parameters: &[Expression],
    body: &Statement,
    env: &SharedEnv,
) -> Object {
    Object::Function {
        parameters: parameters.to_vec(),
        body: body.clone(),
        env: env.clone(),
    }
}

fn eval_call_expression(
    function: &Expression,
    arguments: &[Expression],
    env: &SharedEnv,
) -> Object {
    let function = eval_expression(function, env);

    if is_error(&function) {
        return function;
    }

    let arguments = eval_expressions(arguments, env);
    let arguments = match arguments {
        Ok(arguments) => arguments,
        Err(error) => return error,
    };

    apply_function(function, &arguments)
}

fn eval_expressions(arguments: &[Expression], env: &SharedEnv) -> Result<Vec<Object>, Object> {
    let mut result = Vec::new();

    for arg in arguments {
        let evaluated = eval_expression(arg, env);

        if is_error(&evaluated) {
            return Err(evaluated);
        }

        result.push(evaluated);
    }

    Ok(result)
}

fn apply_function(function: Object, arguments: &[Object]) -> Object {
    match function {
        Object::Function {
            parameters,
            body,
            env,
        } => {
            let extended_env = extend_function_environment(parameters, env, arguments);
            let evaluated = eval_statement(&body, &extended_env);
            unwrap_return_value(evaluated)
        }
        Object::BuiltIn(func) => func(arguments),
        _ => Object::Error(format!("Not a function: {}", function.get_type())),
    }
}

fn extend_function_environment(
    fn_parameters: Vec<Expression>,
    fn_env: SharedEnv,
    arguments: &[Object],
) -> SharedEnv {
    let mut extended_env = Environment::new_enclosed(fn_env);

    for (i, param) in fn_parameters.into_iter().enumerate() {
        let Expression::Identifier(name) = param else {
            // All parameters are of type `Expression::Identifier` thanks to the parser.
            unreachable!();
        };
        extended_env.set(name, arguments[i].clone());
    }

    Rc::new(RefCell::new(extended_env))
}

fn unwrap_return_value(object: Object) -> Object {
    if let Object::ReturnValue(return_value) = object {
        *return_value
    } else {
        object
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Boolean(b) => *b,
        Object::Integer(i) => *i != 0,
        Object::Null => false,
        _ => unreachable!(),
    }
}

fn eval_identifier(name: &str, env: &SharedEnv) -> Object {
    if let Some(val) = env.borrow().get(name) {
        return val;
    }

    if let Some(builtin) = get_builtins(name) {
        return Object::BuiltIn(builtin);
    }

    Object::Error(format!("identifier not found: {name}"))
}

fn eval_integer_infix_expression(left: i64, operator: &str, right: i64) -> Object {
    match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        ">" => Object::Boolean(left > right),
        "<" => Object::Boolean(left < right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Error(format!(
            "unknown operator: {} {operator} {}",
            Object::integer_type_str(),
            Object::integer_type_str()
        )),
    }
}

fn eval_float_infix_expression(left: f64, operator: &str, right: f64) -> Object {
    match operator {
        "+" => Object::Float(left + right),
        "-" => Object::Float(left - right),
        "*" => Object::Float(left * right),
        "/" => Object::Float(left / right),
        ">" => Object::Boolean(left > right),
        "<" => Object::Boolean(left < right),
        "==" => Object::Boolean((left - right).abs() < FLOAT_COMP_ERROR_MARGIN),
        "!=" => Object::Boolean((left - right).abs() > FLOAT_COMP_ERROR_MARGIN),
        _ => Object::Error(format!(
            "unknown operator: {} {operator} {}",
            Object::float_type_str(),
            Object::float_type_str()
        )),
    }
}

fn eval_string_infix_expression(left: &str, operator: &str, right: &str) -> Object {
    match operator {
        "+" => Object::String(format!("{left}{right}")),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Error(format!(
            "unknown operator: {} {operator} {}",
            Object::string_type_str(),
            Object::string_type_str()
        )),
    }
}

fn eval_boolean_infix_expression(left: bool, operator: &str, right: bool) -> Object {
    match operator {
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Error(format!(
            "unknown operator: {} {operator} {}",
            Object::boolean_type_str(),
            Object::boolean_type_str()
        )),
    }
}

fn eval_null_infix_expression(operator: &str) -> Object {
    match operator {
        "==" => Object::Boolean(true),
        "!=" => Object::Boolean(false),
        _ => Object::Error(format!(
            "unknown operator: {} {operator} {}.",
            Object::boolean_type_str(),
            Object::boolean_type_str()
        )),
    }
}

fn is_error(object: &Object) -> bool {
    matches!(object, Object::Error(_))
}

#[cfg(test)]
mod evaluator_tests {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser};
    use std::any::Any;

    #[test]
    fn test_eval_integer_expression() {
        const TESTS: [(&str, i64); 15] = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 - 50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        let mut all_tests_passed = true;
        for (input, expected) in TESTS {
            let evaluated = test_eval(input);
            all_tests_passed &= test_integer_object(evaluated, expected);
        }
        assert!(all_tests_passed);
    }

    #[test]
    fn test_eval_float_expression() {
        const TESTS: [(&str, f64); 15] = [
            ("5.2", 5.2),
            ("10.123456", 10.123456),
            ("-5.12", -5.12),
            ("-10.9", -10.9),
            ("5.0 + 5.0 + 5.0 + 5.0 - 10.0", 10.0),
            ("2.0 * 2.0 * 2.0 * 2.0 * 2.0", 32.0),
            ("-50.1 + 100.2 - 50.1", 0.0),
            ("5.1 * 2.0 + 10.1", 20.3),
            ("5.1 + 2.0 * 10.0", 25.1),
            ("20.0 + 2.0 * -10.0", 0.0),
            ("50.0 / 2.0 * 2.0 + 10.0", 60.0),
            ("2.0 * (5.0 + 10.0)", 30.0),
            ("3.0 * 3.0 * 3.0 + 10.0", 37.0),
            ("3.0 * (3.0 * 3.0) + 10.0", 37.0),
            ("(5.0 + 10.0 * 2.0 + 15.0 / 3.0) * 2.0 + -10.0", 50.0),
        ];

        let mut all_tests_passed = true;
        for (input, expected) in TESTS {
            let evaluated = test_eval(input);
            all_tests_passed &= test_float_object(evaluated, expected);
        }
        assert!(all_tests_passed);
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests: [(&str, bool); 23] = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("1.0 != 2.0", true),
            ("1.0 == 1.0", true),
            ("1.0 < 2.0", true),
            ("1.0 > 2.0", false),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        let mut all_tests_passed = true;
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            all_tests_passed &= test_boolean_object(evaluated, expected);
        }
        assert!(all_tests_passed);
    }

    #[test]
    fn test_bang_operator() {
        let tests: [(&str, bool); 7] = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!6.7", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        let mut all_tests_passed = true;
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            all_tests_passed &= test_boolean_object(evaluated, expected);
        }
        assert!(all_tests_passed);
    }

    #[test]
    fn test_if_else_expression() {
        let tests: [(&str, Box<dyn Any>); 7] = [
            ("if (true) { 10 }", Box::new(10_i64)),
            ("if (false) { 10 }", Box::new(Object::Null)),
            ("if (1) { 10 }", Box::new(10_i64)),
            ("if (1 < 2) { 10 }", Box::new(10_i64)),
            ("if (1 > 2) { 10 }", Box::new(Object::Null)),
            ("if (1 > 2) { 10 } else { 20 }", Box::new(20_i64)),
            ("if (1 < 2) { 10 } else { 20 }", Box::new(10_i64)),
        ];

        let mut all_tests_passed = true;
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            all_tests_passed &= test_object(evaluated, expected);
        }
        assert!(all_tests_passed);
    }

    #[test]
    fn test_return_statements() {
        let test: [(&str, i64); 8] = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9: return 2 * 5; 9;", 10),
            ("if (10 > 1) { return 10; }", 10),
            (
                "if (10 > 1) {\
                    if (10 > 1) {\
                        return 10;\
                    }\
                    \
                    return 1;\
                }",
                10,
            ),
            (
                "let f = fn(x) {\
                    return x;\
                    x + 10;\
                };\
                f(10);",
                10,
            ),
            (
                "let f = fn(x) {\
                    let result = x + 10;\
                    return result;\
                    return 10;\
                };\
                f(10);",
                20,
            ),
        ];

        let mut all_tests_passed = true;
        for (input, expected) in test {
            let evaluated = test_eval(input);
            all_tests_passed &= test_integer_object(evaluated, expected);
        }

        let evaluated = test_eval("return 42.12;");
        all_tests_passed &= test_float_object(evaluated, 42.12);
        assert!(all_tests_passed);
    }

    #[test]
    fn test_error_handling() {
        const TESTS: [(&str, &str); 10] = [
            ("5 + true;", "type mismatch: Integer + Boolean"),
            ("5.5 + true;", "type mismatch: Float + Boolean"),
            ("5 + true; 5;", "type mismatch: Integer + Boolean"),
            ("-true", "unknown operator: -Boolean"),
            ("true + false;", "unknown operator: Boolean + Boolean"),
            ("5; true + false; 5", "unknown operator: Boolean + Boolean"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: Boolean + Boolean",
            ),
            (
                "if (10 > 1) {\
                    if (10 > 1) {\
                        return true + false;\
                    }\
                    \
                    return 1;\
                }",
                "unknown operator: Boolean + Boolean",
            ),
            ("foobar", "identifier not found: foobar"),
            (r#""Hello" - "World!""#, "unknown operator: String - String"),
        ];

        let mut all_tests_passed = true;
        for (input, expected) in TESTS {
            let evaluated = test_eval(input);
            all_tests_passed &= test_error_object(evaluated, expected);
        }
        assert!(all_tests_passed);
    }

    #[test]
    fn test_let_statements() {
        let test: [(&str, Box<dyn Any>); 5] = [
            ("let a = 5; a;", Box::new(5_i64)),
            ("let a = 5.5; a;", Box::new(5.5_f64)),
            ("let a = 5 * 5; a;", Box::new(25_i64)),
            ("let a = 5; let b = a; b;", Box::new(5_i64)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Box::new(15_i64),
            ),
        ];

        let mut all_tests_passed = true;
        for (input, expected) in test {
            let evaluated = test_eval(input);
            all_tests_passed &= test_object(evaluated, expected);
        }
        assert!(all_tests_passed);
    }

    #[test]
    fn test_function_object() {
        const INPUT: &str = "fn(x) { x + 2; };";

        let evaluated = test_eval(INPUT);
        let Object::Function {
            parameters,
            body,
            env: _,
        } = evaluated
        else {
            panic!("expected `Object::Function` object, found {evaluated:?}");
        };

        assert_eq!(parameters.len(), 1);
        assert_eq!(parameters[0].to_string(), "x");

        assert_eq!(body.to_string(), "(x + 2)");
    }

    #[test]
    fn test_function_application() {
        let tests: [(&str, Box<dyn Any>); 7] = [
            ("let identity = fn(x) { x; }; identity(5);", Box::new(5_i64)),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Box::new(5_i64),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Box::new(10_i64),
            ),
            ("let add = fn(x, y) { x + y }; add(5, 5);", Box::new(10_i64)),
            (
                "let add = fn(x, y) { x + y }; add(5.5, 5.5);",
                Box::new(11.0_f64),
            ),
            (
                "let add = fn(x, y) { x + y }; add(5 + 5, add(5, 5));",
                Box::new(20_i64),
            ),
            ("fn(x) { x; }(5)", Box::new(5_i64)),
        ];

        let mut all_tests_passed = true;
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            all_tests_passed &= test_object(evaluated, expected);
        }
        assert!(all_tests_passed);
    }

    #[test]
    fn test_closures() {
        const INPUT: &str = "\
        let new_adder = fn(x) {\
            fn(y) { x + y };\
        };\
        \
        let add_two = new_adder(2);\
        add_two(2);";

        let evaluated = test_eval(INPUT);
        assert!(test_integer_object(evaluated, 4));
    }

    #[test]
    fn test_string_literal() {
        const INPUT: &str = r#""Hello, World!""#;

        let evaluated = test_eval(INPUT);
        let Object::String(value) = evaluated else {
            panic!("expected `Object::String` object, found {evaluated:?}");
        };

        assert_eq!(value, "Hello, World!");
    }

    #[test]
    fn test_string_concatenation() {
        const INPUT: &str = r#""Hello," + " " + "World!""#;

        let evaluated = test_eval(INPUT);
        let Object::String(value) = evaluated else {
            panic!("expected `Object::String` object, found {evaluated:?}");
        };

        assert_eq!(value, "Hello, World!");
    }

    #[test]
    fn test_string_comparison() {
        const TESTS: [(&str, bool); 4] = [
            (r#""hello" == "hello""#, true),
            (r#""hello" == "world""#, false),
            (r#""hello" != "hello""#, false),
            (r#""hello" != "world""#, true),
        ];

        let mut all_tests_passed = true;
        for (input, expected) in TESTS {
            let evaluated = test_eval(input);
            all_tests_passed &= test_boolean_object(evaluated, expected);
        }
        assert!(all_tests_passed);
    }

    #[test]
    fn test_builtin_function_len() {
        let tests: [(&str, Box<dyn Any>); 5] = [
            (r#"len("")"#, Box::new(0_i64)),
            (r#"len("four")"#, Box::new(4_i64)),
            (r#"len("Hello, World!")"#, Box::new(13_i64)),
            (
                r#"len(1)"#,
                Box::new("argument to 'len' not supported, expected 'String' got 'Integer'."),
            ),
            (
                r#"len("one", "two")"#,
                Box::new("wrong number of arguments. got=2, want=1."),
            ),
        ];

        let mut all_tests_passed = true;
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            all_tests_passed &= test_object(evaluated, expected);
        }
        assert!(all_tests_passed);
    }

    #[test]
    fn test_builtin_function_int_cast() {
        let tests: Vec<(&str, Box<dyn Any>)> = vec![
            (r#"int(42)"#, Box::new(42_i64)),
            (r#"int(42.42)"#, Box::new(42_i64)),
            (r#"int(true)"#, Box::new(1_i64)),
            (r#"int(false)"#, Box::new(0_i64)),
            (r#"int("42")"#, Box::new(42_i64)),
            (
                r#"int("abc")"#,
                Box::new("could not parse: 'abc' to Integer."),
            ),
            (
                r#"int(fn(x){return x;})"#,
                Box::new(
                    "argument to 'int' not supported, expected 'Integer', 'Float', 'Boolean' or 'String' got 'Function'.",
                ),
            ),
            (
                r#"int(42, 42)"#,
                Box::new("wrong number of arguments. got=2, want=1."),
            ),
        ];

        assert_tests(tests);
    }

    #[test]
    fn test_builtin_function_float_cast() {
        let tests: Vec<(&str, Box<dyn Any>)> = vec![
            (r#"float(42)"#, Box::new(42.0_f64)),
            (r#"float(42.42)"#, Box::new(42.42_f64)),
            (r#"float(true)"#, Box::new(1.0_f64)),
            (r#"float(false)"#, Box::new(0.0_f64)),
            (r#"float("42.0")"#, Box::new(42.0_f64)),
            (
                r#"float("abc")"#,
                Box::new("could not parse: 'abc' to Float."),
            ),
            (
                r#"float(fn(x){return x;})"#,
                Box::new(
                    "argument to 'float' not supported, expected 'Integer', 'Float', 'Boolean' or 'String' got 'Function'.",
                ),
            ),
            (
                r#"float(42.0, 42.0)"#,
                Box::new("wrong number of arguments. got=2, want=1."),
            ),
        ];

        assert_tests(tests);
    }

    #[test]
    fn test_builtin_function_boolean_cast() {
        let tests: Vec<(&str, Box<dyn Any>)> = vec![
            (r#"boolean(42)"#, Box::new(true)),
            (r#"boolean(0)"#, Box::new(false)),
            (r#"boolean(42.42)"#, Box::new(true)),
            (r#"boolean(0.0)"#, Box::new(false)),
            (r#"boolean(true)"#, Box::new(true)),
            (r#"boolean(false)"#, Box::new(false)),
            (r#"boolean("true")"#, Box::new(true)),
            (r#"boolean("false")"#, Box::new(false)),
            (
                r#"boolean("abc")"#,
                Box::new("could not parse: 'abc' to boolean."),
            ),
            (
                r#"boolean(fn(x){return x;})"#,
                Box::new(
                    "argument to 'boolean' not supported, expected 'Integer', 'Float', 'Boolean' or 'String' got 'Function'.",
                ),
            ),
            (
                r#"boolean(true, false)"#,
                Box::new("wrong number of arguments. got=2, want=1."),
            ),
        ];

        assert_tests(tests);
    }

    #[test]
    fn test_builtin_function_string_cast() {
        let tests: Vec<(&str, &str)> = vec![
            (r#"string(42)"#, "42"),
            (r#"string(42.42)"#, "42.42"),
            (r#"string(true)"#, "true"),
            (r#"string(false)"#, "false"),
            (r#"string("42")"#, "42"),
            (r#"string(fn(x){return x;})"#, "fn(x) {\n\treturn x;\n}"),
            (
                r#"string(42, 42)"#,
                "wrong number of arguments. got=2, want=1.",
            ),
        ];

        let mut all_tests_passed = true;
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            if let Object::String(s) = evaluated {
                if s != expected {
                    println!("\t'{s}' != '{expected}'");
                    all_tests_passed = false;
                }
            } else if let Object::Error(e) = evaluated {
                if e != expected {
                    println!("\t'{e}' != '{expected}'");
                    all_tests_passed = false;
                }
            } else {
                println!("\t{:?}", evaluated);
                all_tests_passed &= false;
            }
        }
        assert!(all_tests_passed);
    }

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Environment::new()));
        eval(program, &env)
    }

    fn assert_tests(tests: Vec<(&str, Box<dyn Any>)>) {
        let mut all_tests_passed = true;
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            all_tests_passed &= test_object(evaluated, expected);
        }
        assert!(all_tests_passed);
    }

    #[must_use]
    fn test_object(object: Object, expected: Box<dyn Any>) -> bool {
        if expected.is::<bool>() {
            test_boolean_object(object, *expected.downcast_ref::<bool>().unwrap())
        } else if expected.is::<i64>() {
            test_integer_object(object, *expected.downcast_ref::<i64>().unwrap())
        } else if expected.is::<f64>() {
            test_float_object(object, *expected.downcast_ref::<f64>().unwrap())
        } else if expected.is::<&str>() {
            test_error_object(object, *expected.downcast_ref::<&str>().unwrap())
        } else if expected.is::<Object>() {
            test_null_object(object)
        } else {
            panic!("expected type not handled: '{:?}'.", expected.type_id())
        }
    }

    #[must_use]
    fn test_integer_object(object: Object, expected_int: i64) -> bool {
        let Object::Integer(integer) = object else {
            println!("\tobject is not `Object::Integer`, got: '{object:?}'.");
            return false;
        };

        let result = integer == expected_int;
        if !result {
            println!("\t{integer} != {expected_int}");
        }
        result
    }

    #[must_use]
    fn test_float_object(object: Object, expected_float: f64) -> bool {
        let Object::Float(float) = object else {
            println!("\tobject is not `Object::Float`, got: '{object:?}'.");
            return false;
        };

        let result = (float - expected_float).abs() < FLOAT_COMP_ERROR_MARGIN;
        if !result {
            println!("\t{float} != {expected_float}");
        }
        result
    }

    #[must_use]
    fn test_boolean_object(object: Object, expected_bool: bool) -> bool {
        let Object::Boolean(boolean) = object else {
            println!("\tobject is not `Object::Boolean`, got: '{object:?}'.");
            return false;
        };

        let result = boolean == expected_bool;
        if !result {
            println!("\t{boolean} != {expected_bool}");
        }
        result
    }

    #[must_use]
    fn test_null_object(object: Object) -> bool {
        let result = matches!(object, Object::Null);
        if !result {
            println!("\tobject is not `Object::Null`, got: '{object:?}'.");
        }
        result
    }

    #[must_use]
    fn test_error_object(object: Object, expected_error: &str) -> bool {
        let Object::Error(error) = object else {
            println!("object is not `Object::Error`, got: '{object:?}'.");
            return false;
        };

        let result = error == expected_error;
        if !result {
            println!("\t'{error}' != '{expected_error}'");
        }
        result
    }
}
