use crate::ast::{Expression, Program, Statement};
use crate::{ast::Node, object::Object};

pub fn eval<T>(node: T) -> Object
where
    T: Into<Node>,
{
    match node.into() {
        Node::Program(program) => eval_program(&program),
        Node::Statement(statement) => eval_statement(&statement),
        Node::Expression(expression) => eval_expression(&expression),
    }
}

fn eval_program(program: &Program) -> Object {
    eval_statements(program.get_statements())
}

fn eval_statements(statements: &[Statement]) -> Object {
    let mut result = Object::Null;

    for statement in statements {
        result = eval_statement(statement);

        if let Object::ReturnValue(return_value) = result {
            return *return_value;
        }
    }

    result
}

fn eval_statement(statement: &Statement) -> Object {
    match statement {
        Statement::Let { name: _, value: _ } => Object::Null,
        Statement::Return { return_value } => {
            let return_value = eval_expression(return_value);
            Object::ReturnValue(Box::new(return_value))
        }
        Statement::Expression(expression) => eval_expression(expression),
        Statement::Block(statements) => eval_statements(statements),
    }
}

fn eval_expression(expression: &Expression) -> Object {
    match expression {
        Expression::Integer(i) => Object::Integer(*i),
        Expression::Boolean(b) => Object::Boolean(*b),
        Expression::Prefix { operator, right } => {
            let right = eval_expression(right);
            eval_prefix_expression(operator, &right)
        }
        Expression::Infix {
            left,
            operator,
            right,
        } => {
            let left = eval_expression(left);
            let right = eval_expression(right);
            eval_infix_expression(&left, operator, &right)
        }
        Expression::If {
            condition,
            consequences,
            alternatives,
        } => eval_if_expression(condition, consequences, alternatives.as_deref()),
        _ => Object::Null,
    }
}

fn eval_prefix_expression(operator: &str, right: &Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => panic!("Unsupported prefix operator: '{operator}'."),
    }
}

fn eval_infix_expression(left: &Object, operator: &str, right: &Object) -> Object {
    match (left, right) {
        (Object::Integer(left_value), Object::Integer(right_value)) => {
            eval_integer_infix_expression(*left_value, operator, *right_value)
        }
        (Object::Boolean(left_bool), Object::Boolean(right_bool)) => {
            eval_boolean_infix_expression(*left_bool, operator, *right_bool)
        }
        (Object::Null, Object::Null) => eval_null_infix_expression(operator),
        _ => Object::Null,
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequences: &Statement,
    alternatives: Option<&Statement>,
) -> Object {
    let condition = eval_expression(condition);

    if is_truthy(&condition) {
        eval_statement(consequences)
    } else {
        alternatives.map_or(Object::Null, eval_statement)
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Boolean(b) => *b,
        Object::Integer(i) => *i != 0,
        Object::Null => false,
        Object::ReturnValue(_) => todo!(), // is_truthy(value),
    }
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
        _ => panic!("Unsupported `Object::Integer` operator: '{operator}'."),
    }
}

fn eval_boolean_infix_expression(left: bool, operator: &str, right: bool) -> Object {
    match operator {
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => panic!("Unsupported `Object::Boolean` operator: '{operator}'."),
    }
}

fn eval_null_infix_expression(operator: &str) -> Object {
    match operator {
        "==" => Object::Boolean(true),
        "!=" => Object::Boolean(false),
        _ => panic!("Unsupported `Object::Null` operator: '{operator}'."),
    }
}

fn eval_bang_operator_expression(right: &Object) -> Object {
    match right {
        Object::Null => Object::Boolean(true),
        Object::Integer(_) => Object::Boolean(false),
        Object::Boolean(bool) => Object::Boolean(!bool),
        Object::ReturnValue(_) => todo!(), // eval_bang_operator_expression(value)
    }
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Object {
    match right {
        Object::Integer(i64) => Object::Integer(-*i64),
        Object::Null | Object::Boolean(_) => Object::Null,
        Object::ReturnValue(_) => todo!(),
    }
}

#[cfg(test)]
mod evaluator_tests {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser};
    use std::any::Any;

    #[test]
    fn test_eval_integer_expression() {
        let tests: [(&str, i64); 15] = [
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

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests: [(&str, bool); 19] = [
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

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests: [(&str, bool); 6] = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
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

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            if expected.is::<i64>() {
                test_integer_object(evaluated, *expected.downcast::<i64>().unwrap());
            } else if expected.is::<Object>() {
                test_null_object(*expected.downcast::<Object>().unwrap())
            } else {
                panic!("expected type not handled: '{:?}'.", expected.type_id())
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let test: [(&str, i64); 5] = [
            ("return 10;", 10_i64),
            ("return 10; 9;", 10_i64),
            ("return 2 * 5; 9;", 10_i64),
            ("9: return 2 * 5; 9;", 10_i64),
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
        ];

        for (input, expected) in test {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        eval(program)
    }

    fn test_integer_object(object: Object, expected_int: i64) {
        let Object::Integer(integer) = object else {
            panic!("object is not `Object::Integer`, got: '{object:?}'.")
        };

        assert_eq!(integer, expected_int);
    }

    fn test_boolean_object(object: Object, expected_bool: bool) {
        let Object::Boolean(boolean) = object else {
            panic!("object is not `Object::Boolean`, got: '{object:?}'.")
        };

        assert_eq!(boolean, expected_bool);
    }

    fn test_null_object(object: Object) {
        assert!(matches!(object, Object::Null))
    }
}
