#![allow(dead_code)]
#![feature(string_into_chars)]

use std::{cell::RefCell, fs::File, io::Read, rc::Rc};

use crate::environment::Environment;

mod ast;
mod builtins;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        println!("Hello! This is the Monkey programming language!");
        repl::start(std::io::stdin(), std::io::stdout());
    } else {
        let mut file = File::open(args[1].as_str()).unwrap();
        let mut buffer = String::new();
        let read = file.read_to_string(&mut buffer);
        if read.is_err() {
            println!("{read:?}");
            return;
        }

        let lexer = lexer::Lexer::new(buffer);
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program();

        if !parser.get_errors().is_empty() {
            println!("errors: {:?}", parser.get_errors());
            return;
        }

        let env = Rc::new(RefCell::new(Environment::new()));
        let evaluated = evaluator::eval(program, &env);
        println!("{}", evaluated.inspect());
    }
}
