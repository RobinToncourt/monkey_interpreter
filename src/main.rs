#![allow(dead_code)]
#![feature(string_into_chars)]

mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;
mod environment;

fn main() {
    println!("Hello! This is the Monkey programming language!");

    repl::start(std::io::stdin(), std::io::stdout());
}
