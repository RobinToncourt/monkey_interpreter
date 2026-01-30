use std::{
    io::{BufReader, Read, Write},
    {cell::RefCell, rc::Rc},
};

use crate::{environment::Environment, evaluator, lexer, parser};

const PROMPT: &str = ">> ";
const MONKEY_FACE: &str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

pub fn start<IN, OUT>(mut input: IN, mut output: OUT)
where
    IN: Read,
    OUT: Write,
{
    let env = Rc::new(RefCell::new(Environment::new()));

    loop {
        output.write_all(PROMPT.as_bytes()).unwrap();
        output.flush().unwrap();

        let buffer = read_line(&mut input);
        let buffer = buffer.trim();

        if buffer.is_empty() {
            continue;
        }

        let lexer = lexer::Lexer::new(buffer.to_owned());
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program();

        if !parser.get_errors().is_empty() {
            print_parser_errors(&mut output, parser.get_errors());
            continue;
        }

        let evaluated = evaluator::eval(program, &env);
        println!("{}", evaluated.inspect());
    }
}

/// Read `input` until a new line char is encountered.
fn read_line<IN>(input: &mut IN) -> String
where
    IN: Read,
{
    let mut buffer: Vec<u8> = Vec::new();

    let input = BufReader::new(input);
    let mut bytes = input.bytes();
    while let Some(Ok(c)) = bytes.next() {
        if c == b'\n' {
            break;
        }
        buffer.push(c);
    }

    let utf8_line = String::from_utf8(buffer);
    utf8_line.unwrap_or_else(|_| {
        println!("Invalid UTF-8 input.");
        String::new()
    })
}

fn print_parser_errors<OUT>(output: &mut OUT, errors: &[String])
where
    OUT: Write,
{
    output
        .write_all(format!("{MONKEY_FACE}\n").as_bytes())
        .unwrap();
    output
        .write_all("Whoops! We ran in some monkey business here!\n".as_bytes())
        .unwrap();
    output.write_all(" parser errors:\n".as_bytes()).unwrap();

    for error in errors {
        output.write_all(b"\t").unwrap();
        output.write_all(error.as_bytes()).unwrap();
        output.write_all(b"\n").unwrap();
        output.flush().unwrap();
    }
}
