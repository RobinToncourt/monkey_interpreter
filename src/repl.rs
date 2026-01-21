use std::io::{BufReader, Read, Write};

use crate::lexer;

const PROMPT: &str = ">> ";

pub fn start<IN, OUT>(mut input: IN, mut output: OUT)
where
    IN: Read,
    OUT: Write,
{
    loop {
        output.write_all(PROMPT.as_bytes()).unwrap();
        output.flush().unwrap();

        let buffer = read_line(&mut input);
        let buffer = buffer.trim();

        if buffer.is_empty() {
            continue;
        }
        if buffer == "exit" {
            break;
        }

        let lexer = lexer::Lexer::new(buffer.to_owned());
        for token in lexer {
            output
                .write_all(&format!("{token:?}\n").into_bytes())
                .unwrap();
        }
    }
}

// Read `input` until a new line char is encountered.
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

    String::from_utf8(buffer).unwrap()
}
