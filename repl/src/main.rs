use std::io::{stdin, stdout, Write};

use parser::{ast::Program, lexer::lexedtokens::LexedTokens, parser::Parser};

fn main() -> Result<(), std::io::Error> {
    println!("Welcome to lasagnalang, try and write some code:");

    loop {
        let mut buffer = String::new();

        print!("> ");
        stdout().flush()?;

        match stdin().read_line(&mut buffer) {
            Ok(_) => {
                let input = buffer.trim_end();
                let tokens = LexedTokens::from(input);
                let mut parser: Parser = Parser::new(tokens);
                let program: Program = parser.parse_program();

                println!("{:}", program);
            }
            Err(_) => panic!(),
        }
    }
}
