use std::io::{stdin, stdout, Write};

use lexer::lexer::generate_tokens;

fn main() ->  Result<(), std::io::Error> {
    println!("Welcome to lasagnalang, try and write some code:");

    loop {
        let mut buffer = String::new();

        print!("> ");
        stdout().flush()?;

        match stdin().read_line(&mut buffer) {
            Ok(_) => {
                let input = buffer.trim_end();
                let res = { generate_tokens(input) };

                println!("{:?}", res);
            }
            Err(_) => panic!(),
        }
    }
}
