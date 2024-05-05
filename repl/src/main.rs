use std::io::stdin;

use lexer::lexer::generate_tokens;

fn main() {
    println!("Welcome to lasagnelang, try and write some code");

    loop {
        let mut buffer = String::new();

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
