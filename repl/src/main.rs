use std::io::stdin;

use lexer::lexer::Lexer;

fn main() {
    println!("Welcome to lasagnelang, try and write some code");

    loop {
        let mut buffer = String::new();

        match stdin().read_line(&mut buffer) {
            Ok(_) => {
                let name = buffer.trim_end();
                let res = {
                    let lexer = Lexer::new(name);
                    lexer.generate_tokens()
                };

                println!("{:?}", res);
            }
            Err(_) => panic!(),
        }
    }
}
