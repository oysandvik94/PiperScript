use std::io::{stdin, stdout, Write};

use interpreter::eval::{self};
use tracing_subscriber::FmtSubscriber;

fn main() -> Result<(), std::io::Error> {
    let subscriber = FmtSubscriber::builder()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    println!("Welcome to lasagnalang, try and write some code:");

    loop {
        let mut buffer = String::new();

        print!("> ");
        stdout().flush()?;

        match stdin().read_line(&mut buffer) {
            Ok(_) => {
                let input = buffer.trim_end();
                eval::eval(input);
                todo!()
                // TODO: Will be replaced with eval result.

                // let tokens = LexedTokens::from(input);
                // let mut parser: Parser = Parser::new(tokens);
                // let program: Program = parser.parse_program();
                //
                // if program.parse_errors.is_empty() {
                //     println!("{:}", program);
                // } else {
                //     println!("Error!");
                //     program.parse_errors.iter().for_each(|x| println!("{x}"));
                // }
            }
            Err(_) => panic!(),
        }
    }
}
