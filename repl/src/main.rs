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
                let evaluated_output = eval::eval(input);

                match evaluated_output {
                    Ok(output) => println!("{output}"),
                    Err(error) => {
                        println!("Runtime error:");
                        println!("{error}");
                    }
                }
            }
            Err(_) => panic!(),
        }
    }
}
