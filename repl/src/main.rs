use std::io::{stdin, stdout, Write};

use interpreter::eval::{self, objects::Environment, EvaledProgram};
use tracing_subscriber::FmtSubscriber;

fn main() -> Result<(), std::io::Error> {
    let subscriber = FmtSubscriber::builder()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    println!("Welcome to lasagnalang, try and write some code:");
    let repl_scope = &mut Environment::new();

    loop {
        let mut buffer = String::new();

        print!("> ");
        stdout().flush()?;

        match stdin().read_line(&mut buffer) {
            Ok(_) => {
                let input = buffer.trim_end();
                let evaluated_output = eval::eval(input, repl_scope);

                match evaluated_output {
                    EvaledProgram::Valid(object) => println!("{object}"),
                    EvaledProgram::ParseError(parse_errors) => {
                        eprintln!("Found parse errors:");
                        parse_errors.into_iter().for_each(|error| {
                            eprintln!("{error}");
                        });
                    }
                    EvaledProgram::EvalError(runtime_error) => {
                        eprintln!("Runtime error: {runtime_error}")
                    }
                }
            }
            Err(_) => panic!(),
        }
    }
}
