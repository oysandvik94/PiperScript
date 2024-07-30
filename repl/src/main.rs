use std::{
    env,
    fs::{self},
    io::{stdin, stdout, Write},
};

use interpreter::eval::{self, objects::Environment, EvaledProgram};
use tracing_subscriber::FmtSubscriber;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();

    if args.len() == 2 {
        let filename = &args[1];

        let file_content = fs::read_to_string(filename)?;
        let execution_scope = &mut Environment::new_env_reference();
        let evaluated_output = eval::eval(&file_content, execution_scope);
        handle_output(evaluated_output);

        return Ok(());
    }

    let subscriber = FmtSubscriber::builder()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    println!("Welcome to lasagnalang, try and write some code:");
    let repl_scope = &mut Environment::new_env_reference();

    loop {
        let mut buffer = String::new();

        print!("> ");
        stdout().flush()?;

        match stdin().read_line(&mut buffer) {
            Ok(_) => {
                let input = buffer.trim_end();
                let evaluated_output = eval::eval(input, repl_scope);

                handle_output(evaluated_output);
            }
            Err(_) => panic!(),
        }
    }
}

fn handle_output(evaluated_output: EvaledProgram) {
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
