use std::io::{stdin, stdout, Write};

use interpreter::eval::{self, objects::Environment, EvaledProgram};

use crate::{args::PiperArgs, execute_code};

pub fn execute_repl(args: &PiperArgs) -> Result<(), std::io::Error> {
    println!("Welcome to piperscript, try and write some code:");
    let repl_scope = &mut Environment::new_env_reference();

    loop {
        let mut buffer = String::new();

        print!("> ");
        stdout().flush()?;

        match stdin().read_line(&mut buffer) {
            Ok(_) => {
                let input = buffer.trim_end();

                if args.use_vm {
                    execute_code(input);
                } else {
                    let evaluated_output = eval::eval(input, repl_scope);
                    handle_output(evaluated_output);
                }
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
