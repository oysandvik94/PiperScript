use std::fs;

use interpreter::{
    eval::{self, objects::Environment, EvaledProgram},
    parser::{ParsedProgram, Parser},
};

pub fn execute_file(filename: &str) -> Result<(), std::io::Error> {
    let file_content = fs::read_to_string(filename)?;
    let execution_scope = &mut Environment::new_env_reference();
    let evaluated_output = eval::eval(&file_content, execution_scope);
    handle_output(evaluated_output);

    Ok(())
}

pub fn parse_file(filename: &str) -> Result<(), std::io::Error> {
    let file_content = fs::read_to_string(filename)?;
    let mut parser = Parser::new(&file_content);

    let parsed_program = parser.parse_program();
    match parsed_program {
        ParsedProgram::ValidProgram(_) => println!("Program contained no errors"),
        ParsedProgram::InvalidProgram(parse_errors) => {
            eprintln!("Found parse errors:");
            parse_errors.into_iter().for_each(|error| {
                eprintln!("{error}");
            });
        }
    }

    Ok(())
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
