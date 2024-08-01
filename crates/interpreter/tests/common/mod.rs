use std::fmt::Display;

use interpreter::eval::{self, objects::Environment, EvaledProgram};

pub fn assert_eval<T: Display>(source_code: &str, expected_output: T) {
    let scope = &mut Environment::new_env_reference();
    let object = eval::eval(source_code, scope);

    match object {
        EvaledProgram::Valid(output) => {
            let expected_output = expected_output.to_string();
            assert_eq!(output.to_string(), expected_output)
        }
        EvaledProgram::ParseError(parse_errors) => {
            parse_errors.into_iter().for_each(|ele| {
                eprintln!("{ele}");
            });
            panic!("Test failed with parse errors")
        }
        EvaledProgram::EvalError(eval_error) => {
            panic!("Test failed with runtime error: {}", eval_error)
        }
    }
}
