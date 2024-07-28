use interpreter::eval::{self, eval_error::EvalError, objects::Environment, EvaledProgram};

mod common;

#[test]
fn len_test() {
    let test_cases = [
        (r#"len("Hello")"#, 5),
        (r#"len("")"#, 0),
        (r#"len("Hello world")"#, 11),
    ];

    for test_case in test_cases {
        common::assert_eval(test_case.0, test_case.1);
    }
}

#[test]
fn len_error_test() {
    let test_cases = [
        (r#"len(5)"#, ("len", "Expected a string")),
        (
            r#"len("one", "tro")"#,
            ("len", "Expected 1 argument, got 2"),
        ),
    ];

    for test_case in test_cases {
        let scope = &mut Environment::new_env_reference();
        let eval_res = eval::eval(test_case.0, scope);
        match eval_res {
            EvaledProgram::EvalError(eval_error) => match eval_error {
                EvalError::BuiltInInvalidArguments(fn_name, error) => {
                    assert_eq!(fn_name, test_case.1 .0);
                    assert_eq!(error, test_case.1 .1);
                }
                unexpected_error => {
                    panic!("Test failed with unexpected result: {:?}", unexpected_error)
                }
            },
            _ => panic!("Test failed with unexpected result"),
        }
    }
}
