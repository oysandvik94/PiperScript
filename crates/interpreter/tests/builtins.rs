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
fn len_returns_size_of_array() {
    let test_cases = [
        (r#"len([2, 5, 3])"#, 3),
        (r#"let foo: [2, 6, 4, 6]. len(foo)"#, 4),
        (r#"len([])"#, 0),
    ];

    for test_case in test_cases {
        common::assert_eval(test_case.0, test_case.1);
    }
}

#[test]
fn last_returns_last_element() {
    let test_cases = [
        (r#"last([2, 5, 8])"#, 8),
        (r#"let foo: [2, 6, 4, 6]. last(foo)"#, 6),
    ];

    for test_case in test_cases {
        common::assert_eval(test_case.0, test_case.1);
    }
}

#[test]
fn rest_returns_last_elements() {
    let test_cases = [
        (r#"rest([2, 5, 8])"#, "[5, 8]"),
        (r#"let foo: [2, 6, 4, 6]. rest(foo)"#, "[6, 4, 6]"),
        (r#"let foo: [2, 6, 4, 6]. rest(foo)[0]"#, "6"),
    ];

    for test_case in test_cases {
        common::assert_eval(test_case.0, test_case.1);
    }
}

#[test]
fn push_adds_element_to_array() {
    let test_cases = [
        (r#"push([2, 5, 8], 4)"#, "[2, 5, 8, 4]"),
        (r#"let foo: [2, 6, 4, 6]. push(foo, 4)"#, "[2, 6, 4, 6, 4]"),
        (r#"let foo: [2, 6, 4, 6]. last(push(foo, 10))"#, "10"),
    ];

    for test_case in test_cases {
        common::assert_eval(test_case.0, test_case.1);
    }
}

#[test]
fn last_on_empty_array_errors() {
    let input = "last([])";

    let scope = &mut Environment::new_env_reference();
    let object = eval::eval(input, scope);

    match object {
        EvaledProgram::EvalError(EvalError::BuiltInInvalidArguments(_, _)) => {}
        _ => panic!("Expected empty array error"),
    }
}

#[test]
fn len_error_test() {
    let test_cases = [
        (r#"len(5)"#, ("len", "Expected a string or array")),
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
