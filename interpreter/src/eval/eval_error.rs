use std::fmt::Display;

use crate::{
    eval::objects::FunctionListable,
    parser::{
        ast::{Identifier, Operator},
        expressions::expression::Expression,
    },
};

use super::objects::Object;

#[derive(Debug)]
pub enum EvalError {
    EmptyProgram,
    IncorrectBangSuffix(Object),
    IntegerInfixOperatorError(Operator),
    StringInfixOperatorError(Operator),
    InfixRightLeft(Object, Object),
    BooleanInfixOperator(Operator),
    NonBooleanConditional(Object),
    IdentifierNotFound(Identifier),
    VoidAssignment(Expression),
    UnexpectedFunctionExpression(Object),
    ArgumentMismatch(Vec<Identifier>, Vec<Object>),
    BuiltInInvalidArguments(String, String),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::EmptyProgram => writeln!(f, "Program contained no code"),
            EvalError::IncorrectBangSuffix(object) => {
                writeln!(f, "! can not be followed by {object}")
            }
            EvalError::IntegerInfixOperatorError(operator) => {
                writeln!(f, "Operator {operator} is not supported for numbers")
            }
            EvalError::InfixRightLeft(left, right) => {
                writeln!(f, "{left} and {right} does not have a common operator")
            }
            EvalError::BooleanInfixOperator(operator) => {
                writeln!(f, "Operator {operator} is not supported for booleans")
            }
            EvalError::NonBooleanConditional(object) => {
                writeln!(f, "Expected boolean in for conditional, but got {object}")
            }
            EvalError::IdentifierNotFound(identifier) => {
                writeln!(f, "Identifier {identifier} not found in scope")
            }
            EvalError::VoidAssignment(_) => {
                writeln!(
                    f,
                    "Tried to assign an expression that doesn't return a value to an identifier"
                )
            }
            EvalError::UnexpectedFunctionExpression(object) => {
                writeln!(f, "Can only call a function on an identifier representing a function, or an actual function literal. Instead tried to call on {object}")
            }
            EvalError::ArgumentMismatch(params, args) => {
                writeln!(f, "Passed in arguments to no matche function parameters. Parameters: {} Arguments: {}", params.to_function_string(), args.to_function_string())
            }
            EvalError::StringInfixOperatorError(operator) => {
                writeln!(f, "Operator {operator} is not supported for strings")
            }
            EvalError::BuiltInInvalidArguments(func_name, error_msg) => {
                writeln!(
                    f,
                    "Error calling built-in function {func_name}: {error_msg}"
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        eval::{self, objects::Environment, EvaledProgram},
        test_util,
    };

    use super::EvalError;

    #[test]
    fn test_error_handling() {
        let input_expected: Vec<(&str, &str)> = vec![
            ("5 + true", "5 and true does not have a common operator"),
            ("5 + true. 5", "5 and true does not have a common operator"),
            ("-true", "! can not be followed by true"),
            ("true + false", "Operator + is not supported for booleans"),
            (
                "5. true + false. 5",
                "Operator + is not supported for booleans",
            ),
            (
                "if 10 > 1: return true + false~",
                "Operator + is not supported for booleans",
            ),
            (
                "
                if 10 > 1:
                    if 10 > 1:
                        return true + false
                    ~
                ~
                ",
                "Operator + is not supported for booleans",
            ),
            (
                "\"Hello\" - \"world\"",
                "Operator - is not supported for strings",
            ),
        ];

        test_util::assert_list(input_expected, |expected: &&str, input: &&str| {
            let evaled_program = eval::eval(input, &mut Environment::new_env_reference());

            match evaled_program {
                EvaledProgram::EvalError(eval_error) => {
                    assert_eq!(expected, &eval_error.to_string().trim())
                }
                EvaledProgram::ParseError(_) => panic!("Got parse error when expecting eval error"),
                EvaledProgram::Valid(_) => panic!("Got valid program when expecting eval error"),
            }
        });
    }

    #[test]
    fn void_assignment_should_fail_test() {
        let invalid_output = "let a: if false: 5~";
        let evaled_program = eval::eval(invalid_output, &mut Environment::new_env_reference());

        match evaled_program {
            EvaledProgram::EvalError(eval_error) => {
                matches!(eval_error, EvalError::VoidAssignment(_))
            }
            _ => panic!("Code was supposed to return an error"),
        };
    }
}
