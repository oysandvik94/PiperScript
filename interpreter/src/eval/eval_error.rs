use std::fmt::Display;

use crate::parser::ast::Operator;

use super::objects::Object;

#[derive(Debug)]
pub enum EvalError {
    EmptyProgram,
    IncorrectBangSuffix(Object),
    IntegerInfixOperatorError(Operator),
    InfixRightLeft(Object, Object),
    BooleanInfixOperator(Operator),
    NonBooleanConditional(Object),
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
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        eval::{self, EvaledProgram},
        parser::test_util,
    };

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
        ];

        test_util::assert_list(input_expected, |expected: &&str, input: &&str| {
            let hehe = eval::eval(input);

            match hehe {
                EvaledProgram::EvalError(eval_error) => {
                    assert_eq!(expected, &eval_error.to_string().trim())
                }
                EvaledProgram::ParseError(_) => panic!("Got parse error when expecting eval error"),
                EvaledProgram::Valid(_) => panic!("Got valid program when expecting eval error"),
            }
        });
    }
}
