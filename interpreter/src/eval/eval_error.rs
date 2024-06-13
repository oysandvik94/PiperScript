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
