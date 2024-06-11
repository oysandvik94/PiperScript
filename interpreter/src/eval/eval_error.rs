use std::fmt::Display;

use crate::parser::ast::Operator;

use super::objects::Object;

#[derive(Debug)]
pub enum EvalError {
    EmptyProgram,
    IncorrectBangSuffix(Object),
    IntegerInfixOperatorError(Operator),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::EmptyProgram => write!(f, "Program contained no code"),
            EvalError::IncorrectBangSuffix(object) => {
                write!(f, "! can not be followed by {object}")
            }
            EvalError::IntegerInfixOperatorError(operator) => {
                write!(f, "Operator {operator} is not supported for numbers")
            }
        }
    }
}
