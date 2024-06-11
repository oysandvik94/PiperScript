use std::fmt::Display;

use super::objects::Object;

#[derive(Debug)]
pub enum EvalError {
    EmptyProgram,
    IncorrectBangSuffix(Object),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::EmptyProgram => write!(f, "Program contained no code"),
            EvalError::IncorrectBangSuffix(object) => {
                write!(f, "! can not be followed by {object}")
            }
        }
    }
}
