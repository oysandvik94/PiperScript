use std::fmt::Display;

#[derive(Debug)]
pub enum EvalError {
    EmptyProgram,
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::EmptyProgram => write!(f, "Program contained no code"),
        }
    }
}
