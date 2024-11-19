use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("Stackoverflow")]
    StackOverflowError,
    #[error("Incompatible types")]
    TypeError,
    #[error("Operand cannot be used with !")]
    UnknownBangOperand,
    #[error("Operand cannot be used with -")]
    UnknownMinusOperand,
}
