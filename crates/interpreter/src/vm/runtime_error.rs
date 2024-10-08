use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("Stackoverflow")]
    StackOverflowError,
}
