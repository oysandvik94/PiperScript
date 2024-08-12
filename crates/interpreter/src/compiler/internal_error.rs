use thiserror::Error;

#[derive(Error, Debug)]
pub enum InternalError {
    #[error("The opcode {0} was not recognized by the compiler")]
    UnknownOpcode(u8),
}
