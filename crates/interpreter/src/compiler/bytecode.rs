use std::fmt::Display;

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
pub enum OpCode {
    OpConstant(u16),
    Add,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::OpConstant(num) => write!(f, "OpConstant({num})"),
            OpCode::Add => write!(f, "Add"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assert_operation_size() {
        assert_eq!(size_of::<OpCode>(), 4);
    }
}
