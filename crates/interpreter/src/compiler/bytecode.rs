use std::fmt::Display;

#[repr(u8)]
#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    OpConstant(u16),
    Add,
    Sub,
    Mul,
    Div,
    Pop,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::OpConstant(num) => write!(f, "OpConstant({num})"),
            Instruction::Add => write!(f, "Add"),
            Instruction::Pop => write!(f, "Pop"),
            Instruction::Sub => write!(f, "Sub"),
            Instruction::Mul => write!(f, "Mul"),
            Instruction::Div => write!(f, "Div"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assert_operation_size() {
        assert_eq!(size_of::<Instruction>(), 4);
    }
}
