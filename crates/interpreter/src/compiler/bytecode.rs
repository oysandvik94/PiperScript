#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Instruction {
    OpConstant(u16),
    Add,
    Sub,
    Mul,
    Div,
    Pop,
    True,
    False,
    Equal,
    NotEqual,
    GreaterThan,
    Bang,
    Minus,
    Jump(u16),
    JumpNotTruthy(u16),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assert_operation_size() {
        assert_eq!(size_of::<Instruction>(), 4);
    }
}
