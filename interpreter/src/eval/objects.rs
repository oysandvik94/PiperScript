use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Object::*;

        match self {
            Integer(number) => write!(f, "{number}"),
            Boolean(boolean) => write!(f, "{boolean}"),
        }
    }
}
