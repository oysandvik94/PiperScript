use crate::eval::{eval_error::EvalError, objects::Object};

pub const FN_NAME: &str = "print";
pub fn print(args: &[Object]) -> Result<Object, EvalError> {
    for arg in args {
        println!("{}", arg);
    }

    Ok(Object::Void)
}
