use crate::eval::{eval_error::EvalError, objects::Object};

pub const FN_NAME: &str = "last";
pub fn last(args: &[Object]) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::BuiltInInvalidArguments(
            FN_NAME.to_string(),
            format!("Expected 1 argument, got {}", args.len()),
        ));
    }

    match &args[0] {
        Object::Array(arr) => call(arr),
        _ => Err(EvalError::BuiltInInvalidArguments(
            FN_NAME.to_string(),
            "Expected a array".to_string(),
        )),
    }
}

fn call(args: &[Object]) -> Result<Object, EvalError> {
    match args.last() {
        Some(obj) => Ok(obj.clone()),
        None => Err(EvalError::BuiltInInvalidArguments(
            FN_NAME.to_string(),
            "Expected a non-empty array".to_string(),
        )),
    }
}
