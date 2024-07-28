use tracing::{event, Level};

use crate::eval::{eval_error::EvalError, objects::Object};

pub fn len(args: &[Object]) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::BuiltInInvalidArguments(
            "len".to_string(),
            format!("Expected 1 argument, got {}", args.len()),
        ));
    }

    match &args[0] {
        Object::Str(s) => {
            event!(Level::DEBUG, "Calling len on: {s}");
            Ok(Object::Integer(s.len() as i32))
        }
        _ => Err(EvalError::BuiltInInvalidArguments(
            "len".to_string(),
            "Expected a string".to_string(),
        )),
    }
}
