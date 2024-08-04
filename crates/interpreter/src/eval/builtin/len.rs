use tracing::{event, Level};

use crate::eval::{
    eval_error::EvalError,
    objects::{Object, PrimitiveObject},
};

pub fn len(args: &[Object]) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::BuiltInInvalidArguments(
            "len".to_string(),
            format!("Expected 1 argument, got {}", args.len()),
        ));
    }

    match &args[0] {
        Object::Array(arr) => {
            event!(Level::DEBUG, "Calling len on array");
            Ok(Object::primitive_from_int(arr.len() as i32))
        }
        Object::Primitive(PrimitiveObject::Str(string)) => {
            event!(Level::DEBUG, "Calling len on string");
            Ok(Object::primitive_from_int(string.len() as i32))
        }
        _ => Err(EvalError::BuiltInInvalidArguments(
            "len".to_string(),
            "Expected a string or array".to_string(),
        )),
    }
}
