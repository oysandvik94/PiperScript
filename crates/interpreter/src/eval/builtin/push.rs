use crate::eval::{eval_error::EvalError, objects::Object};

const FN_NAME: &str = "push";
pub fn push(args: &[Object]) -> Result<Object, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::BuiltInInvalidArguments(
            FN_NAME.to_string(),
            format!("Expected 2 argument, got {}", args.len()),
        ));
    }

    match (&args[0], &args[1]) {
        (Object::Array(arr), object) => call(arr, object),
        _ => Err(EvalError::BuiltInInvalidArguments(
            FN_NAME.to_string(),
            "Expected an array and an element".to_string(),
        )),
    }
}

fn call(array: &[Object], element: &Object) -> Result<Object, EvalError> {
    let mut new_array = array.to_vec();
    new_array.push(element.clone());
    Ok(Object::Array(new_array))
}
