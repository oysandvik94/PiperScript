mod len;

use super::{eval_error::EvalError, objects::Object};

#[derive(Debug, Clone)]
pub struct BuiltInFunctionObject {
    pub name: String,
    pub function: fn(&[Object]) -> Result<Object, EvalError>,
}

pub(crate) fn lookup_builtins(identifier: &str) -> Option<Object> {
    match identifier {
        "len" => Some(Object::BuiltInFunction(BuiltInFunctionObject {
            name: "len".to_string(),
            function: len::len,
        })),
        _ => None,
    }
}
