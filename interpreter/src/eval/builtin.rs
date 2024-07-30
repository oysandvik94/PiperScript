mod last;
mod len;
mod push;
mod rest;

use super::{eval_error::EvalError, objects::Object};

#[derive(Debug, Clone)]
pub struct BuiltInFunctionObject {
    pub name: String,
    pub function: fn(&[Object]) -> Result<Object, EvalError>,
}

pub(crate) fn lookup_builtins(identifier: &str) -> Option<Object> {
    match identifier {
        "len" => create_builtin("len", len::len),
        "last" => create_builtin("last", last::last),
        "rest" => create_builtin("rest", rest::rest),
        "push" => create_builtin("push", push::push),
        _ => None,
    }
}

fn create_builtin(
    name: &str,
    function: fn(&[Object]) -> Result<Object, EvalError>,
) -> Option<Object> {
    Some(Object::BuiltInFunction(BuiltInFunctionObject {
        name: name.to_string(),
        function,
    }))
}
