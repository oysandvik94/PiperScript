mod last;
mod len;
mod print;
mod push;
mod rest;

use super::{eval_error::EvalError, objects::Object};

#[derive(Debug, Clone, PartialEq)]
pub struct BuiltInFunctionObject {
    pub name: String,
    pub function: fn(&[Object]) -> Result<Object, EvalError>,
}

pub(crate) fn lookup_builtins(identifier: &str) -> Option<Object> {
    match identifier {
        print::FN_NAME => create_builtin(print::FN_NAME, print::print),
        len::FN_NAME => create_builtin(len::FN_NAME, len::len),
        last::FN_NAME => create_builtin(last::FN_NAME, last::last),
        rest::FN_NAME => create_builtin(rest::FN_NAME, rest::rest),
        push::FN_NAME => create_builtin(push::FN_NAME, push::push),
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
