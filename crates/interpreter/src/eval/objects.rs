use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::parser::ast::Identifier;

use super::{
    builtin::BuiltInFunctionObject, eval_error::EvalError, function_evaluator::FunctionObject,
};

#[derive(Debug, Clone, PartialEq)]
pub struct HashPair {
    pub key: PrimitiveObject,
    pub value: Object,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Primitive(PrimitiveObject),
    Array(Vec<Object>),
    Hash(HashMap<PrimitiveObject, HashPair>),
    Void,
    ReturnValue(Box<Object>),
    Function(FunctionObject),
    BuiltInFunction(BuiltInFunctionObject),
}

impl Object {
    pub fn primitive_from_int(primitive: i32) -> Object {
        Object::Primitive(PrimitiveObject::Integer(primitive))
    }

    pub fn primitive_from_str(primitive: String) -> Object {
        Object::Primitive(PrimitiveObject::Str(primitive))
    }

    pub fn primitive_from_bool(primitive: bool) -> Object {
        Object::Primitive(PrimitiveObject::Boolean(primitive))
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum PrimitiveObject {
    Integer(i32),
    Str(String),
    Boolean(bool),
}

pub type EnvReference = Rc<RefCell<Environment>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    scope: HashMap<String, Object>,
    outer_scopes: Option<EnvReference>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            scope: HashMap::new(),
            outer_scopes: None,
        }
    }

    pub fn new_env_reference() -> EnvReference {
        Rc::new(RefCell::new(Environment::new()))
    }

    pub fn new_from_enclosing(env: &EnvReference) -> EnvReference {
        let env = Environment {
            scope: HashMap::new(),
            outer_scopes: Some(Rc::clone(env)),
        };

        Rc::new(RefCell::new(env))
    }

    pub fn get_identifier(&self, identifier: &str) -> Option<Object> {
        match self.scope.get(identifier) {
            Some(object) => Some(object.clone()),
            None => match &self.outer_scopes {
                Some(outer_scope) => outer_scope.borrow().get_identifier(identifier),
                None => None,
            },
        }
    }

    pub fn set_identifier(&mut self, identifier: &str, object: Object) {
        self.scope.insert(String::from(identifier), object);
    }

    pub fn fill_from_params_and_arguments(
        &mut self,
        parameters: &[Identifier],
        arguments: &[Object],
    ) -> Result<(), EvalError> {
        if parameters.len() != arguments.len() {
            return Err(EvalError::ArgumentMismatch(
                parameters.to_vec(),
                arguments.to_vec(),
            ));
        }

        parameters
            .iter()
            .zip(arguments.iter())
            .for_each(|(param, argument)| {
                self.set_identifier(&param.0, argument.clone());
            });

        Ok(())
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

pub trait Listable {
    fn to_commaseperated_list(&self) -> String;
}

impl<T: ToString> Listable for Vec<T> {
    fn to_commaseperated_list(&self) -> String {
        self.iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ")
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Object::*;

        match self {
            Primitive(primitive) => write!(f, "{}", primitive),
            Void => write!(f, ""),
            ReturnValue(object) => write!(f, "{object}"),
            Function(function) => {
                write!(f, "fn ({})", function.parameters.to_commaseperated_list())
            }
            BuiltInFunction(builtin) => write!(f, "builtin fn: {}", builtin.name),
            Array(elements) => write!(f, "[{}]", elements.to_commaseperated_list()),
            Hash(map) => write!(
                f,
                "{{{}}}",
                map.iter()
                    .map(|(key, value)| format!("{}: {}", key, value.value))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl Display for PrimitiveObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use PrimitiveObject::*;

        match self {
            Integer(number) => write!(f, "{number}"),
            Str(string) => write!(f, "{string}"),
            Boolean(boolean) => write!(f, "{boolean}"),
        }
    }
}
