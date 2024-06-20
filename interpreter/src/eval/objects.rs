use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    Void,
    ReturnValue(Box<Object>),
}

pub struct Environment {
    scope: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            scope: HashMap::new(),
        }
    }

    pub fn get_identifier(&self, identifier: &str) -> Option<Object> {
        self.scope.get(identifier).cloned()
    }

    pub fn set_identifier(&mut self, identifier: &str, object: Object) {
        self.scope.insert(String::from(identifier), object);
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Object::*;

        match self {
            Integer(number) => write!(f, "{number}"),
            Boolean(boolean) => write!(f, "{boolean}"),
            Void => write!(f, ""),
            ReturnValue(object) => write!(f, "{object}"),
        }
    }
}
