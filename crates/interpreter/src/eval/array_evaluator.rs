use crate::parser::expressions::arrays::ArrayLiteral;

use super::{eval_error::EvalError, expression_evaluator::Evaluable, objects::Object};

impl Evaluable for ArrayLiteral {
    fn eval(
        &self,
        env: &mut super::objects::EnvReference,
    ) -> Result<super::objects::Object, super::eval_error::EvalError> {
        let elements = self
            .elements
            .iter()
            .map(|expr| expr.eval(env))
            .collect::<Result<Vec<Object>, EvalError>>()?;

        Ok(Object::Array(elements))
    }
}
