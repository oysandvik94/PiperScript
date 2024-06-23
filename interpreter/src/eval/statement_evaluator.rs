use tracing::{span, Level};

use crate::parser::{
    assign_statement::AssignStatement,
    ast::{BlockStatement, Identifier, Statement},
    expressions::expression_statement::ExpressionStatement,
    return_statement::ReturnStatement,
};

use super::{
    eval_error::EvalError,
    expression_evaluator::Evaluable,
    objects::{EnvReference, Object},
};

pub fn eval_statements(
    statements: &Vec<Statement>,
    env: &mut EnvReference,
) -> Result<Object, EvalError> {
    let mut object: Object = Object::Void;

    for statement in statements {
        object = statement.eval(env)?;

        if let Object::ReturnValue(value) = object {
            return Ok(*value);
        }
    }

    Ok(object)
}

impl Evaluable for Statement {
    fn eval(&self, env: &mut EnvReference) -> Result<Object, EvalError> {
        let expression_statement_span = span!(Level::DEBUG, "Eval");
        let _enter = expression_statement_span.enter();

        match self {
            Statement::Expression(ExpressionStatement { expression }) => expression.eval(env),
            Statement::Return(return_statement) => return_statement.eval(env),
            Statement::Assign(assign_statement) => assign_statement.eval(env),
        }
    }
}

impl Evaluable for AssignStatement {
    fn eval(&self, env: &mut EnvReference) -> Result<Object, EvalError> {
        let value = self.assignment.eval(env)?;

        if let Object::Void = value {
            return Err(EvalError::VoidAssignment(self.assignment.clone()));
        }

        env.borrow_mut().set_identifier(&self.identifier.0, value);
        Ok(Object::Void)
    }
}

impl Evaluable for Identifier {
    fn eval(&self, env: &mut EnvReference) -> Result<Object, EvalError> {
        match env.borrow().get_identifier(&self.0) {
            Some(object) => Ok(object),
            None => Err(EvalError::IdentifierNotFound(self.clone())),
        }
    }
}

impl Evaluable for ReturnStatement {
    fn eval(&self, env: &mut EnvReference) -> Result<Object, EvalError> {
        Ok(Object::ReturnValue(Box::new(self.return_value.eval(env)?)))
    }
}

impl Evaluable for BlockStatement {
    fn eval(&self, env: &mut EnvReference) -> Result<Object, EvalError> {
        let mut object: Object = Object::Void;

        for statement in &self.statements {
            object = statement.eval(env)?;

            if let Object::ReturnValue(_) = object {
                return Ok(object);
            }
        }

        Ok(object)
    }
}

#[cfg(test)]
mod tests {
    use crate::{eval::objects::Object, parser::test_util};

    #[test]
    fn eval_return_statement_test() {
        let input_expected: Vec<(&str, i32)> = vec![
            ("return 10", 10),
            ("return 10. 9.", 10),
            ("return 2 * 5. 9.", 10),
            ("9. return 2 * 5. 9.", 10),
            (
                "
             if 10 > 1: 
                if 10 > 1:
                    return 10
                ~
               return 1
             ~
             ",
                10,
            ),
        ];

        test_util::assert_list(input_expected, |expected: &i32, input: &&str| {
            let object = test_util::expect_evaled_program(input);

            match object {
                Object::Integer(boolean) => assert_eq!(expected, &boolean),
                something_else => {
                    panic!("Expected correct integer, got {something_else} for input '{input}'")
                }
            }
        });
    }

    #[test]
    fn eval_assignment_statement_test() {
        let input_expected: Vec<(&str, i32)> = vec![
            ("let a: 5. a.", 5),
            ("let a: 5 * 5. a.", 25),
            ("let a: 5. let b: a. b.", 5),
            ("let a: 5. let b: a. let c: a + b + 5. c.", 15),
            ("let a: if true: 15 else: 2~ a.", 15),
            ("let a: if false: 15 else: 2~ a.", 2),
        ];

        test_util::assert_list(input_expected, |expected: &i32, input: &&str| {
            let object = test_util::expect_evaled_program(input);

            match object {
                Object::Integer(boolean) => assert_eq!(expected, &boolean),
                something_else => {
                    panic!("Expected correct integer, got {something_else} for input '{input}'")
                }
            }
        });
    }
}
