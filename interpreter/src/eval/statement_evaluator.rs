use tracing::{span, Level};

use crate::parser::{
    ast::{BlockStatement, Statement},
    expressions::expression_statement::ExpressionStatement,
    return_statement::ReturnStatement,
};

use super::{eval_error::EvalError, expression_evaluator::Evaluable, objects::Object};

pub fn eval_statements(statements: &Vec<Statement>) -> Result<Object, EvalError> {
    let mut object: Object = Object::Void;

    for statement in statements {
        object = statement.eval()?;

        if let Object::ReturnValue(value) = object {
            return Ok(*value);
        }
    }

    Ok(object)
}

impl Evaluable for Statement {
    fn eval(&self) -> Result<Object, EvalError> {
        let expression_statement_span = span!(Level::DEBUG, "Eval");
        let _enter = expression_statement_span.enter();

        match self {
            Statement::Expression(ExpressionStatement { expression }) => expression.eval(),
            Statement::Return(return_statement) => return_statement.eval(),
            Statement::Assign(_) => todo!(),
        }
    }
}

impl Evaluable for ReturnStatement {
    fn eval(&self) -> Result<Object, EvalError> {
        Ok(Object::ReturnValue(Box::new(self.return_value.eval()?)))
    }
}

impl Evaluable for BlockStatement {
    fn eval(&self) -> Result<Object, EvalError> {
        let mut object: Object = Object::Void;

        for statement in &self.statements {
            object = statement.eval()?;

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
}
