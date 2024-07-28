use std::rc::Rc;

use crate::parser::{
    ast::{BlockStatement, Identifier},
    expressions::functions::{CallExpression, FunctionLiteral},
};

use super::{
    eval_error::EvalError,
    expression_evaluator::Evaluable,
    objects::{EnvReference, Environment, Object},
};

#[derive(Debug, Clone)]
pub struct FunctionObject {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub scope: EnvReference,
}

impl FunctionObject {
    pub fn call(&self, args: &[Object]) -> Result<Object, EvalError> {
        let mut extended_env = Environment::new_from_enclosing(&self.scope);
        extended_env
            .borrow_mut()
            .fill_from_params_and_arguments(&self.parameters, args)?;

        match self.body.eval(&mut extended_env)? {
            Object::ReturnValue(return_value) => Ok(*return_value),
            obj => Ok(obj),
        }
    }
}

impl Evaluable for FunctionLiteral {
    fn eval(&self, env: &mut EnvReference) -> Result<Object, EvalError> {
        Ok(Object::Function(FunctionObject {
            parameters: self.parameters.clone(),
            body: self.body.clone(),
            scope: Rc::clone(env),
        }))
    }
}

impl Evaluable for CallExpression {
    fn eval(&self, env: &mut EnvReference) -> Result<Object, EvalError> {
        let function = match self.function.eval(env)? {
            Object::Function(function_object) => function_object,
            unexpected_object => {
                return Err(EvalError::UnexpectedFunctionExpression(unexpected_object))
            }
        };

        let args = self
            .arguments
            .iter()
            .map(|expr| expr.eval(env))
            .collect::<Result<Vec<Object>, EvalError>>()?;

        function.call(&args)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        eval::objects::Object,
        parser::{
            ast::{BlockStatement, Identifier, Operator},
            expressions::expression::Expression,
        },
        test_util,
    };

    #[test]
    fn closure_test() {
        let input = "
            let newAdder: fn(x):
                fn(y): x + y~
            ~

             let addTwo: newAdder(2)

            addTwo(2)

            ";
        let object = test_util::expect_evaled_program(input);
        match object {
            Object::Integer(integer) => assert_eq!(4, integer, "Function should evaluate to 4"),
            unexpected_object => panic!("expected integer, but got {unexpected_object}"),
        }
    }

    #[test]
    fn function_object_test() {
        let input = "fn(x): x + 2~ ";

        let object = test_util::expect_evaled_program(input);

        match object {
            Object::Function(function_object) => {
                assert_eq!(function_object.parameters.len(), 1);
                assert_eq!(function_object.parameters[0], Identifier(String::from("x")));
                let expected_block_statement = BlockStatement {
                    statements: vec![test_util::create_infix_test_case(
                        test_util::create_identifierliteral("x"),
                        Expression::IntegerLiteral(2),
                        Operator::Plus,
                    )],
                };
                assert_eq!(function_object.body, expected_block_statement);
            }
            something_else => {
                panic!("Expected correct function, got {something_else} for input '{input}'")
            }
        }
    }

    #[test]
    fn function_call_test() {
        let input_expected: Vec<(&str, i32)> = vec![
            ("let identity: fn(x): return x~ identity(5)", 5),
            ("let double: fn(x): return x * 2~ double(5)", 10),
            ("let add: fn(x, y): return x + y~ add(5, 10)", 15),
            ("let add: fn(x, y): return x + y~ add(5 + 5, add(5, 5))", 20),
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
