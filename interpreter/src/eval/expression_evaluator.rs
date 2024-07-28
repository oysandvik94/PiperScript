use tracing::{event, span, Level};

use crate::parser::expressions::if_expression::IfExpression;
use crate::parser::{
    ast::{Identifier, Operator, PrefixOperator},
    expressions::expression::Expression,
};

use super::{
    eval_error::EvalError,
    objects::{EnvReference, Object},
};

pub(crate) trait Evaluable {
    fn eval(&self, env: &mut EnvReference) -> Result<Object, EvalError>;
}

impl Evaluable for Expression {
    fn eval(&self, env: &mut EnvReference) -> Result<Object, EvalError> {
        use Object::*;

        match self {
            Expression::IntegerLiteral(number) => {
                event!(Level::DEBUG, "Evaluated to number {number}");
                Ok(Integer(*number))
            }
            Expression::StringLiteral(string) => {
                event!(Level::DEBUG, "Evaluated to string {string}");
                Ok(Str(string.clone()))
            }
            Expression::IdentifierLiteral(identifier) => {
                eval_identifier_expression(identifier, env)
            }
            Expression::BooleanLiteral(boolean) => {
                event!(Level::DEBUG, "Evaluated to boolean {boolean}");
                Ok(Boolean(*boolean))
            }
            Expression::Prefix { right, operator } => eval_prefix_expression(right, operator, env),
            Expression::Infix {
                left,
                right,
                operator,
            } => {
                let left = left.eval(env)?;
                let right = right.eval(env)?;
                eval_infix_expression(operator, left, right)
            }
            Expression::If(if_expression) => if_expression.eval(env),
            Expression::Function(function_literal) => function_literal.eval(env),
            Expression::Call(call_expression) => call_expression.eval(env),
        }
    }
}

fn eval_identifier_expression(
    identifier: &Identifier,
    env: &EnvReference,
) -> Result<Object, EvalError> {
    match env.borrow().get_identifier(&identifier.0) {
        Some(object) => Ok(object),
        None => Err(EvalError::IdentifierNotFound(identifier.clone())),
    }
}

impl Evaluable for IfExpression {
    fn eval(&self, env: &mut EnvReference) -> Result<Object, EvalError> {
        let expression_statement_span = span!(Level::DEBUG, "If");
        let _enter = expression_statement_span.enter();
        event!(Level::DEBUG, "Evaluating if condition");

        let evaluated_condition = match self.condition.eval(env)? {
            Object::Boolean(boolean) => boolean,
            unexpected_condition => {
                return Err(EvalError::NonBooleanConditional(unexpected_condition))
            }
        };

        match evaluated_condition {
            true => Ok(self.consequence.eval(env)?),
            false => match &self.alternative {
                Some(alternative) => Ok(alternative.eval(env)?),
                None => Ok(Object::Void),
            },
        }
    }
}

fn eval_infix_expression(
    operator: &crate::parser::ast::Operator,
    left: Object,
    right: Object,
) -> Result<Object, EvalError> {
    use Object::*;
    match (left, right) {
        (Integer(left_integer), Integer(right_integer)) => {
            eval_integer_infix_expression(left_integer, right_integer, operator)
        }
        (Boolean(left_boolean), Boolean(right_boolean)) => {
            eval_boolean_infix_expression(left_boolean, right_boolean, operator)
        }
        (Str(left_str), Str(right_str)) => {
            eval_string_infix_expression(left_str, right_str, operator)
        }
        (unexpected_left, unexpected_right) => Err(EvalError::InfixRightLeft(
            unexpected_left.clone(),
            unexpected_right.clone(),
        )),
    }
}

fn eval_string_infix_expression(
    left_str: String,
    right_str: String,
    operator: &Operator,
) -> Result<Object, EvalError> {
    Ok(match operator {
        Operator::Plus => {
            let concatted_str = [left_str, right_str].concat();
            Object::Str(concatted_str)
        }
        unsupported_operator => {
            return Err(EvalError::StringInfixOperatorError(
                unsupported_operator.clone(),
            ))
        }
    })
}

fn eval_boolean_infix_expression(
    left_boolean: bool,
    right_boolean: bool,
    operator: &Operator,
) -> Result<Object, EvalError> {
    use Object::*;

    Ok(match operator {
        Operator::Equals => Boolean(left_boolean == right_boolean),
        Operator::NotEquals => Boolean(left_boolean != right_boolean),
        unsupported_operator => {
            return Err(EvalError::BooleanInfixOperator(
                unsupported_operator.clone(),
            ))
        }
    })
}

fn eval_integer_infix_expression(
    left_integer: i32,
    right_integer: i32,
    operator: &crate::parser::ast::Operator,
) -> Result<Object, EvalError> {
    use Object::*;

    Ok(match operator {
        Operator::Minus => Integer(left_integer - right_integer),
        Operator::Plus => Integer(left_integer + right_integer),
        Operator::Multiply => Integer(left_integer * right_integer),
        Operator::DividedBy => Integer(left_integer / right_integer),
        Operator::LessThan => Boolean(left_integer < right_integer),
        Operator::GreaterThan => Boolean(left_integer > right_integer),
        Operator::Equals => Boolean(left_integer == right_integer),
        Operator::NotEquals => Boolean(left_integer != right_integer),
        unexpected_operator => {
            return Err(EvalError::IntegerInfixOperatorError(
                unexpected_operator.clone(),
            ));
        }
    })
}

fn eval_prefix_expression(
    right: &Expression,
    operator: &PrefixOperator,
    env: &mut EnvReference,
) -> Result<Object, EvalError> {
    let right = right.eval(env)?;
    match operator {
        PrefixOperator::Bang => eval_bang_operator_expression(&right),
        PrefixOperator::Minus => eval_minus_operator_expression(&right),
    }
}

fn eval_minus_operator_expression(right: &Object) -> Result<Object, EvalError> {
    match right {
        Object::Integer(integer_value) => Ok(Object::Integer(-integer_value)),
        unexpected_object => Err(EvalError::IncorrectBangSuffix(unexpected_object.clone())),
    }
}

fn eval_bang_operator_expression(right: &Object) -> Result<Object, EvalError> {
    match right {
        Object::Boolean(boolean_value) => Ok(Object::Boolean(!boolean_value)),
        unexpected_object => Err(EvalError::IncorrectBangSuffix(unexpected_object.clone())),
    }
}

#[cfg(test)]
mod tests {
    use crate::{eval::objects::Object, test_util};

    #[test]
    fn eval_integer_expression_test() {
        let input_expected: Vec<(&str, i32)> = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        let asserter = |expected: &i32, input: &&str| {
            let object = test_util::expect_evaled_program(input);

            match object {
                Object::Integer(number) => assert_eq!(&number, expected),
                unexpected_type => {
                    panic!("Should have returned a number, instead got {unexpected_type}")
                }
            }
        };

        test_util::assert_list(input_expected, asserter);
    }

    #[test]
    fn eval_boolean_expression_test() {
        let input_expected: Vec<(&str, bool)> = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        let asserter = |expected: &bool, input: &&str| {
            let object = test_util::expect_evaled_program(input);

            match object {
                Object::Boolean(boolean) => assert_eq!(expected, &boolean),
                something_else => panic!("Expected boolean, got {something_else}"),
            }
        };

        test_util::assert_list(input_expected, asserter);
    }

    #[test]
    fn eval_bang_operator_test() {
        let input_expected: Vec<(&str, bool)> = vec![
            ("!true", false),
            ("!false", true),
            ("!!true", true),
            ("!!false", false),
        ];

        test_util::assert_list(input_expected, |expected: &bool, input: &&str| {
            let object = test_util::expect_evaled_program(input);

            match object {
                Object::Boolean(boolean) => assert_eq!(expected, &boolean),
                something_else => panic!("Expected boolean, got {something_else}"),
            }
        });
    }

    #[test]
    fn eval_if_else_expression_test() {
        test_util::setup_logger();

        let input_expected: Vec<(&str, i32)> = vec![
            ("if true: 10~", 10),
            ("if false: 10 else: 5~", 5),
            ("if 1 < 2: 10~", 10),
            ("if 1 > 2: 10 else: 5~", 5),
            ("if 1 == 2: 10 else: 5~", 5),
            ("if 1 != 2: 10 else: 5~", 10),
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
    fn eval_string_literal_test() {
        let input_expected: Vec<(&str, &str)> = vec![("\"hello world\"", "hello world")];

        test_util::assert_list(input_expected, |expected: &&str, input: &&str| {
            let object = test_util::expect_evaled_program(input);

            match object {
                Object::Str(string) => assert_eq!(expected, &string),
                something_else => panic!("Expected boolean, got {something_else}"),
            }
        });
    }
}
