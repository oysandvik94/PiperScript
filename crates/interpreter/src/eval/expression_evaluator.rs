use std::collections::HashMap;

use tracing::{event, span, Level};

use crate::eval::objects::{HashPair, PrimitiveObject};
use crate::parser::expressions::if_expression::IfExpression;
use crate::parser::{
    ast::{Identifier, Operator, PrefixOperator},
    expressions::expression::Expression,
};

use super::builtin;
use super::{
    eval_error::EvalError,
    objects::{EnvReference, Object},
};

pub(crate) trait Evaluable {
    fn eval(&self, env: &mut EnvReference) -> Result<Object, EvalError>;
}

impl Evaluable for Expression {
    fn eval(&self, env: &mut EnvReference) -> Result<Object, EvalError> {
        match self {
            Expression::IntegerLiteral(number) => {
                event!(Level::DEBUG, "Evaluated to number {number}");
                Ok(Object::primitive_from_int(*number))
            }
            Expression::StringLiteral(string) => {
                event!(Level::DEBUG, "Evaluated to string {string}");
                Ok(Object::primitive_from_str(string.to_owned()))
            }
            Expression::BooleanLiteral(boolean) => {
                event!(Level::DEBUG, "Evaluated to boolean {boolean}");
                Ok(Object::primitive_from_bool(*boolean))
            }
            Expression::IdentifierLiteral(identifier) => {
                eval_identifier_expression(identifier, env)
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
            Expression::Array(array) => array.eval(env),
            Expression::Index { left, index } => eval_index_expression(left, index, env),
            Expression::HashLiteral(hash_vec) => eval_hash_literal(hash_vec, env),
        }
    }
}

fn eval_hash_literal(
    hash_vec: &[(Expression, Expression)],
    env: &mut EnvReference,
) -> Result<Object, EvalError> {
    let mut hash = HashMap::with_capacity(hash_vec.len());

    for (key_expr, value_expr) in hash_vec {
        let key = eval_hash_key(key_expr, env)?;
        let value = value_expr.eval(env)?;

        hash.insert(key.clone(), HashPair { key, value });
    }

    Ok(Object::Hash(hash))
}

fn eval_hash_key(expr: &Expression, env: &mut EnvReference) -> Result<PrimitiveObject, EvalError> {
    match expr.eval(env)? {
        Object::Primitive(prim) => Ok(prim),
        obj => Err(EvalError::InvalidHashKey(obj)),
    }
}

fn eval_index_expression(
    left: &Expression,
    index: &Expression,
    env: &mut EnvReference,
) -> Result<Object, EvalError> {
    match left.eval(env)? {
        Object::Array(array) => eval_index_array_expression(&array, index, env),
        Object::Hash(hash) => eval_index_hash_expression(&hash, index, env),
        unexpected_object => Err(EvalError::IndexingNonArray(unexpected_object)),
    }
}

fn eval_index_hash_expression(
    hash: &HashMap<PrimitiveObject, HashPair>,
    index: &Expression,
    env: &mut EnvReference,
) -> Result<Object, EvalError> {
    let index = match index.eval(env)? {
        Object::Primitive(index) => index,
        unexpected_object => {
            return Err(EvalError::NonHashableKey(unexpected_object));
        }
    };

    match hash.get(&index) {
        Some(hash_pair) => Ok(hash_pair.value.clone()),
        None => Err(EvalError::NonResolvedKey(index)),
    }
}

fn eval_index_array_expression(
    array: &[Object],
    index: &Expression,
    env: &mut EnvReference,
) -> Result<Object, EvalError> {
    let index = match index.eval(env)? {
        Object::Primitive(PrimitiveObject::Integer(index)) => index,
        unexpected_object => {
            return Err(EvalError::IndexNotInteger(unexpected_object));
        }
    };

    match array.get(index as usize) {
        Some(indexed_object) => Ok(indexed_object.clone()),
        None => Err(EvalError::IndexOutOfBounds(array.len(), index)),
    }
}

fn eval_identifier_expression(
    identifier: &Identifier,
    env: &EnvReference,
) -> Result<Object, EvalError> {
    if let Some(object) = env.borrow().get_identifier(&identifier.0) {
        return Ok(object);
    }

    if let Some(object) = builtin::lookup_builtins(&identifier.0) {
        return Ok(object);
    }

    Err(EvalError::IdentifierNotFound(identifier.clone()))
}
impl Evaluable for IfExpression {
    fn eval(&self, env: &mut EnvReference) -> Result<Object, EvalError> {
        let expression_statement_span = span!(Level::DEBUG, "If");
        let _enter = expression_statement_span.enter();
        event!(Level::DEBUG, "Evaluating if condition");

        let evaluated_condition = match self.condition.eval(env)? {
            Object::Primitive(PrimitiveObject::Boolean(boolean)) => boolean,
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
    use PrimitiveObject::*;

    match (left, right) {
        (Primitive(Integer(left)), Primitive(Integer(right))) => {
            eval_integer_infix_expression(left, right, operator)
        }

        (Primitive(Boolean(left)), Primitive(Boolean(right))) => {
            eval_boolean_infix_expression(left, right, operator)
        }

        (Primitive(Str(left)), Primitive(Str(right))) => {
            eval_string_infix_expression(left, right, operator)
        }

        (unexpected_left, unexpected_right) => {
            Err(EvalError::InfixRightLeft(unexpected_left, unexpected_right))
        }
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
            Object::primitive_from_str(concatted_str)
        }
        Operator::Equals => Object::primitive_from_bool(left_str == right_str),
        Operator::NotEquals => Object::primitive_from_bool(left_str != right_str),
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
    Ok(match operator {
        Operator::Equals => Object::primitive_from_bool(left_boolean == right_boolean),
        Operator::NotEquals => Object::primitive_from_bool(left_boolean != right_boolean),
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
    Ok(match operator {
        Operator::Minus => Object::primitive_from_int(left_integer - right_integer),
        Operator::Plus => Object::primitive_from_int(left_integer + right_integer),
        Operator::Multiply => Object::primitive_from_int(left_integer * right_integer),
        Operator::DividedBy => Object::primitive_from_int(left_integer / right_integer),
        Operator::LessThan => Object::primitive_from_bool(left_integer < right_integer),
        Operator::GreaterThan => Object::primitive_from_bool(left_integer > right_integer),
        Operator::Equals => Object::primitive_from_bool(left_integer == right_integer),
        Operator::NotEquals => Object::primitive_from_bool(left_integer != right_integer),
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
        Object::Primitive(PrimitiveObject::Integer(integer_value)) => {
            Ok(Object::primitive_from_int(-integer_value))
        }
        unexpected_object => Err(EvalError::IncorrectBangSuffix(unexpected_object.clone())),
    }
}

fn eval_bang_operator_expression(right: &Object) -> Result<Object, EvalError> {
    match right {
        Object::Primitive(PrimitiveObject::Boolean(boolean_value)) => {
            Ok(Object::primitive_from_bool(!boolean_value))
        }
        unexpected_object => Err(EvalError::IncorrectBangSuffix(unexpected_object.clone())),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        eval::{
            self,
            eval_error::EvalError,
            objects::{Environment, Object, PrimitiveObject},
            EvaledProgram,
        },
        test_util,
    };

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
                Object::Primitive(PrimitiveObject::Integer(number)) => {
                    assert_eq!(&number, expected)
                }
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
            (r#""test" == "test""#, true),
            (r#""ok" == "test""#, false),
        ];

        let asserter = |expected: &bool, input: &&str| {
            let object = test_util::expect_evaled_program(input);

            match object {
                Object::Primitive(PrimitiveObject::Boolean(boolean)) => {
                    assert_eq!(expected, &boolean)
                }
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
                Object::Primitive(PrimitiveObject::Boolean(boolean)) => {
                    assert_eq!(expected, &boolean)
                }
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
                Object::Primitive(PrimitiveObject::Integer(integer)) => {
                    assert_eq!(expected, &integer)
                }
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
                Object::Primitive(PrimitiveObject::Str(string)) => assert_eq!(expected, &string),
                something_else => panic!("Expected boolean, got {something_else}"),
            }
        });
    }

    #[test]
    fn eval_array_literal_test() {
        let input = "[1, 2 * 2, 3 + 3]";

        let object = test_util::expect_evaled_program(input);

        match object {
            Object::Array(array) => {
                assert_eq!(array.len(), 3);
                test_util::assert_integar_literal(&array[0], 1);
                test_util::assert_integar_literal(&array[1], 4);
                test_util::assert_integar_literal(&array[2], 6);
            }
            something_else => panic!("Expected boolean, got {something_else}"),
        }
    }

    #[test]
    fn indexing_array_shoud_produce_correct_element() {
        let test_cases = vec![
            ("[1, 2, 3][0]", 1),
            ("[1, 2, 3][1]", 2),
            ("[1, 2, 3][2]", 3),
            ("let i: 0. [1][i]", 1),
            ("[1, 2, 3][1 + 1]", 3),
            ("let myArray: [1, 2, 3]. myArray[2]", 3),
            (
                "let myArray: [1, 2, 3]. myArray[0] + myArray[1] + myArray[2]",
                6,
            ),
            ("let myArray: [1, 2, 3]. let i: myArray[0]. myArray[i]", 2),
        ];

        for test_case in test_cases {
            let object = test_util::expect_evaled_program(test_case.0);
            test_util::assert_integar_literal(&object, test_case.1);
        }
    }

    #[test]
    fn indexing_out_of_bounds_produces_error() {
        let test_cases = vec![("[1, 2, 3][3]", 3, 3), ("[1, 2, 3][-1]", 3, -1)];

        for test_case in test_cases {
            match eval::eval(test_case.0, &mut Environment::new_env_reference()) {
                EvaledProgram::EvalError(eval_error) => match eval_error {
                    EvalError::IndexOutOfBounds(size, indexed) => {
                        assert_eq!(size, test_case.1, "Reported size should be correct");
                        assert_eq!(indexed, test_case.2, "Reported index should be correct");
                    }
                    _ => panic!("Expected index out of bounds error"),
                },
                _ => panic!("Expected eval error"),
            }
        }
    }

    #[test]
    fn hash_with_string_key_should_eval() {
        let input = r#"
        let two: "two"
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee": 6 / 2,
        }
        "#;

        let object = test_util::expect_evaled_program(input);

        match object {
            Object::Hash(hash) => {
                assert_eq!(hash.len(), 3);

                let expected = vec![("one", 1), ("two", 2), ("three", 3)];

                for (key, value) in expected {
                    let key = PrimitiveObject::Str(String::from(key));

                    let fail_msg = format!("Expected key not found: {key}");
                    hash.get(&key).expect(&fail_msg);

                    let hash_value = &hash[&key];
                    test_util::assert_integar_literal(&hash_value.value, value);
                }
            }
            unexpected => panic!("Expected hash, got {unexpected}"),
        }
    }

    #[test]
    fn hash_with_int_key_should_eval() {
        let input = r#"
        let six: 6
        {
            4: 10 - 9,
            six: 1 + 1,
        }
        "#;

        let object = test_util::expect_evaled_program(input);

        match object {
            Object::Hash(hash) => {
                assert_eq!(hash.len(), 2);

                let expected = vec![(4, 1), (6, 2)];

                for (key, value) in expected {
                    let key = PrimitiveObject::Integer(key);

                    let fail_msg = format!("Expected key not found: {key}");
                    hash.get(&key).expect(&fail_msg);

                    let hash_value = &hash[&key];
                    test_util::assert_integar_literal(&hash_value.value, value);
                }
            }
            unexpected => panic!("Expected hash, got {unexpected}"),
        }
    }

    #[test]
    fn index_hash_test() {
        let input = vec![
            (r#"{"foo": 5}["foo"]"#, 5),
            (r#"let key: "foo". {"foo": 5}[key]"#, 5),
            (r#"{5: 5}[5]"#, 5),
            (r#"{true: 5}[true]"#, 5),
            (r#"{false: 5}[false]"#, 5),
        ];

        for (input, expected) in input {
            let object = test_util::expect_evaled_program(input);
            test_util::assert_integar_literal(&object, expected);
        }
    }

    #[test]
    fn key_not_in_hash_should_fail() {
        let input = vec![
            (
                r#"{"foo": 5}["bar"]"#,
                PrimitiveObject::Str("bar".to_owned()),
            ),
            (r#"{}[5]"#, PrimitiveObject::Integer(5)),
        ];

        for (input, expected) in input {
            match eval::eval(input, &mut Environment::new_env_reference()) {
                EvaledProgram::EvalError(EvalError::NonResolvedKey(key)) => {
                    assert_eq!(key, expected);
                }
                _ => panic!("Expected eval error"),
            };
        }
    }
}
