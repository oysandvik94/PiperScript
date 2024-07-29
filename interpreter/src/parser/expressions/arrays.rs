use std::fmt::Display;

use crate::{
    eval::objects::Listable,
    parser::{lexer::token::Token, parse_errors::ParseError, Parser},
};

use super::expression::Expression;

#[derive(PartialEq, Debug, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.elements.to_commaseperated_list())
    }
}

impl ArrayLiteral {
    pub fn parse(parser: &mut Parser) -> Result<Expression, ParseError> {
        Ok(Expression::Array(ArrayLiteral {
            elements: Expression::parse_expression_list(parser, &Token::RBracket)?,
        }))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            ast::{Operator, Statement},
            expressions::expression::Expression,
        },
        test_util,
    };

    #[test]
    fn test_array_parsing() {
        let input = "[1, 2 * 2, 3 + 3]";

        let statemens = test_util::expect_parsed_program(input);
        let statement = statemens.first().expect("Should only return one statement");

        let array_expression = match statement {
            Statement::Expression(expression) => match expression {
                Expression::Array(array) => array,
                _ => panic!("Expected array expression"),
            },
            _ => panic!("Expected expression statement"),
        };

        match array_expression.elements[0] {
            Expression::IntegerLiteral(integer) => assert_eq!(integer, 1),
            _ => panic!("Expected integer expression"),
        }

        match &array_expression.elements[1] {
            Expression::Infix {
                right,
                left,
                operator,
            } => {
                assert_eq!(operator, &Operator::Multiply);
                assert_eq!(right, &Box::new(Expression::IntegerLiteral(2)));
                assert_eq!(left, &Box::new(Expression::IntegerLiteral(2)));
            }
            unexpected => panic!("Expected integer expression, got {unexpected}"),
        }

        match &array_expression.elements[2] {
            Expression::Infix {
                right,
                left,
                operator,
            } => {
                assert_eq!(operator, &Operator::Plus);
                assert_eq!(right, &Box::new(Expression::IntegerLiteral(3)));
                assert_eq!(left, &Box::new(Expression::IntegerLiteral(3)));
            }
            unexpected => panic!("Expected integer expression, got {unexpected}"),
        }
    }
}
