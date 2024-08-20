use std::fmt::Display;

use crate::{
    eval::objects::Listable,
    parser::{
        lexer::token::{Token, TokenKind},
        parse_errors::ParseError,
        Parser,
    },
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
    pub fn parse(parser: &mut Parser) -> Result<(Expression, Vec<Token>), ParseError> {
        let (elements, tokens) = Expression::parse_expression_list(parser, &TokenKind::RBracket)?;
        let array_literal = Expression::Array(ArrayLiteral { elements });
        Ok((array_literal, tokens))
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Borrow;

    use crate::{
        parser::{
            ast::{Identifier, Operator, StatementType},
            expressions::expression::Expression,
        },
        test_util,
    };

    #[test]
    fn test_array_parsing() {
        let input = "[1, 2 * 2, 3 + 3]";

        let statemens = test_util::expect_parsed_program(input);
        let statement = statemens.first().expect("Should only return one statement");

        let array_expression = match &statement.statement_type {
            StatementType::Expression(expression) => match expression {
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

    #[test]
    fn test_index_array_parsing() {
        test_util::setup_logger();
        let input = "myArray[1 + 1]";

        let statemens = test_util::expect_parsed_program(input);
        let statement = statemens.first().expect("Should only return one statement");

        let (left, index) = match &statement.statement_type {
            StatementType::Expression(expression) => match expression {
                Expression::Index { left, index } => (left, index),
                unexpected => panic!("Expected index expression, but got {unexpected}"),
            },
            _ => panic!("Expected expression statement"),
        };

        match left.borrow() {
            Expression::IdentifierLiteral(Identifier(name)) => assert_eq!(name, "myArray"),
            unexpected => panic!("Expected identifier expression, got {unexpected}"),
        }

        match index.borrow() {
            Expression::Infix {
                right,
                left,
                operator,
            } => {
                assert_eq!(operator, &Operator::Plus);
                assert_eq!(right, &Box::new(Expression::IntegerLiteral(1)));
                assert_eq!(left, &Box::new(Expression::IntegerLiteral(1)));
            }
            unexpected => panic!("Expected integer expression, got {unexpected}"),
        }
    }
}
