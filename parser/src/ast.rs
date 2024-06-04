use std::fmt::Display;

use lexer::token::Token;

use crate::{
    lexer,
    parse_errors::{ParseError, TokenExpectation},
};

pub struct Program {
    pub statements: Vec<Statement>,
    pub parse_errors: Vec<ParseError>,
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    AssignStatement(Identifier, Expression),
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
}

#[derive(PartialEq, Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    IdentifierLiteral(Identifier),
    IntegerLiteral(i32),
    BooleanLiteral(bool),
    PrefixExpression {
        right: Box<Expression>,
        operator: Operator,
    },
    InfixExpression {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: Operator,
    },
    IfExpression {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    FunctionLiteral {
        parameters: Vec<Identifier>,
        body: BlockStatement,
    },
    CallExpression {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

#[derive(PartialEq, Debug)]
pub enum Operator {
    Bang,
    Minus,
    Plus,
    Multiply,
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    DividedBy,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn parse_from_token(value: &Token) -> Result<Identifier, ParseError> {
        match value {
            Token::Ident(ident_literal) => Ok(Identifier(ident_literal.to_string())),
            unexpected_token => Err(ParseError::UnexpectedToken {
                expected_token: TokenExpectation::SingleExpectation(Token::Ident("".to_string())),
                found_token: Some(unexpected_token.clone()),
            }),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            writeln!(f, "{}", statement)?;
        }

        Ok(())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::AssignStatement(ident, expression) => write!(f, "~{ident}: {expression}~"),
            Statement::ReturnStatement(expression) => write!(f, "return {expression}"),
            Statement::ExpressionStatement(expression) => write!(f, "{expression}"),
        }
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.statements
            .iter()
            .try_for_each(|statement| write!(f, "{}", statement))
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::IdentifierLiteral(ident) => write!(f, "{ident}"),
            Expression::IntegerLiteral(integerd_literal) => write!(f, "{integerd_literal}"),
            Expression::PrefixExpression { right, operator } => write!(f, "({operator}{right})"),
            Expression::InfixExpression {
                left,
                right,
                operator,
            } => write!(f, "({left} {operator} {right})"),
            Expression::BooleanLiteral(boolean) => write!(f, "{boolean}"),
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                write!(f, "if {condition}: {consequence}")?;
                match alternative {
                    Some(found_alternative) => {
                        write!(f, " else: {found_alternative}")
                    }
                    None => Ok(()),
                }
            }
            Expression::FunctionLiteral { parameters, body } => {
                let parameters: Vec<String> =
                    parameters.iter().map(|ident| ident.0.clone()).collect();
                write!(f, "fn({}): {}", parameters.join(","), body)
            }
            Expression::CallExpression {
                function,
                arguments,
            } => {
                let arguments: Vec<String> =
                    arguments.iter().map(|ident| ident.to_string()).collect();
                write!(f, "{function}({})", arguments.join(", "))
            }
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Bang => write!(f, "!"),
            Operator::Minus => write!(f, "-"),
            Operator::Plus => write!(f, "+"),
            Operator::Multiply => write!(f, "*"),
            Operator::Equals => write!(f, "=="),
            Operator::NotEquals => write!(f, "!="),
            Operator::GreaterThan => write!(f, ">"),
            Operator::LessThan => write!(f, "<"),
            Operator::DividedBy => write!(f, "/"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Identifier};

    use super::{Program, Statement};

    #[test]
    fn test_display() {
        let program: Program = Program {
            statements: Vec::from([
                Statement::AssignStatement(
                    Identifier("foo".to_string()),
                    Expression::IdentifierLiteral(Identifier("bar".to_string())),
                ),
                Statement::ReturnStatement(Expression::IdentifierLiteral(Identifier(
                    "hey".to_string(),
                ))),
            ]),
            parse_errors: Vec::new(),
        };

        let expected_program: &str = "~foo: bar~
return hey
";

        assert_eq!(expected_program, format!("{program}"));
    }
}
