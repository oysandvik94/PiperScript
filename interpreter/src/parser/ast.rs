use std::fmt::Display;

use super::{
    assign_statement::AssignStatement,
    expressions::{
        expression::Expression, expression_statement::ExpressionStatement,
        functions::CallExpression, if_expression::IfExpression,
    },
    lexer::token::Token,
    parse_errors::ParseError,
    return_statement::ReturnStatement,
    ParsedProgram,
};

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Assign(AssignStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[derive(PartialEq, Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum PrefixOperator {
    Bang,
    Minus,
}

#[derive(PartialEq, Debug, Clone)]
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
            unexpected_token => Err(ParseError::single_unexpected(
                &Token::Ident(String::from("")),
                Some(unexpected_token),
            )),
        }
    }
}

impl Display for ParsedProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsedProgram::ValidProgram(statements) => {
                for statement in statements {
                    writeln!(f, "{}", statement)?;
                }
            }
            ParsedProgram::InvalidProgram(errors) => {
                for error in errors {
                    writeln!(f, "{}", error)?;
                }
            }
        }

        Ok(())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Assign(assign_statement) => write!(f, "{assign_statement}"),
            Statement::Return(expression) => write!(f, "{expression}"),
            Statement::Expression(expression) => write!(f, "{expression}"),
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
            Expression::IntegerLiteral(integer_literal) => write!(f, "{integer_literal}"),
            Expression::Prefix { right, operator } => write!(f, "({operator}{right})"),
            Expression::Infix {
                left,
                right,
                operator,
            } => write!(f, "({left} {operator} {right})"),
            Expression::BooleanLiteral(boolean) => write!(f, "{boolean}"),
            Expression::If(IfExpression {
                condition,
                consequence,
                alternative,
            }) => {
                write!(f, "if {condition}: {consequence}")?;
                match alternative {
                    Some(found_alternative) => {
                        write!(f, " else: {found_alternative}")
                    }
                    None => Ok(()),
                }
            }
            Expression::Function(function_literal) => {
                write!(f, "{function_literal}")
            }
            Expression::Call(CallExpression {
                function,
                arguments,
            }) => {
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

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOperator::Bang => write!(f, "!"),
            PrefixOperator::Minus => write!(f, "-"),
        }
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

    use crate::parser::{
        assign_statement::AssignStatement, ast::Identifier, expressions::expression::Expression,
        return_statement::ReturnStatement, test_util, ParsedProgram,
    };

    use super::Statement;

    #[test]
    fn test_display() {
        let program: ParsedProgram = ParsedProgram::ValidProgram(Vec::from([
            Statement::Assign(AssignStatement {
                identifier: Identifier(String::from("foo")),
                assignment: Expression::IdentifierLiteral(Identifier(String::from("bar"))),
            }),
            Statement::Return(ReturnStatement {
                return_value: test_util::create_identifierliteral("hey"),
            }),
        ]));

        let expected_program: &str = "let foo: bar.
return hey
";

        assert_eq!(expected_program, format!("{program}"));
    }
}
