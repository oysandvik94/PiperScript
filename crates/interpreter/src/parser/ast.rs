use std::fmt::Display;

use crate::eval::objects::Listable;

use super::{
    assign_statement::AssignStatement,
    expressions::{expression::Expression, functions::CallExpression, if_expression::IfExpression},
    lexer::token::{Token, TokenKind},
    parse_errors::{ParseError, ParseErrorKind},
    return_statement::ReturnStatement,
    ParsedProgram,
};

#[derive(PartialEq, Debug, Clone)]
pub struct Statement {
    pub statement_type: StatementType,
    pub tokens: Vec<Token>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum StatementType {
    Assign(AssignStatement),
    Return(ReturnStatement),
    Expression(Expression),
}

#[derive(PartialEq, Debug, Clone)]
pub struct BlockStatement {
    pub statements: Vec<StatementType>,
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
        match &value.token_kind {
            TokenKind::Ident(ident_literal) => Ok(Identifier(ident_literal.to_string())),
            _ => Err(ParseError::new(
                value.clone(),
                ParseErrorKind::UnexpectedToken(TokenKind::Ident("".to_owned())),
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
        write!(f, "{}", self.statement_type)
    }
}

impl Display for StatementType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StatementType::Assign(assign_statement) => write!(f, "{assign_statement}"),
            StatementType::Return(expression) => write!(f, "{expression}"),
            StatementType::Expression(expression) => write!(f, "{expression}"),
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
            Expression::StringLiteral(string_literal) => write!(f, "\"{string_literal}\""),
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
                write!(f, "{function}({})", arguments.to_commaseperated_list())
            }
            Expression::Array(array_literal) => write!(f, "{array_literal}"),
            Expression::Index { left, index } => write!(f, "({left}[{index}])"),
            Expression::HashLiteral(hash) => {
                let keypairs = hash
                    .iter()
                    .map(|(x, y)| format!("{}: {}", x, y))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{{{keypairs}}}")
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

    use crate::{
        parser::{
            assign_statement::AssignStatement, ast::Identifier,
            expressions::expression::Expression, return_statement::ReturnStatement, ParsedProgram,
        },
        test_util,
    };

    use super::Statement;
    use super::StatementType;

    #[test]
    fn test_display() {
        let program: ParsedProgram = ParsedProgram::ValidProgram(Vec::from([
            Statement {
                statement_type: StatementType::Assign(AssignStatement {
                    identifier: Identifier(String::from("foo")),
                    assignment: Expression::IdentifierLiteral(Identifier(String::from("bar"))),
                }),
                tokens: vec![],
            },
            Statement {
                statement_type: StatementType::Return(ReturnStatement {
                    return_value: test_util::create_identifierliteral("hey"),
                }),
                tokens: vec![],
            },
        ]));

        let expected_program: &str = "let foo: bar.
return hey
";

        assert_eq!(expected_program, format!("{program}"));
    }
}
