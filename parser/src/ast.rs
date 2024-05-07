use std::fmt::Display;

use crate::parser::ParseError;

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
pub enum Expression {
    TodoExpression,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Identifier(pub String);

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

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::TodoExpression => write!(f, ""),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

mod tests {
    use crate::ast::{Expression, Identifier};

    use super::{Program, Statement};

    #[test]
    fn test_display() {
        let program: Program = Program {
            statements: Vec::from([
                Statement::AssignStatement(
                    Identifier("foo".to_string()),
                    Expression::TodoExpression,
                ),
                Statement::ReturnStatement(
                    Expression::TodoExpression,
                ),
            ]),
            parse_errors: Vec::new(),
        };

        let expected_program: &str = "~foo: ~
return 
";

        assert_eq!(expected_program, format!("{program}"));
    }
}
