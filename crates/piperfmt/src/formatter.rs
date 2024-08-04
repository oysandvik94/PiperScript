use interpreter::parser::{ast::Statement, expressions::expression::Expression};

pub trait Format {
    fn format(&self) -> String;
}

impl Format for Statement {
    fn format(&self) -> String {
        match self {
            Statement::Assign(assign_statement) => {
                format!(
                    "let {}: {}",
                    assign_statement.identifier,
                    assign_statement.assignment.format()
                )
            }
            Statement::Return(_) => todo!(),
            Statement::Expression(_) => todo!(),
        }
    }
}

impl Format for Expression {
    fn format(&self) -> String {
        match self {
            Expression::IdentifierLiteral(_) => todo!(),
            Expression::StringLiteral(_) => todo!(),
            Expression::IntegerLiteral(integer) => integer.to_string(),
            Expression::BooleanLiteral(_) => todo!(),
            Expression::Array(_) => todo!(),
            Expression::Index { left: _, index: _ } => todo!(),
            Expression::Prefix {
                right: _,
                operator: _,
            } => todo!(),
            Expression::Infix {
                left: _,
                right: _,
                operator: _,
            } => todo!(),
            Expression::If(_) => todo!(),
            Expression::Function(_) => todo!(),
            Expression::Call(_) => todo!(),
            Expression::HashLiteral(_) => todo!(),
        }
    }
}
