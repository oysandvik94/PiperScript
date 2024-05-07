use crate::parser::ParseError;

pub struct Program {
    pub statements: Vec<Statement>,
    pub parse_errors: Vec<ParseError>,
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    AssignStatement(Identifier, Expression),
    ReturnStatement(Expression),
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    TodoExpression,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Identifier(pub String);
