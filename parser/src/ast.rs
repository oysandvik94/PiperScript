pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    LetStatement(Identifier, Expression),
    ReturnStatement(Expression),
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    TodoExpression,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Identifier(pub String);
