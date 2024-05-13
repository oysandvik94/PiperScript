use lexer::{lexer::generate_tokens, token::Token};

use crate::{
    ast::{Expression, Identifier, Operator, Program, Statement},
    parser::Parser,
};

pub fn check_parser_errors(program: &Program) {
    if program.parse_errors.is_empty() {
        return;
    }

    eprintln!("Found parser errors:");
    for parse_error in &program.parse_errors {
        eprintln!("parser error: {parse_error}");
    }

    panic!("Test failed because of parser errors");
}

pub fn parse_program(source_code: &str) -> Program {
    let tokens: Vec<Token> = generate_tokens(source_code);
    let mut parser: Parser = Parser::new(tokens);
    parser.parse_program()
}

pub fn create_prefix_test_case(right_expression: Expression, operator: Operator) -> Statement {
    Statement::ExpressionStatement(Expression::PrefixExpression {
        right: Box::new(right_expression),
        operator,
    })
}
