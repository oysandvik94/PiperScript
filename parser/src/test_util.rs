use tracing_subscriber::FmtSubscriber;

use crate::{
    ast::{BlockStatement, Identifier, Operator, Program, Statement},
    expressions::{
        expression::Expression, expression_statement::ExpressionStatement,
        functions::FunctionLiteral, if_expression::IfExpression,
    },
    lexer::lexedtokens::LexedTokens,
    parser::Parser,
};

pub fn has_parser_errors(program: &Program) -> bool {
    if program.parse_errors.is_empty() {
        return false;
    }

    eprintln!("Found parser errors:");
    for parse_error in &program.parse_errors {
        eprintln!("parser error: {parse_error}");
    }

    true
}

pub fn parse_program(source_code: &str) -> Program {
    let tokens = LexedTokens::from(source_code);
    let mut parser: Parser = Parser::new(tokens);
    parser.parse_program()
}

pub fn create_prefix_test_case(right_expression: Expression, operator: Operator) -> Statement {
    Statement::Expression(ExpressionStatement {
        expression: Expression::PrefixExpression {
            right: Box::new(right_expression),
            operator,
        },
    })
}

pub fn create_infix_test_case(
    left_expression: Expression,
    right_expression: Expression,
    operator: Operator,
) -> Statement {
    Statement::Expression(ExpressionStatement {
        expression: Expression::InfixExpression {
            left: Box::new(left_expression),
            right: Box::new(right_expression),
            operator,
        },
    })
}

pub fn create_function_expression(parameters: Vec<&str>, body: BlockStatement) -> Statement {
    Statement::Expression(ExpressionStatement {
        expression: Expression::Function(FunctionLiteral {
            parameters: parameters
                .iter()
                .map(|param| Identifier(param.to_string()))
                .collect(),
            body,
        }),
    })
}
pub fn create_if_condition(
    condition: Expression,
    consequence: BlockStatement,
    alternative: Option<BlockStatement>,
) -> Statement {
    use Expression::*;
    Statement::Expression(ExpressionStatement {
        expression: If(IfExpression {
            condition: Box::from(condition),
            consequence,
            alternative,
        }),
    })
}

pub fn create_infix_expression(
    left: Expression,
    right_expression: Expression,
    operator: Operator,
) -> Expression {
    Expression::InfixExpression {
        right: Box::from(right_expression),
        left: Box::from(left),
        operator,
    }
}

pub fn create_identifierliteral(literal: &str) -> Expression {
    Expression::IdentifierLiteral(Identifier(literal.to_string()))
}

pub fn setup_logger() {
    let subscriber = FmtSubscriber::builder()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");
}
