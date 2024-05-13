use lexer::token::Token;

use crate::{
    ast::{Expression, Identifier}, parse_errors::ParseError
};

pub trait PrefixParser {
    fn parse(&self) -> Option<Result<Expression, ParseError>>;
}

impl PrefixParser for Token {
    fn parse(&self) -> Option<Result<Expression, ParseError>> {
        match self {
            Token::Ident(literal) => Some(Ok(Expression::IdentifierExpression(Identifier(
                literal.to_string(),
            )))),
            Token::Int(integer_literal) => {
                match integer_literal.parse::<i32>() {
                    Ok(parsed_number) => Some(Ok(Expression::IntegerLiteral(parsed_number))),
                    Err(error) => Some(Err(ParseError::ParseIntegerError(error)))
                }
            }
            _ => None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expression, Identifier, Program, Statement},
        test_util::{check_parser_errors, parse_program},
    };

    #[test]
    fn test_integer_expression() {
        let input: &str = "5";

        let program: Program = parse_program(input);
        check_parser_errors(&program);

        let parsed_statement = program.statements.first().expect("Should only have one statement");

        assert!(matches!(
            parsed_statement,
            Statement::ExpressionStatement(Expression::IntegerLiteral(
                5
            )) 
        ));
    }

    #[test]
    fn test_identifier_expression() {
        let input: &str = "foobar";

        let program: Program = parse_program(input);
        check_parser_errors(&program);

        assert_eq!(
            1,
            program.statements.len(),
            "Should only have parsed one expression statement"
        );

        let parsed_statement = program.statements.first().expect("Already checked length");

        assert!(matches!(
            parsed_statement,
            Statement::ExpressionStatement(Expression::IdentifierExpression(
                Identifier(ident)
            )) if ident == "foobar"
        ));
    }
}
