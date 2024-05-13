use lexer::token::Token;

use crate::{ast::Expression, parse_errors::ParseError};

trait InfixParser {
    fn parse_infix(&self, left: Expression) -> Option<Result<Expression, ParseError>>;
}

impl InfixParser for Token {
    fn parse_infix(&self, left: Expression) -> Option<Result<Expression, ParseError>> {
        match self {
            Token::Add => {
                todo!()
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expression, Identifier, Operator, Program, Statement},
        test_util::{check_parser_errors, create_prefix_test_case, parse_program},
    };

    #[test]
    fn test_integer_expression() {
        let input: &str = "5";

        let program: Program = parse_program(input);
        check_parser_errors(&program);

        let parsed_statement = program
            .statements
            .first()
            .expect("Should only have one statement");

        assert!(matches!(
            parsed_statement,
            Statement::ExpressionStatement(Expression::IntegerExpression(5))
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

    #[test]
    fn test_parse_prefix() {
        struct TestCase {
            input: String,
            statement: Statement,
        }

        let test_cases: [TestCase; 3] = [
            (
                "!5",
                create_prefix_test_case(Expression::IntegerExpression(5), Operator::Bang),
            ),
            (
                "!foo",
                create_prefix_test_case(
                    Expression::IdentifierExpression(Identifier("foo".to_string())),
                    Operator::Bang,
                ),
            ),
            (
                "-15",
                create_prefix_test_case(Expression::IntegerExpression(15), Operator::Minus),
            ),
        ]
        .map(|(input, statement)| TestCase {
            input: input.to_string(),
            statement,
        });

        for test_case in test_cases {
            let program: Program = parse_program(&test_case.input);
            check_parser_errors(&program);

            assert_eq!(program.statements.len(), 1, "Should only parse 1 statement");
            let statement = program.statements.first().expect("Should be one statement");

            assert_eq!(
                statement, &test_case.statement,
                "Parsed statement should match testcase"
            );
        }
    }
}
