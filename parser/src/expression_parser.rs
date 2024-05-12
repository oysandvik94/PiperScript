use lexer::token::Token;

use crate::{
    ast::{Expression, Identifier},
    parser::Parser,
};

pub trait PrefixParser {
    fn parse(&self, parser: &Parser) -> Option<Expression>;
}

impl PrefixParser for Token {
    fn parse(&self, parser: &Parser) -> Option<Expression> {
        match self {
            Token::Ident(literal) => Some(Expression::IdentifierExpression(Identifier(literal.to_string()))),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Identifier, Program, Statement},
        parser::Parser,
        test_util::check_parser_errors,
    };

    #[test]
    fn test_identifier_expression() {
        let input: &str = "foobar";

        let tokens = lexer::lexer::generate_tokens(input);
        let mut parser = Parser::new(tokens);
        let program: Program = parser.parse_program();

        check_parser_errors(&program);

        assert_eq!(
            1,
            program.statements.len(),
            "Should only have parsed one expression statement"
        );

        let parsed_statement = program.statements.first().expect("Already checked length");

        assert!(matches!(
            parsed_statement,
            Statement::ExpressionStatement(crate::ast::Expression::IdentifierExpression(
                Identifier(ident)
            )) if ident == "foobar"
        ));
    }
}
