use lexer::token::TokenType;

use crate::parser::Parser;

trait PrefixParser {
    fn parse(&self, parser: Parser);
}

impl PrefixParser for TokenType {
    fn parse(&self, parser: Parser) {
        todo!()
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
            "Shoul only have parsed one expression statement"
        );

        let parsed_statement = program
            .statements
            .get(0)
            .expect("Already checked length")
            .clone();

        assert!(matches!(
            parsed_statement,
            Statement::ExpressionStatement(crate::ast::Expression::IdentifierExpression(
                Identifier(ident)
            )) if ident == "foobar"
        ));
    }
}
