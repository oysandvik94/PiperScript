use lexer::token::Token;

use crate::ast::Program;

fn parse_program(tokens: Vec<Token>) -> Program {
    todo!()
}

#[cfg(test)]
mod tests {
    use lexer::lexer::generate_tokens;

    use crate::ast::{Identifier, Program, Statement};

    use super::{parse_program, Token};

    #[test]
    fn parse_let_statement() {
        let source_code = "
            ~x: 5~
            ~y: 10~
            ~foobar: 54456~
        ";

        let tokens: Vec<Token> = generate_tokens(source_code);
        let program: Program = parse_program(tokens);

        assert_eq!(
            program.statements.len(),
            3,
            "Program should be parsed to 3 statements"
        );

        let expected_identifiers: [Identifier; 3] = [
            Identifier(String::from("x")),
            Identifier(String::from("y")),
            Identifier(String::from("foobar")),
        ];

        expected_identifiers
            .iter()
            .enumerate()
            .for_each(|(idx, ident)| test_let_statement(&program.statements[idx], ident));
    }

    fn test_let_statement(found: &Statement, expected_identifier: &Identifier) {
        match found {
            Statement::LetStatement(found_identfier, _) => {
                assert_eq!(found_identfier, expected_identifier)
            }
            incorrect => panic!("Expected let-statement, but got {incorrect:?}"),
        };
    }
}
