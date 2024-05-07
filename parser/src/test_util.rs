use crate::ast::Program;

#[cfg(test)]
pub fn check_parser_errors(program: &Program) {
    if program.parse_errors.is_empty() {
        return;
    }

    eprintln!("Found parser errors:");
    for parse_error in &program.parse_errors {
        eprintln!("parser error: {:?}", parse_error);
    }

    panic!("Test failed because of parses errors");
}
