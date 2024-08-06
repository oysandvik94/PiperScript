use std::fs;

use anyhow::Result;
use formatter::Format;
use interpreter::parser::{ParsedProgram, Parser};

mod formatter;

pub fn format_file(filename: &str) -> Result<String> {
    let file_content = fs::read_to_string(filename)?;
    Ok(parse_and_format(&file_content))
}

pub fn format_snippet(snippet: &str) -> String {
    parse_and_format(snippet)
}

fn parse_and_format(code: &str) -> String {
    let mut parser = Parser::new(code);
    let parsed_program = parser.parse_program();

    match parsed_program {
        ParsedProgram::ValidProgram(statements) => statements
            .iter()
            .map(|statement| statement.format())
            .collect(),
        ParsedProgram::InvalidProgram(_) => todo!(),
    }
}
