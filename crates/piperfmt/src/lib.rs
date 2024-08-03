use std::fs;

use anyhow::Result;
use formatter::Format;
use interpreter::parser::{lexer::lexedtokens::LexedTokens, ParsedProgram, Parser};

mod formatter;

pub fn format_file(filename: &str) -> Result<String> {
    let file_content = fs::read_to_string(filename)?;
    Ok(parse_and_format(&file_content))
}

pub fn format_snippet(snippet: &str) -> String {
    parse_and_format(snippet)
}

fn parse_and_format(code: &str) -> String {
    let tokens = LexedTokens::from(code);
    let parsed_program = Parser::parse_tokens(tokens);

    match parsed_program {
        ParsedProgram::ValidProgram(statements) => statements
            .iter()
            .map(|statement| statement.format())
            .collect(),
        ParsedProgram::InvalidProgram(_) => todo!(),
    }
}
