pub(crate) use anyhow::Result;

use interpreter::parser::{ParsedProgram, Parser};
use lsp_types::{Diagnostic, DiagnosticSeverity, Range, Uri};

use super::SonOfAnton;

pub fn run_diagnostics(uri: Uri, lsp: &mut SonOfAnton) -> Result<Vec<Diagnostic>> {
    let mut parser = Parser::new(lsp.document_store.get_document(&uri)?);
    let parse_result = parser.parse_program();

    Ok(match parse_result {
        ParsedProgram::ValidProgram(_) => Vec::with_capacity(0),
        ParsedProgram::InvalidProgram(errors) => errors
            .iter()
            .map(|error| {
                let error_location = &error.parse_error.token.location;
                let range = Range {
                    start: lsp_types::Position {
                        line: error_location.line_number - 1,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: error_location.line_number,
                        character: 0,
                    },
                };
                Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("piperscript".to_owned()),
                    message: error.to_string(),
                    ..Default::default()
                }
            })
            .collect(),
    })
}
