use anyhow::Result;
use lsp_types::{DocumentFormattingParams, Position, Range, TextDocumentIdentifier, TextEdit};
use piperfmt::format_snippet;

use super::SonOfAnton;

pub fn handle_formatting(
    params: DocumentFormattingParams,
    lsp: &mut SonOfAnton,
) -> Result<TextEdit> {
    let text_identifier: TextDocumentIdentifier = params.text_document;
    let text = lsp.document_store.get_document(&text_identifier.uri)?;

    let formatted_text = format_snippet(text);
    Ok(TextEdit {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 2,
                character: 0,
            },
        },
        new_text: formatted_text,
    })
}
