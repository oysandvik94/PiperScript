use anyhow::anyhow;
use anyhow::Result;
use lsp_types::DidChangeTextDocumentParams;

use super::SonOfAnton;

pub fn handle_change(params: DidChangeTextDocumentParams, lsp: &mut SonOfAnton) -> Result<()> {
    let document_change = params
        .content_changes
        .first()
        .ok_or_else(|| anyhow!("Change event contained no changes"))?;

    lsp.document_store
        .put_document(params.text_document.uri, document_change.text.clone());

    Ok(())
}
