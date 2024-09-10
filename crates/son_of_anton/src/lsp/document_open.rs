use lsp_types::DidOpenTextDocumentParams;
use tracing::{event, Level};

use super::SonOfAnton;

pub fn handle_open(params: DidOpenTextDocumentParams, lsp: &mut SonOfAnton) {
    let uri = params.text_document.uri;
    event!(Level::DEBUG, "Got open: {:?}", uri);

    lsp.document_store
        .put_document(uri, params.text_document.text);
}
