pub mod diagnostics;
pub mod document_change;
mod document_open;
mod formatting;
pub mod initialize;

use std::io::{Stdout, Write};
use std::process::exit;

use anyhow::anyhow;
use anyhow::Result;
use lsp_types::{
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    DocumentDiagnosticParams, DocumentFormattingParams, FullDocumentDiagnosticReport,
    InitializeParams, InitializedParams, RelatedFullDocumentDiagnosticReport,
};
use serde::{de::DeserializeOwned, Serialize};
use serde_json::from_str;
use tracing::{event, Level};

use crate::document_store::DocumentStore;
use crate::rpc::{self, LspRequest, Request, Response};

pub struct SonOfAnton {
    writer: Stdout,
    document_store: DocumentStore,
}

impl SonOfAnton {
    pub fn from(writer: Stdout) -> Self {
        Self {
            writer,
            document_store: DocumentStore::new(),
        }
    }

    pub fn handle_message(&mut self, lsp_request: LspRequest) -> Result<()> {
        match lsp_request.request.method.as_str() {
            "initialize" => {
                let params: InitializeParams =
                    deserialize_request::<InitializeParams>(&lsp_request)?;

                let client_name = match params.client_info {
                    Some(info) => info.name,
                    None => "Unknown client".to_owned(),
                };

                event!(Level::INFO, "Initializing with client: {client_name}");
                let resp = initialize::handle_initialize();
                self.send_response(lsp_request, resp)?;
            }
            "textDocument/didOpen" => {
                let params: DidOpenTextDocumentParams = deserialize_request(&lsp_request)?;
                document_open::handle_open(params, self);
            }
            "textDocument/didChange" => {
                let params: DidChangeTextDocumentParams = deserialize_request(&lsp_request)?;
                document_change::handle_change(params, self)?;
            }
            "textDocument/didSave" => {
                let params: DidSaveTextDocumentParams = deserialize_request(&lsp_request)?;
                // TODO: Decide what to do on save
            }
            "textDocument/formatting" => {
                let params: DocumentFormattingParams = deserialize_request(&lsp_request)?;

                let formatted_text = formatting::handle_formatting(params, self)?;
                self.send_response(lsp_request, formatted_text);
            }
            "textDocument/diagnostic" => {
                let params: DocumentDiagnosticParams = deserialize_request(&lsp_request)?;

                let diagnostics = diagnostics::run_diagnostics(params.text_document.uri, self)?;
                let response = RelatedFullDocumentDiagnosticReport {
                    related_documents: None,
                    full_document_diagnostic_report: FullDocumentDiagnosticReport {
                        result_id: None,
                        items: diagnostics,
                    },
                };
                self.send_response(lsp_request, response)?;
            }
            "initialized" => {
                let _ = deserialize_request::<InitializedParams>(&lsp_request)?;
                event!(Level::INFO, "Server was initialized");
            }
            "shutdown" => {
                event!(Level::INFO, "Shutting down server by request of the client");
                self.send_response(lsp_request, serde_json::Value::Null)?;
            }
            "exit" => {
                event!(Level::INFO, "Exiting after confirmation by client");
                exit(0)
            }
            unknown_method => {
                event!(
                    Level::DEBUG,
                    "Received unknown request from server: method = {}",
                    unknown_method
                );
            }
        }

        Ok(())
    }

    fn send_response<T: Serialize>(
        &mut self,
        lsp_request: LspRequest,
        resp: T,
    ) -> Result<(), anyhow::Error> {
        let resp = Response {
            message: lsp_request.request.message,
            result: Some(resp),
        };
        let resp = rpc::encode_response(resp)?;
        event!(Level::DEBUG, "Sending response to server: {:?}", resp);
        self.writer.write_all(resp.as_bytes())?;
        self.writer.flush()?;
        Ok(())
    }
}

fn deserialize_request<T>(lsp_request: &LspRequest) -> Result<T>
where
    T: DeserializeOwned + std::fmt::Debug,
{
    let deserialized_request: Request<T> = from_str(&lsp_request.content)?;
    event!(
        Level::DEBUG,
        "Received request from server: {:#?}",
        deserialized_request
    );
    deserialized_request
        .params
        .ok_or_else(|| anyhow!("Expected params to be Some, but got None"))
}
