pub mod initialize;

use std::io::{Stdout, Write};

use anyhow::anyhow;
use anyhow::Result;
use lsp_types::{InitializeParams, InitializedParams};
use serde::{de::DeserializeOwned, Serialize};
use serde_json::from_str;
use tracing::{event, Level};

use crate::rpc::{self, LspRequest, Request, Response};

pub struct SonOfAnton {
    writer: Stdout,
}

impl SonOfAnton {
    pub fn from(writer: Stdout) -> Self {
        Self { writer }
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
            "initialized" => {
                let _ = deserialize_request::<InitializedParams>(&lsp_request)?;
                event!(Level::INFO, "Server was initialized");
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
        event!(
            Level::DEBUG,
            "Sending initialize response to server: {:?}",
            resp
        );
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
        "Received request from server: {:?}",
        deserialized_request
    );
    deserialized_request
        .params
        .ok_or_else(|| anyhow!("Expected params to be Some, but got None"))
}
