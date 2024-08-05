pub mod initialize;

use std::io::{Stdout, Write};

use anyhow::Result;
use initialize::{InitalizeParams, InitializeResponse};
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
                let _ = fun_name::<InitalizeParams>(&lsp_request)?;

                let resp: InitializeResponse = initialize::handle_initialize();
                self.send_response(lsp_request, resp)?;
            }
            _ => {
                event!(Level::DEBUG, "Received unknown request from server",);
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

fn fun_name<T>(lsp_request: &LspRequest) -> Result<Option<T>>
where
    T: DeserializeOwned + std::fmt::Debug,
{
    let deserialized_request: Request<T> = from_str(&lsp_request.content)?;
    event!(
        Level::DEBUG,
        "Received request from server: {:?}",
        deserialized_request
    );
    Ok(deserialized_request.params)
}
