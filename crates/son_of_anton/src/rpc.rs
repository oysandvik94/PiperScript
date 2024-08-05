use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use tracing::{event, Level};

#[derive(Serialize, Deserialize, Debug)]
pub struct BaseMessage {
    id: u64,
    jsonrpc: String,
}

pub struct LspRequest {
    pub request: BaseRequest,
    pub content: String,
}

impl LspRequest {
    pub fn new(message: BaseRequest, content: String) -> LspRequest {
        LspRequest {
            request: message,
            content,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct BaseRequest {
    #[serde(flatten)]
    pub message: BaseMessage,
    pub method: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Request<T> {
    #[serde(flatten)]
    pub request: BaseRequest,
    pub params: Option<T>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Response<T> {
    #[serde(flatten)]
    pub message: BaseMessage,
    pub result: Option<T>,
}

pub fn encode_response<T: Serialize>(message: Response<T>) -> Result<String> {
    let content = serde_json::to_string(&message).context("Could not serialize message")?;
    event!(Level::DEBUG, "Responding with {}", content);

    let resp = format!("Content-Length: {}\r\n\r\n{}", content.len(), content);
    Ok(resp)
}

#[cfg(test)]
mod tests {
    use crate::rpc::{BaseMessage, Response};

    use super::encode_response;

    #[test]
    fn test_encode() {
        let expected =
            "Content-Length: 40\r\n\r\n{\"id\":1,\"jsonrpc\":\"2.0\",\"result\":\"test\"}";
        let actual = encode_response(Response {
            message: BaseMessage {
                id: 1,
                jsonrpc: "2.0".to_string(),
            },
            result: Some("test".to_string()),
        })
        .unwrap();

        assert_eq!(expected, actual);
    }
}
