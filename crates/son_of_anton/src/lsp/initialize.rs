use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct InitalizeParams {
    client_info: Option<ClientInfo>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ClientInfo {
    pub name: String,
    pub version: Option<String>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct InitializeResponse {
    capabilities: ServerCapabilities,
    server_info: ServerInfo,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ServerCapabilities {
    hover_provider: Option<bool>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ServerInfo {
    name: String,
    version: String,
}

pub fn handle_initialize() -> InitializeResponse {
    InitializeResponse {
        capabilities: ServerCapabilities {
            hover_provider: Some(true),
        },
        server_info: ServerInfo {
            name: "Son of Anton".to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
        },
    }
}
