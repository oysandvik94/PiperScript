use lsp_types::{HoverProviderCapability, InitializeResult, ServerCapabilities, ServerInfo};

pub fn handle_initialize() -> InitializeResult {
    InitializeResult {
        capabilities: ServerCapabilities {
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            ..Default::default()
        },
        server_info: Some(ServerInfo {
            name: "Son of Anton".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
    }
}
