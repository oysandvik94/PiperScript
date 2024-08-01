use tower_lsp::lsp_types::{CompletionItem, CompletionParams, CompletionResponse};

pub fn handle_completion(_: CompletionParams) -> CompletionResponse {
    CompletionResponse::Array(vec![
        CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
        CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
    ])
}
