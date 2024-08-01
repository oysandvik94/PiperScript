use tower_lsp::lsp_types::{Hover, HoverContents, HoverParams, MarkedString};

pub fn handle_hover(_: HoverParams) -> Hover {
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(
            "Thanks you for hovering".to_string(),
        )),
        range: None,
    }
}
