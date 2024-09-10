use std::collections::HashMap;

use lsp_types::Uri;

pub struct DocumentStore {
    store: HashMap<Uri, String>,
}

use anyhow::{anyhow, Result};

impl DocumentStore {
    pub fn new() -> DocumentStore {
        DocumentStore {
            store: HashMap::new(),
        }
    }

    pub fn put_document(&mut self, uri: Uri, document: String) {
        self.store.insert(uri, document);
    }

    pub fn get_document(&self, uri: &Uri) -> Result<&String> {
        self.store
            .get(uri)
            .ok_or_else(|| anyhow!("Document not found for URI: {:?}", uri))
    }
}

impl Default for DocumentStore {
    fn default() -> Self {
        Self::new()
    }
}
