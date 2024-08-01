pub const ANTON_CAPABILITIES: ServerCapabilities = ServerCapabilities {
    text_document_sync: Some(TextDocumentSyncCapability::Kind(
        TextDocumentSyncKind::INCREMENTAL,
    )),
    completion_provider: Some(CompletionOptions {
        resolve_provider: Some(false),
        trigger_characters: Some(vec![".".to_string()]),
        work_done_progress_options: Default::default(),
        all_commit_characters: None,
        ..Default::default()
    }),
    execute_command_provider: Some(ExecuteCommandOptions {
        commands: vec!["dummy.do_something".to_string()],
        work_done_progress_options: Default::default(),
    }),
    workspace: Some(WorkspaceServerCapabilities {
        workspace_folders: Some(WorkspaceFoldersServerCapabilities {
            supported: Some(true),
            change_notifications: Some(OneOf::Left(true)),
        }),
        file_operations: None,
    }),
    hover_provider: Some(HoverProviderCapability::Simple(true)),
    ..ServerCapabilities::default()
};
