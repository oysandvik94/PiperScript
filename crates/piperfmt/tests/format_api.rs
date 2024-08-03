use std::path::PathBuf;

use piperfmt::{format_file, format_snippet};

#[test]
fn file_should_be_formatted() {
    let mut test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    test_file_path.push("tests/test_format.las");

    let formatted_file =
        format_file(test_file_path.to_str().unwrap()).expect("Should succeed in formatting");
    assert_eq!(formatted_file, "let a: 8");
}

#[test]
fn snippet_should_be_formatted() {
    let formatted_snippet = format_snippet("let    a : 8");
    assert_eq!(formatted_snippet, "let a: 8");
}
