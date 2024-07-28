mod common;

#[test]
fn string_concatenation_test() {
    let source_code = r#""Hello" + "world""#;
    common::assert_eval(source_code, "Helloworld")
}
