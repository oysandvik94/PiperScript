use std::{fs, path::PathBuf, sync::Once};

use interpreter::parser::{ParsedProgram, Parser};
use tracing_subscriber::FmtSubscriber;

static TRACING: Once = Once::new();
pub fn setup_logger() {
    let subscriber = FmtSubscriber::builder()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .finish();

    TRACING.call_once(|| {
        tracing::subscriber::set_global_default(subscriber)
            .expect("setting default subscriber failed");
    })
}

#[test]
fn test_error_message() {
    setup_logger();

    let mut test_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    test_file_path.push("tests/test_error.las");
    let file_name = test_file_path.to_str().unwrap();
    let file_content = fs::read_to_string(file_name).expect("Should be able to read string");

    let mut parser = Parser::new(&file_content);
    let program = parser.parse_program();

    let expected_error_messages = [
        "Error on line 2: expected binding to let statement\n",
        "Error on line 4: expected identifier in let statement\n",
    ];

    match program {
        ParsedProgram::InvalidProgram(parse_errors) => {
            if parse_errors.len() != 2 {
                for ele in parse_errors {
                    println!("{ele}");
                }
                panic!("Expected 2 parse errors, but got the above ones instead");
            }
            for (idx, expected_message) in expected_error_messages.iter().enumerate() {
                assert_eq!(
                    expected_message,
                    &parse_errors.get(idx).unwrap().to_string()
                );
            }
        }
        _ => panic!("Should return error"),
    }
}
