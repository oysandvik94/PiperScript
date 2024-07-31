use piperscript::{execute_piper, setup_logging};

fn main() -> Result<(), std::io::Error> {
    setup_logging();

    execute_piper()
}
