use args::{PiperArgs, PiperCommands};
use clap::Parser;
use tracing_subscriber::FmtSubscriber;

mod args;

pub fn setup_logging() {
    let subscriber = FmtSubscriber::builder()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .finish();
    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");
}

pub fn execute_piper() -> Result<(), std::io::Error> {
    let cli = PiperArgs::parse();
    match &cli.command {
        PiperCommands::Repl => repl::execute_repl()?,
        PiperCommands::Run { filename } => filerunner::execute_file(filename)?,
    }

    Ok(())
}
