use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct PiperArgs {
    #[command(subcommand)]
    pub command: PiperCommands,
}

#[derive(Subcommand)]
pub enum PiperCommands {
    Repl,
    Run { filename: String },
    Check { filename: String },
}
