use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct PiperArgs {
    #[command(subcommand)]
    pub command: PiperCommands,

    #[arg(long, global = true)]
    pub use_vm: bool,
}

#[derive(Subcommand)]
pub enum PiperCommands {
    Repl,
    Run { filename: String },
    Check { filename: String },
}
