use args::{PiperArgs, PiperCommands};
use clap::Parser;
use interpreter::{compiler::Compiler, parser::ParsedProgram, vm::VirtualMachine};
use tracing_subscriber::FmtSubscriber;

pub mod args;
mod filerunner;
mod repl;

pub fn setup_logging() {
    let subscriber = FmtSubscriber::builder()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .finish();
    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");
}

pub fn execute_piper() -> Result<(), std::io::Error> {
    let cli = PiperArgs::parse();
    match &cli.command {
        PiperCommands::Repl => repl::execute_repl(&cli)?,
        PiperCommands::Run { filename } => filerunner::execute_file(filename, &cli)?,
        PiperCommands::Check { filename } => filerunner::parse_file(filename)?,
    }

    Ok(())
}

pub fn execute_code(input: &str) {
    let mut parser = interpreter::parser::Parser::new(input);
    match parser.parse_program() {
        ParsedProgram::ValidProgram(ast) => {
            let mut compiler: Compiler = Compiler::default();
            compiler.compile(ast);
            let bytecode = compiler.bytecode();

            let mut vm = VirtualMachine::new(bytecode);
            vm.run().unwrap();

            let stack_elem = vm.stack_top();
            println!("{stack_elem}");
        }
        ParsedProgram::InvalidProgram(errors) => {
            eprintln!("Found parse errors:");
            errors.into_iter().for_each(|error| {
                eprintln!("{error}");
            });
        }
    }
}
