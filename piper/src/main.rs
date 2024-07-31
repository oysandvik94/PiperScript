use std::env;

use tracing_subscriber::FmtSubscriber;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();

    let subscriber = FmtSubscriber::builder()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .finish();
    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    if args.len() == 2 {
        let filename = &args[1];
        filerunner::execute_file(filename)?;
    } else {
        repl::execute_repl()?;
    }

    Ok(())
}
