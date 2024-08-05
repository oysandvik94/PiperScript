use anyhow::Result;
use tracing_appender::non_blocking::WorkerGuard;
use tracing_subscriber::FmtSubscriber;

pub fn setup_logging() -> Result<WorkerGuard> {
    let file_appender = tracing_appender::rolling::never("/tmp", "anton_log");
    let (non_blocking, _guard) = tracing_appender::non_blocking(file_appender);

    let subscriber = FmtSubscriber::builder()
        .with_writer(non_blocking)
        .with_env_filter("debug")
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    Ok(_guard)
}
