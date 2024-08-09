use std::io::{self, BufRead};

use anyhow::{anyhow, Result};
use serde_json::from_str;
use son_of_anton::{
    logging::setup_logging,
    lsp::SonOfAnton,
    rpc::{BaseRequest, LspRequest},
};
use tracing::{event, Level};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let _guard = setup_logging()?;

    event!(Level::INFO, "Starting son of anton");

    let mut reader = io::stdin().lock();
    let mut anton = SonOfAnton::from(io::stdout());

    loop {
        let mut buffer = String::new();
        reader.read_line(&mut buffer)?;

        if buffer.trim().is_empty() {
            continue;
        }

        match process_message(&mut buffer.as_bytes(), &mut anton) {
            Ok(()) => {
                continue;
            }
            Err(e) => {
                event!(Level::ERROR, "Error processing message: {:?}", e);
            }
        }
    }
}

fn process_message(
    reader: &mut impl BufRead,
    anton: &mut SonOfAnton,
) -> Result<(), Box<dyn std::error::Error>> {
    let content_length = read_until_content_length(reader)?;
    read_until_carriage_return(reader);

    let request = read_message(content_length, reader)?;
    anton.handle_message(request)?;
    Ok(())
}

fn read_message(content_length: u64, reader: &mut impl BufRead) -> Result<LspRequest> {
    let content = read_content(content_length, reader)?;

    let decoded_request: BaseRequest = from_str(&content)?;
    event!(
        Level::DEBUG,
        "Received message from server: {:?}",
        decoded_request
    );

    Ok(LspRequest {
        request: decoded_request,
        content,
    })
}

fn read_content(content_length: u64, reader: &mut impl BufRead) -> Result<String, anyhow::Error> {
    let mut buffer = vec![0; content_length as usize];
    reader.read_exact(&mut buffer)?;
    let content = String::from_utf8(buffer)?;
    Ok(content)
}

fn read_until_carriage_return(reader: &mut impl BufRead) {
    for line in reader.lines().map_while(Result::ok) {
        if line.is_empty() {
            return;
        }
    }
}

fn read_until_content_length(reader: &mut impl BufRead) -> Result<u64> {
    for line in reader.lines() {
        event!(Level::INFO, "finding content length");
        let line = line?;

        let content_length = parse_headers(&line);
        if let Some(content_length) = content_length {
            return Ok(content_length);
        }
    }

    Err(anyhow!("Never retrieved content length"))
}

fn parse_headers(line: &str) -> Option<u64> {
    let header = line.split(":").collect::<Vec<&str>>();
    if header.len() == 2 {
        let key = header[0].trim();
        if key == "Content-Length" {
            let parsed_header = header[1].trim().parse::<usize>();
            event!(Level::DEBUG, "split header: {:?}", parsed_header);
            if let Ok(parsed_header) = parsed_header {
                return Some(parsed_header as u64);
            }
        }
    };

    None
}
