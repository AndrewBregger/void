use super::token::{Position, FilePos, Span};
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::string::ToString;

enum Severity {
    Info,
    Warning,
    Error,
    Critical,
}

impl ToString for Severity {
    fn to_string(&self) -> String {
        match self {
            Severity::Info => "Info",
            Severity::Warning => "Warning",
            Severity::Error => "Error",
            Severity::Critical => "Critical Error",
        }.to_string()
    }
}

struct Message {
    msg: String,
    sev: Severity,
    loc: Option<Position>
}

pub struct Diagnostics {
    total_errors: u32,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            total_errors: 0
        }
    }

    fn print_source_line(&self, loc: &Position) {
        // maybe move this somewhere else.
        let mut file = File::open(&loc.file.source).expect("Failed to find file");
        file.seek(SeekFrom::Start(loc.span.start as u64)).expect("Failed to seek into file");
        let mut buf = Vec::with_capacity(loc.span.len());
        file.read_exact(buf.as_mut_slice()).expect("Failed to read from file");

        let s = std::str::from_utf8(&buf).expect("Failed to transform buffer").trim_start();
        println!(">\t{}", s);
    }

    // fn print_message(&self, msg: &str) {
    // }

    fn print_message(&mut self, msg: &Message) {
        match &msg.loc {
            Some(loc) => {
                println!("{} | {}: {}", loc, msg.sev.to_string(), msg.msg);
                // self.print_source_line(&loc);
            }
            None => {
                println!("{}: {}", msg.sev.to_string(), msg.msg);
            }
        }
        self.total_errors += 1;
    }

    pub fn syntax_error(&mut self, msg: &str, loc: &Position) {
        let msg = Message {
            msg: msg.to_string(),
            sev: Severity::Error,
            loc: Some(loc.clone())
        };

        self.print_message(&msg);
    }
}
