use super::compiler::FileRef;
use crate::syntax::token::{Position};
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::path::PathBuf;
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
        }
        .to_string()
    }
}

struct Message {
    msg: String,
    sev: Severity,
    loc: Option<Position>,
}

pub struct Diagnostics {
    pub total_errors: u32,
    pub files: HashMap<PathBuf, FileRef>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            total_errors: 0,
            files: HashMap::new(),
        }
    }

    fn print_source_line(&self, loc: &Position) {
        if let Some(file) = self.files.get(&loc.file.source) {
            println!("");
            println!(">\t{}", file.line_from_pos(loc));
            println!("");
        }
        else {
            self.fatal(format!("Compiler error: unable to find file at path: {}", loc.file.source.display()).as_str())
        }
        
    }

    // fn print_message(&self, msg: &str) {
    // }

    fn print_message(&mut self, msg: &Message) {
        match &msg.loc {
            Some(loc) => {
                println!("{} | {}: {}", loc, msg.sev.to_string(), msg.msg);
                self.print_source_line(&loc);
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
            loc: Some(loc.clone()),
        };

        self.print_message(&msg);
    }

    pub fn type_error(&mut self, msg: &str, loc: Position) {
        let msg = Message {
            msg: msg.to_string(),
            sev: Severity::Error,
            loc: Some(loc.clone()),
        };

        self.print_message(&msg);
    }

    pub fn fatal(&self, msg: &str) {
        print!("Fatal Compiler Error: ");
        println!("{}", msg);
        std::process::exit(1);
    }

    pub fn info(&self, msg: &str) {
        println!("\tInfo: {}", msg);
    }

    pub fn info_with_line(&self, msg: &str, pos: Position) {
        self.info(msg);
        self.print_source_line(&pos);
    }
}
