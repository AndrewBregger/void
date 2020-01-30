use crate::diagnostics::Diagnostics;
use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;
use std::rc::Rc;

use super::error::Error;
use super::semantics;
use super::syntax::*;

pub type Result<T> = ::std::result::Result<T, Error>;

pub struct File {
    content: String,
    lines: Vec<String>,
}

pub type FileRef = Rc<File>;

impl File {
    pub fn from_path(path: &PathBuf) -> io::Result<Self> {
        let mut file = fs::File::open(path)?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;

        let lines: Vec<String> = content.as_str().lines().map(|s| s.to_string()).collect();
        Ok(File { content, lines })
    }
}

pub struct Compiler {
    source: PathBuf,
    diagnostics: Diagnostics,
}

impl Compiler {
    pub fn new(source: PathBuf) -> Result<Self> {
        Ok(Self {
            source,
            diagnostics: Diagnostics::new(),
        })
    }

    fn load_file<T: Into<PathBuf>>(&mut self, path: T) -> Result<FileRef> {
        let path: PathBuf = path.into();
        let file = File::from_path(&path).map_err(|e| Error::IoError(e))?;
        let file = FileRef::new(file);
        self.diagnostics.files.insert(path, file.clone());
        Ok(file)
    }

    pub fn compile_source(&mut self) -> Result<()> {
        let file = self.load_file(self.source.clone())?;

        let mut parser = match Parser::new(
            file.content.as_str(),
            self.source.clone(),
            &mut self.diagnostics,
        ) {
            Ok(parser) => parser,
            Err(err) => match err {
                parser::Error::ParseError => return Err(Error::ParseError),
                parser::Error::OtherError(val) => {
                    println!("Error: {}", val);
                    return Err(Error::ParseError);
                }
            },
        };

        if let Ok(parsed_file) = parser.parse_file() {
            parsed_file.render(0);
            let mut semantic = semantics::Semantics::new(&mut self.diagnostics, "prelude/mod.rs");
            semantic
                .check_program(&parsed_file)
                .map_err(|_e| Error::TypeError)?;
            Ok(())
        } else {
            Err(Error::ParseError)
        }
    }
}
