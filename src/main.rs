extern crate clap;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::str::FromStr;

use void::compiler::{Compiler, Result};
use void::error::Error;
use void::syntax::*;

use clap::{App, Arg};

static MAJOR_VERSION: u32 = 0;
static MINOR_VERSION: u32 = 1;

fn main() -> Result<()> {
    let version_string = format!("{}.{}", MAJOR_VERSION, MINOR_VERSION);

    let matches = App::new("Void Language Compiler")
        .version(version_string.as_str())
        .author("Andrew Bregger <andrewbregger@tamu.edu>")
        .about("A compiler for the void programming langauge")
        .arg(
            Arg::with_name("input")
                .value_name("input")
                .help("root file of compilation")
                .required(false)
                .index(1),
        )
        .arg(
            Arg::with_name("version")
                .long("version")
                .help("gets version"),
        )
        .get_matches();

    if matches.is_present("version") {
        println!("Version: {}", version_string);
    } else {
        let source = if let Some(source) = matches.value_of("input") {
            PathBuf::from(source)
        } else {
            return Err(Error::ArgumentError);
        };

        let mut compiler = Compiler::new(source)?;
        compiler.compile_source()?;
    }

    Ok(())
}
