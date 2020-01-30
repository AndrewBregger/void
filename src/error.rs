extern crate snafu;

use snafu::{Snafu};

#[derive(Debug)]
pub enum Error {
    ParseError,
    TypeError,
    IoError(std::io::Error),
    ArgumentError,
}
