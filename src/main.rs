mod syntax;

use std::io::Read;
use std::fs::File;
use std::path::PathBuf;
use std::str::FromStr;

use syntax::*;

fn main() -> std::io::Result<()> {
    // let path = PathBuf::from_str("./tests/scanner_test.vo").unwrap();
    // let mut file = File::open(&path)?;
    // let mut content = String::new();
    // file.read_to_string(&mut content);

    // let test_input = "x.y + 1.0.to_string + y.x(1, 2)";
    let test_input = "let (x, y) = 1.0";
    // let test_input = "1.0";
    // let test_input = "x";

    let mut parser = Parser::new(test_input, PathBuf::new()).ok().unwrap();

    match parser.parse_item() {
        Ok(node) => node.render(0),
        Err(err) => println!("{:?}", err),
    }

    Ok(())
}
