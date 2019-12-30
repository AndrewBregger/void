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

    let test_input = "x.y + 1.0.to_string";
    // let tokens = TokenStream::new(test_input, PathBuf::new()).collect::<Vec<Token>>();

    // for token in &tokens {
    //    println!("{:?}", token.to_string());
    // }

    let mut parser = Parser::new(test_input, PathBuf::new());

    match parser.parse_expr() {
        Ok(expr) => expr.render(0),
        Err(err) => println!("{}", err),
    }

    Ok(())
}
