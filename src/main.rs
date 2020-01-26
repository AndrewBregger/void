mod syntax;

use std::io::Read;
use std::fs::File;
use std::path::PathBuf;
use std::str::FromStr;

use syntax::*;

fn load_file<T: Into<PathBuf>>(path: T) -> std::io::Result<(String, PathBuf)> {
   let path = path.into();
    let mut file = File::open(&path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    Ok((content, path))
}

fn main() -> std::io::Result<()> {
    let (content, path) = load_file("./tests/struct_000.vo")?;

    let mut parser = Parser::new(content.as_str(), path).ok().unwrap();
    match parser.parse_file() {
        Ok(nodes) => {
            for node in &nodes {
                node.render(0);
            }
        },
        Err(err) => println!("{:?}", err),
    }

    Ok(())
}
