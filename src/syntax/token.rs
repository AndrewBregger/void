use std::path::PathBuf;
use std::string::ToString;
use std::cmp::{PartialEq, Eq};
use std::fmt::Display;

use super::ast::AstNode;

#[derive(Debug, Clone)]
pub struct FilePos {
    pub line: usize,
    pub column: usize,
    pub source: PathBuf
}

impl FilePos {
    pub fn new(line: usize, column: usize, source: PathBuf) -> Self {
        Self {
            line,
            column,
            source
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end
        }
    }

    /// extends the span to include 'token' and everything between.
    pub fn extend_to(&mut self, token: &Token) {
        self.end = token.pos.span.end;
    }

    pub fn extend_node(&mut self, node: &dyn AstNode) {
        let pos = node.pos();
        self.end = pos.span.end;
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} - {})", self.start, self.end)
    }
}


#[derive(Debug, Clone)]
pub struct Position {
    pub file: FilePos,
    pub span: Span
}

impl Position {
    pub fn new(file: FilePos, span: Span) -> Self {
        Self {
            file,
            span
        }
    }

    pub fn line(&self) -> usize {
        self.file.line
    }

    pub fn column(&self) -> usize {
        self.file.column
    }

    pub fn len(&self) -> usize {
        self.span.len()
    }

    pub fn extend(&mut self, pos: &Position) {
        self.span.end = pos.span.end;
    }
}

// impl ToString for Position {
//     fn to_string(&self) -> String {
//         format!("{}:{}:{}", self.line(), self.column(), self.file.source.display())
//     }
// }

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}:{}:{}", self.line(), self.column(), self.file.source.display())?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Plus,
    Minus,
    Backslash,
    Astrick,

    AstrickAstrick,
    Modulos,
    Equal,

    // boolean operations
    Less,
    Greater,
    Bang,
    EqualEqual,
    BangEqual,

    LessLess,
    GreaterGreater,

    Pipe,
    Ampersand,
    Carot,
    Tilde,

    PlusEqual,
    MinusEqual,
    BackslashEqual,
    AstrickEqual,

    AstrickAstrickEqual,
    ModulosEqual,

    LessEqual,
    GreaterEqual,

    PipeEqual,
    AmpersandEqual,
    CarotEqual,
    TildeEqual,

    LessLessEqual,
    GreaterGreaterEqual,

    Dollar,
    Question,

}

impl Op {
    pub fn prec(&self) -> usize {
        match self {
            Op::AstrickAstrick => 14,
            Op::Backslash |
            Op::Astrick |
            Op::Modulos => 13,
            Op::Plus |
            Op::Minus => 12,
            Op::LessLess |
            Op::GreaterGreater => 11,
            Op::Less |
            Op::Greater |
            Op::EqualEqual |
            Op::BangEqual => 10,
            Op::Ampersand => 9,
            Op::Carot => 8,
            Op::Tilde => 7,
            Op::Pipe => 6,
            // and => 5
            // or  => 4
            Op::Bang => 3,
            Op::Equal |
            Op::PlusEqual |
            Op::MinusEqual |
            Op::BackslashEqual |
            Op::AstrickEqual |
            Op::AstrickAstrickEqual |
            Op::ModulosEqual |
            Op::LessEqual |
            Op::GreaterEqual |
            Op::PipeEqual |
            Op::AmpersandEqual |
            Op::CarotEqual |
            Op::TildeEqual |
            Op::LessLessEqual |
            Op::GreaterGreaterEqual => 2,
            Op::Dollar |
            Op::Question => 1,
        }
    }
}

impl ToString for Op {
    fn to_string(&self) -> String {
        match self {
            Op::Plus => "+",
            Op::Minus => "-",
            Op::Backslash => "/",
            Op::Astrick => "*",
            Op::AstrickAstrick => "**",
            Op::Modulos => "%",
            Op::Equal => "=",
            Op::Less => "<",
            Op::Greater => ">",
            Op::Bang => "!",
            Op::EqualEqual => "==",
            Op::BangEqual => "!=",
            Op::LessLess => "<<",
            Op::GreaterGreater => ">>",
            Op::Pipe => "|",
            Op::Ampersand => "&",
            Op::Carot => "^",
            Op::Tilde => "~",
            Op::PlusEqual => "+=",
            Op::MinusEqual => "-=",
            Op::BackslashEqual => "/=",
            Op::AstrickEqual => "*=",
            Op::AstrickAstrickEqual => "**=",
            Op::ModulosEqual => "%=",
            Op::LessEqual => "<=",
            Op::GreaterEqual => ">=",
            Op::PipeEqual => "|=",
            Op::AmpersandEqual => "&=",
            Op::CarotEqual => "^=",
            Op::TildeEqual => "~=",
            Op::LessLessEqual => "<<=",
            Op::GreaterGreaterEqual => ">>=",
            Op::Dollar => "$",
            Op::Question => "?",
        }.to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kw {
    If,
    Else,
    Elif,
    For,
    While,
    Loop,
    With,
    Trait,
    As,
    And,
    Or,
    Use,
    Fn,
    Struct,
    Let,
    Mut,
    Pub,
    In,
}

impl Kw {
    pub fn prec(&self) -> usize {
        match self {
            Kw::And => 5,
            Kw::Or => 4,
            _ => 0,
        }
    }

}

impl ToString for Kw {
    fn to_string(&self) -> String {
        match self {
            Kw::If =>     "if",
            Kw::Else =>   "else",
            Kw::Elif =>   "elif",
            Kw::For =>    "for",
            Kw::While =>  "while",
            Kw::Loop =>   "loop",
            Kw::With =>   "with",
            Kw::As =>     "as",
            Kw::And =>    "and",
            Kw::Or =>     "or",
            Kw::Trait =>  "triat",
            Kw::Use =>    "use",
            Kw::Fn =>     "fn",
            Kw::Struct => "struct",
            Kw::Let =>    "let",
            Kw::Mut =>    "mut",
            Kw::Pub =>    "pub",
            Kw::In =>     "in",
        }.to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Orientation {
    Left,
    Right
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ctrl {
    Paren(Orientation),
    Bracket(Orientation),
    Brace(Orientation),
    Period,
    Colon,
    Comma,
    Semicolon,
    Hash,
    MinusGreater,
    LessMinus,
    EqualLess,
    Underscore,
}

impl ToString for Ctrl {
    fn to_string(&self) -> String {
        match self {
            Ctrl::Paren(or) => match or {
                Orientation::Left => "(".to_string(),
                Orientation::Right => ")".to_string(),
            },
            Ctrl::Bracket(or) => match or {
                Orientation::Left => "{".to_string(),
                Orientation::Right => "}".to_string(),
            },
            Ctrl::Brace(or) => match or {
                Orientation::Left => "[".to_string(),
                Orientation::Right => "]".to_string(),
            },
            Ctrl::Period => ".".to_string(),
            Ctrl::Colon => ":".to_string(),
            Ctrl::Comma => ",".to_string(),
            Ctrl::Semicolon => ";".to_string(),
            Ctrl::Hash => "#".to_string(),
            Ctrl::MinusGreater => "->".to_string(),
            Ctrl::LessMinus => "<-".to_string(),
            Ctrl::EqualLess => "=>".to_string(),
            Ctrl::Underscore => "_".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    IntegerLiteral(u64),
    FloatLiteral(f64),
    CharLiteral(char),
    StringLiteral(String),
    Identifier(String),

    Operator(Op),
    Keyword(Kw),
    Control(Ctrl),

    Eof,
    Error,
    Comment,
}

impl TokenKind {
    pub fn new_ident(val: &str) -> Self {
        TokenKind::Identifier(val.to_string())
    }

    pub fn new_integer(val: u64) -> Self {
        TokenKind::IntegerLiteral(val)
    }

    pub fn new_float(val: f64) -> Self {
        TokenKind::FloatLiteral(val)
    }

    pub fn new_char(val: char) -> Self {
        TokenKind::CharLiteral(val)
    }

    pub fn new_string(val: &str) -> Self {
        TokenKind::StringLiteral(val.to_string())
    }

    pub fn is_identifier(&self) -> bool {
        match *self {
            TokenKind::Identifier(_) => true,
            _ => false,
        }
    }

    pub fn is_operator(&self) -> bool {
        match self {
            TokenKind::Operator(_) => true,
            _ => false,
        }
    }

    pub fn is_assignment(&self) -> bool {
        match self {
            TokenKind::Operator(op) =>
                match op {
                    Op::Equal |
                    Op::PlusEqual |
                    Op::MinusEqual |
                    Op::BackslashEqual |
                    Op::AstrickEqual |
                    Op::AstrickAstrickEqual |
                    Op::ModulosEqual |
                    Op::LessEqual |
                    Op::GreaterEqual |
                    Op::PipeEqual |
                    Op::AmpersandEqual |
                    Op::CarotEqual |
                    Op::TildeEqual |
                    Op::LessLessEqual |
                    Op::GreaterGreaterEqual => true,
                    _ => false,
                },
            _ => false
        }
    }


    pub fn is_keyword(&self) -> bool {
        match self {
            TokenKind::Keyword(_) => true,
            _ => false,
        }
    }

    pub fn is_literal(&self) -> bool {
        match self {
            TokenKind::CharLiteral(_) |
            TokenKind::StringLiteral(_) |
            TokenKind::IntegerLiteral(_) |
            TokenKind::FloatLiteral(_)
            => true,
            _ => false,
        }
    }


    pub fn is_control(&self) -> bool {
        match self {
            TokenKind::Control(_) => true,
            _ => false,
        }
    }

    pub fn get_ident_kind(val: &str) -> Self {
        match val {
            "if" => TokenKind::Keyword(Kw::If),
            "else" => TokenKind::Keyword(Kw::Else),
            "elif" => TokenKind::Keyword(Kw::Elif),
            "for" => TokenKind::Keyword(Kw::For),
            "while" => TokenKind::Keyword(Kw::While),
            "loop" => TokenKind::Keyword(Kw::Loop),
//            "use" => TokenKind::Keyword(Kw::Use),
            "with" => TokenKind::Keyword(Kw::With),
            "as" => TokenKind::Keyword(Kw::As),
            "and" => TokenKind::Keyword(Kw::And),
            "or" => TokenKind::Keyword(Kw::Or),
            "use"    => TokenKind::Keyword(Kw::Use),
            "fn"     => TokenKind::Keyword(Kw::Fn),
            "struct" => TokenKind::Keyword(Kw::Struct),
            "let"    => TokenKind::Keyword(Kw::Let),
            "mut"    => TokenKind::Keyword(Kw::Mut),
            "pub"    => TokenKind::Keyword(Kw::Pub),
            "trait"  => TokenKind::Keyword(Kw::Trait),
            "in"     => TokenKind::Keyword(Kw::In),
            _ => TokenKind::Identifier(val.to_string()),
        }
    }

    pub fn list_to_str(kinds: &[TokenKind]) -> String {
        let mut res = String::new();

        let strings = kinds.iter().map(|kind| {
            match kind {
                TokenKind::Identifier(_) => "identifier".to_string(),
                _ => kind.to_string(),
            }
        }).collect::<Vec<String>>();

        strings.join(", ")
    }
}

impl ToString for TokenKind {
    fn to_string(&self) -> String {
        match self {
            TokenKind::IntegerLiteral(val) => val.to_string(),
            TokenKind::FloatLiteral(val) => val.to_string(),
            TokenKind::CharLiteral(ch) => ch.to_string(),
            TokenKind::StringLiteral(val) => val.clone(),
            TokenKind::Identifier(val) => val.clone(),
            TokenKind::Operator(op) => op.to_string(),
            TokenKind::Keyword(kw) => kw.to_string(),
            TokenKind::Control(ctrl) => ctrl.to_string(),
            TokenKind::Error => "Error".to_string(),
            TokenKind::Comment => "Comment".to_string(),
            TokenKind::Eof => "Eof".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    kind: TokenKind,
    pos: Position
}


impl Token {
    pub fn new(kind: TokenKind, pos: Position) -> Self {
        Self {
            kind,
            pos
        }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn pos(&self) -> &Position {
        &self.pos
    }

    pub fn prec(&self) -> usize {
        match &self.kind {
            TokenKind::Operator(op) => op.prec(),
            TokenKind::Keyword(kw) => kw.prec(),
            _ => 0,
        }
    }

    pub fn is_keyword(&self) -> bool {
        self.kind.is_keyword()
    }

    pub fn is_operator(&self) -> bool {
        self.kind.is_operator()
    }

    pub fn is_assignment(&self) -> bool {
        self.kind.is_assignment()
    }

    pub fn is_eof(&self) -> bool {
        self.check(TokenKind::Eof)
    }


    pub fn check(&self, kind: TokenKind) -> bool {
        match (self.kind(), &kind) {
            (TokenKind::Identifier(_), TokenKind::Identifier(_)) => true,
            (TokenKind::Operator(op1), TokenKind::Operator(op2)) => *op1 == *op2,
            (TokenKind::Keyword(kw1), TokenKind::Keyword(kw2)) => *kw1 == *kw2,
            (TokenKind::Control(ct1), TokenKind::Control(ct2)) => *ct1 == *ct2,
            (TokenKind::IntegerLiteral(_), TokenKind::IntegerLiteral(_)) => true,
            (TokenKind::FloatLiteral(_), TokenKind::FloatLiteral(_)) => true,
            (TokenKind::CharLiteral(_), TokenKind::CharLiteral(_)) => true,
            (TokenKind::StringLiteral(_), TokenKind::StringLiteral(_)) => true,
            (TokenKind::Comment, TokenKind::Comment) => true,
            (TokenKind::Eof, TokenKind::Eof) => true,
            (_, _) => false,
        }
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        self.kind().to_string()
    }
}
