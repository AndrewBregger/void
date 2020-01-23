use super::{token::{TokenKind, Token, Span, Position, FilePos, Op, Kw, Ctrl, Orientation}};
use std::path::PathBuf;

use std::iter::Iterator;
use std::str::Chars;

#[derive(Debug, Clone)]
pub struct TokenStream<'a> {
    it: Chars<'a>,
    source: &'a str,
    index: usize,
    line: usize,
    column: usize,
    file: PathBuf,
    at_eof: bool,
}

macro_rules! create_token {
    ($kind:expr, $start_column:expr, $start:expr, $stream:expr) => {{
        let pos = FilePos::new($stream.line, $start_column, $stream.file.clone());
        let span = Span::new($start, $stream.index);
        // println!("New Token Span: {}", span);
        Token::new($kind, Position::new(pos, span))
    }}
}

impl<'a> TokenStream<'a> {
    pub fn new(source: &'a str, file: PathBuf) -> Self {
        Self {
            it: source.chars(),
            source: source,
            index: 0usize,
            line: 1usize,
            column: 1usize,
            file,
            at_eof: false,
        }
    }

    fn bump(&mut self) {
        if self.index < self.source.len() {
            self.index += 1;
            self.column += 1;
            self.it.next();
        }
    }

    fn check_for(&mut self, ch: char) -> bool {
        match self.it.clone().next() {
            Some(c) => c == ch,
            _ => false
        }
    }

    fn parse_possible_ident(&mut self) -> Option<Token> {
        let start = self.index;
        let start_column = self.column;
        while self.it.clone().next().map_or(false, |e| e.is_alphanumeric() || e == '_') {
            self.bump();
        }

        let end = self.index;

        let value = &self.source[start..end];

        let kind = if value == "_" {
           TokenKind::Control(Ctrl::Underscore) 
        }
        else {
            TokenKind::get_ident_kind(value)
        };

        Some(create_token!(kind, start_column, start, self))
    }

    fn parse_number(&mut self) -> Option<Token> {
        let start = self.index;
        let start_column = self.column;

        while self.it.clone().next().map_or(false, |e| e.is_numeric()) {
            self.bump();
        }

        let mut is_float = false;

        if self.check_for('.') {

            if self.it.clone().skip(1).next().map_or(false, |e|
                e.is_numeric() || e == 'e' || e == 'E'
            ) {
                is_float = true;
                self.bump();
            }
            else {
                let value = &self.source[start..self.index];
                let value = value.parse::<u64>().unwrap();
                let kind = TokenKind::new_integer(value);

                return Some(create_token!(kind, start_column, start, self));
            }

            while self.it.clone().next().map_or(false, |e| e.is_numeric()) {
                self.bump();
            }
        }

        if self.check_for('e') || self.check_for('E') {
            is_float = true;
            self.bump();

            if self.check_for('-') || self.check_for('+') {
                self.bump();
            }

            while self.it.clone().next().map_or(false, |e| e.is_numeric()) {
                self.bump();
            }
        }

        let value = &self.source[start..self.index];

        if is_float {
            let value = value.parse::<f64>().unwrap();
            let kind = TokenKind::new_float(value);

            Some(create_token!(kind, start_column, start, self))
        }
        else {
            let value = value.parse::<u64>().unwrap();
            let kind = TokenKind::new_integer(value);

            Some(create_token!(kind, start_column, start, self))
        }
    }

    fn ignore_whitespace(&mut self) {
        loop {
            if let Some(ch) = self.it.clone().next() {
                if ch.is_whitespace() {
                    if ch == '\n' {
                        self.line += 1;
                        self.column = 0;
                    }

                    self.bump();
                }
                else {
                    break;
                }
            }
            else {
                break
            }
        }
    }

    fn parse_operator(&mut self, ch: char) -> Option<Token> {
        let start = self.index;
        let start_column = self.column;

        match ch {
            '"' => {
                self.bump();
                let start = self.index;
                let start_column = self.column;

                while self.it.clone().next().map_or(false, |x| x != '"') {
                    self.bump();
                }

                let kind = TokenKind::StringLiteral(String::from(&self.source[start..self.index]));

                let token = Some(create_token!(kind, start_column, start, self));
                self.bump();
                token
            },
            '+' => {
                self.bump();
                let kind = if self.check_for('=') {
                    self.bump();
                    TokenKind::Operator(Op::PlusEqual)
                }
                else {
                    TokenKind::Operator(Op::Plus)
                };

                Some(create_token!(kind, start_column, start, self))
            },
            '-' => {
                self.bump();
                let kind = if self.check_for('=') {
                    self.bump();
                    TokenKind::Operator(Op::MinusEqual)
                }
                else if self.check_for('>') {
                    self.bump();
                    TokenKind::Control(Ctrl::MinusGreater)
                }
                else {
                    TokenKind::Operator(Op::Minus)
                };

                Some(create_token!(kind, start_column, start, self))
            },
            '*' => {
                self.bump();
                let kind = if self.check_for('=') {
                    self.bump();
                    TokenKind::Operator(Op::AstrickEqual)
                }
                else if self.check_for('*') {
                    self.bump();

                    if self.check_for('=') {
                        TokenKind::Operator(Op::AstrickAstrickEqual)
                    }
                    else {
                        TokenKind::Operator(Op::AstrickAstrick)
                    }
                }
                else {
                    TokenKind::Operator(Op::Astrick)
                };

                Some(create_token!(kind, start_column, start, self))
            },
            '/' => {
                self.bump();
                let kind = if self.check_for('=') {
                    self.bump();
                    TokenKind::Operator(Op::BackslashEqual)
                }
                else {
                    TokenKind::Operator(Op::Backslash)
                };
                Some(create_token!(kind, start_column, start, self))
            },
            '%' => {
                self.bump();
                let kind = if self.check_for('=') {
                    self.bump();
                    TokenKind::Operator(Op::ModulosEqual)
                }
                else {
                    TokenKind::Operator(Op::Modulos)
                };

                Some(create_token!(kind, start_column, start, self))
            },
            '~' => {
                self.bump();

                let kind = TokenKind::Operator(Op::Tilde);
                Some(create_token!(kind, start_column, start, self))
            },
            '{' => {
                self.bump();
                let kind = TokenKind::Control(Ctrl::Bracket(Orientation::Left));
                Some(create_token!(kind, start_column, start, self))
            },

            '}' => {
                self.bump();
                let kind = TokenKind::Control(Ctrl::Bracket(Orientation::Right));
                Some(create_token!(kind, start_column, start, self))
            },
            '(' => {
                self.bump();
                let kind = TokenKind::Control(Ctrl::Paren(Orientation::Left));
                Some(create_token!(kind, start_column, start, self))
            },
            ')' => {
                self.bump();
                let kind = TokenKind::Control(Ctrl::Paren(Orientation::Right));
                Some(create_token!(kind, start_column, start, self))
            },
            '[' => {
                self.bump();
                let kind = TokenKind::Control(Ctrl::Brace(Orientation::Left));
                Some(create_token!(kind, start_column, start, self))
            },
            ']' => {
                self.bump();
                let kind = TokenKind::Control(Ctrl::Brace(Orientation::Right));
                Some(create_token!(kind, start_column, start, self))
            },
            '?' => {
                self.bump();
                let kind = TokenKind::Operator(Op::Question);
                Some(create_token!(kind, start_column, start, self))
            },
            ',' => {
                self.bump();
                let kind = TokenKind::Control(Ctrl::Comma);
                Some(create_token!(kind, start_column, start, self))
            },
            '.' => {
                self.bump();
                let kind = TokenKind::Control(Ctrl::Period);
                Some(create_token!(kind, start_column, start, self))
            },
            ':' => {
                self.bump();
                let kind = TokenKind::Control(Ctrl::Colon);
                Some(create_token!(kind, start_column, start, self))
            },
            ';' => {
                self.bump();
                let kind = TokenKind::Control(Ctrl::Semicolon);
                Some(create_token!(kind, start_column, start, self))
            },
            '|' => {
                self.bump();
                let kind = TokenKind::Operator(Op::Pipe);
                Some(create_token!(kind, start_column, start, self))
            },
            '&' => {
                self.bump();
                let kind = if self.check_for('=') {
                    TokenKind::Operator(Op::AmpersandEqual)
                }
                else {
                    TokenKind::Operator(Op::Ampersand)
                };

                Some(create_token!(kind, start_column, start, self))
            },
            '=' => {
                self.bump();
                let kind = if self.check_for('=') {
                    self.bump();
                    TokenKind::Operator(Op::EqualEqual)
                }
                else if self.check_for('>') {
                    self.bump();
                    TokenKind::Control(Ctrl::EqualLess)
                }
                else {
                    TokenKind::Operator(Op::Equal)
                };

                Some(create_token!(kind, start_column, start, self))
            },
            '<' => {
                self.bump();
                let kind = if self.check_for('=') {
                    self.bump();
                    TokenKind::Operator(Op::LessEqual)
                }
                else if self.check_for('-') {
                    self.bump();
                    TokenKind::Control(Ctrl::LessMinus)
                }
                else if self.check_for('<') {
                    self.bump();

                    if self.check_for('=') {
                        TokenKind::Operator(Op::LessLessEqual)
                    }
                    else {
                        TokenKind::Operator(Op::LessLess)
                    }
                }
                else {
                    TokenKind::Operator(Op::Less)
                };

                Some(create_token!(kind, start_column, start, self))
            },
            '>' => {
                self.bump();
                let kind = if self.check_for('=') {
                    self.bump();
                    TokenKind::Operator(Op::GreaterEqual)
                }
                else if self.check_for('>') {
                    self.bump();

                    if self.check_for('=') {
                        TokenKind::Operator(Op::GreaterGreaterEqual)
                    }
                    else {
                        TokenKind::Operator(Op::GreaterGreater)
                    }
                }
                else {
                    TokenKind::Operator(Op::Greater)
                };

                Some(create_token!(kind, start_column, start, self))
            },
            '!' => {
                self.bump();
                let kind = if self.check_for('=') {
                    self.bump();

                    TokenKind::Operator(Op::BangEqual)

                }
                else {
                    TokenKind::Operator(Op::Bang)
                };
                Some(create_token!(kind, start_column, start, self))
            },
            '#' => {
                self.bump();
                let kind = TokenKind::Control(Ctrl::Hash);
                Some(create_token!(kind, start_column, start, self))
                // self.bump();
                // while self.it.clone().next().map_or(false, |x| x != '\n') {
                //     self.bump();
                // }
                // // this can ge modified to return the content of the comment
                // self.next()
            },
            _ => None
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.ignore_whitespace();
        if let Some(ch) = self.it.clone().next() {
            if ch.is_alphabetic() || ch == '_' {
                self.parse_possible_ident()
            }
            else if ch.is_numeric() {
               self.parse_number()
            }
            else {
                self.parse_operator(ch)
            }
        }
        else {
            if !self.at_eof {
                let file = FilePos::new(self.line, self.column, self.file.clone());
                let span = Span::new(self.index, self.index);
                let pos = Position::new(file, span);
                self.at_eof = true;
                Some(Token::new(TokenKind::Eof, pos))
            }
            else {
                None
            }
        }
    }
}
