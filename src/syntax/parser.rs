extern crate snafu;
use snafu::{ensure, Backtrace, ErrorCompat, ResultExt, Snafu};
use super::token::{Token, FilePos, Position, Span, TokenKind, Ctrl, Kw, Op, Orientation};
use super::ast::*;
use super::tokenstream::TokenStream;
use std::iter::Iterator;
use std::path::PathBuf;
use std::cmp::{Eq, PartialEq};
use std::collections::HashSet;

#[derive(Debug, Clone, Snafu)]
pub enum Error {
    #[snafu(display("{} | Expected {}, found {}", pos.to_string(), expected.to_string(), found.to_string()))]
    ExpectedToken {
        expected: TokenKind,
        found: Token,
        pos: Position
    },
    #[snafu(display("{} | Expected {} at end of file", pos.to_string(), expected.to_string()))]
    UnexpectedEof {
        expected: TokenKind,
        pos: Position
    },
    #[snafu(display("{} | Expecting expression following comma, found {}", pos.to_string(), found.to_string()))]
    ExpectedExprAfterComma {
        found: Token,
        pos: Position
    }



}

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Restriction {
    TypeExprOnly,
}

#[derive(Debug, Clone)]
struct Restrictions {
    res: HashSet<Restriction>
}

impl Restrictions {
    fn add(&mut self, res: Restriction) {
        self.res.insert(res);
    }

    fn with(mut self, res: Restriction) -> Self {
        self.res.insert(res);
        self
    }

    fn extend(&mut self, res: Restrictions) {
        for res in res.res {
            self.add(res);
        }
    }

    fn check(&self, res: Restriction) -> bool {
        self.res.contains(&res)
    }
}

impl std::default::Default for Restrictions {
    fn default() -> Self {
        Self {
            res: HashSet::new(),
        }
    }
}
    
    

pub struct Parser<'a> {
    stream: TokenStream<'a>,
    restrictions: Restrictions,
    index: usize,
    token: Option<Token>,
    peek: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, path: PathBuf) -> Self {
        let mut stream = TokenStream::<'a>::new(source, path);
        let token = stream.next();
        let peek = stream.next();
        Self {
            stream,
            index: 0usize,
            restrictions: Restrictions::default(),
            token,
            peek
        }
    }

    fn advance(&mut self) {
        self.token = self.peek.clone();
        self.peek = self.stream.next();
        self.index += 1;
    }

    fn check_current<P>(&self, p: P) -> bool
    where P: Fn(&Token) -> bool {
        match &self.token {
            Some(token) => p(token),
            _ => false,
        }
    }

    fn check(&self, kind: &TokenKind) -> bool {
        self.check_current(|token| token.check(kind.clone()))
    }


    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        if self.check(&kind) {
            let token = self.token.as_ref().expect("Attempting to unwrap token").clone();
            self.advance();
            Ok(token)
        }
        else {
            match &self.token {
                Some(current_token) => {
                    let pos = current_token.pos().clone();
                    Err(Error::ExpectedToken {
                        expected: kind,
                        found: current_token.clone(),
                        pos
                    })
                },
                _ => {
                    panic!();
                }
            }
        }
    }

    fn parse_block_expr(&mut self) -> Result<Ptr<Expr>> {
        let mut pos = self.token.as_ref().unwrap().pos().clone();
        self.expect(TokenKind::Control(Ctrl::Bracket(Orientation::Left)))?;
        pos.span.extend(1);

        let mut stmts = Vec::new();

        while !self.check(&TokenKind::Control(Ctrl::Bracket(Orientation::Right))) {
            match self.parse_stmt() {
                Ok(stmt) => {
                    pos.span.extend(stmt.as_ref().pos().len());
                    stmts.push(stmt);
                },
                Err(err) => {
                    println!("{}", err);
                    // self.sync();
                }
            }
        }

        pos.span.extend(1);

        let kind = ExprKind::Block(stmts);
        Ok(Ptr::new(Expr::new(kind, pos)))
    }

    fn parse_name(&mut self) -> Result<Ptr<Expr>> {
        let ident = self.parse_ident()?;
        let mut pos = ident.pos().clone();

        let kind = if self.check(&TokenKind::Control(Ctrl::Brace(Orientation::Left))) {
            pos.span.extend(1);
            self.advance();
            let mut types = Vec::new();
            let mut expect_expression = false;
            while !self.check(&TokenKind::Control(Ctrl::Brace(Orientation::Left))) {
                let expr = self.parse_expr_with_res(Restrictions::default().with(Restriction::TypeExprOnly))?;
                pos.span.extend(expr.pos().len());
                types.push(expr);

                if self.check(&TokenKind::Control(Ctrl::Comma)) {
                    pos.span.extend(1);
                    self.advance();
                    expect_expression = true;
                }
                else {
                    expect_expression = false;
                    break;
                }
            }

            let found = self.expect(TokenKind::Control(Ctrl::Brace(Orientation::Right)))?;
            pos.span.extend(1);

            if expect_expression {
                let pos = found.pos().clone();
                return Err(Error::ExpectedExprAfterComma {
                    found,
                    pos,
                });
            }
            else {
                ExprKind::NameTyped(ident, types)
            }
        }
        else {
            ExprKind::Name(ident)
        };

        Ok(Ptr::new(Expr::new(kind, pos)))
    }

    fn parse_bottom_expr(&mut self) -> Result<Ptr<Expr>> {
        if let Some(ref token) = self.token.clone() {
            println!("parse_bottom_expr {}", token.to_string());
            let expr =
                match token.kind() {
                    TokenKind::Identifier(_) => {
                        self.parse_name()?
                    },
                    TokenKind::IntegerLiteral(val) => {
                        self.advance();
                        Ptr::new(Expr::new(ExprKind::IntegerLiteral(*val), token.pos().clone()))
                    },
                    TokenKind::FloatLiteral(val)   => {
                        self.advance();
                        Ptr::new(Expr::new(ExprKind::FloatLiteral(*val), token.pos().clone()))
                    },
                    TokenKind::StringLiteral(val)  => {
                        self.advance();
                        Ptr::new(Expr::new(ExprKind::StringLiteral(val.clone()), token.pos().clone()))
                    },
                    TokenKind::CharLiteral(val)    => {
                        self.advance();
                        Ptr::new(Expr::new(ExprKind::CharLiteral(*val), token.pos().clone()))
                    },
                    TokenKind::Control(Ctrl::Paren(Orientation::Left)) => {
                        self.advance();
                        let expr = self.parse_expr()?;
                        self.expect(TokenKind::Control(Ctrl::Paren(Orientation::Right)))?;
                        expr
                    },
                    TokenKind::Control(Ctrl::Bracket(Orientation::Left)) => {
                        self.parse_block_expr()?
                    },
                    // TokenKind::StringLiteral(val) => Expr::new(ExprKind::IntegerLiteral(val), token.pos().clone()),

                    _ => unimplemented!()
                };
            println!("Next Token {}", self.token.as_ref().map_or("".to_string(), |t| t.to_string()));
            Ok(expr)

        }
        else {
            unreachable!()
            // unimplemented!()
        }
    }

    fn parse_function_call(&mut self, operand: Ptr<Expr>) -> Result<Ptr<Expr>> {
        let open = self.expect(TokenKind::Control(Ctrl::Paren(Orientation::Left)))?;

        let mut pos = operand.pos().clone();
        let mut actuals = Vec::new();

        pos.span.extend(open.pos().len());
        while !self.check(&TokenKind::Control(Ctrl::Paren(Orientation::Right))) {
            let expr = self.parse_expr()?;
            pos.span.extend(expr.pos().len());
            actuals.push(expr);

            if self.check(
                &TokenKind::Control(Ctrl::Comma)
            ) {
                pos.span.extend(1);
                self.advance();
            }
            else {
                break;
            }
        }

        pos.span.extend(1);
        self.expect(TokenKind::Control(Ctrl::Paren(Orientation::Right)))?;


        let kind = ExprKind::FunctionCall(operand, actuals);
        Ok(Ptr::new(Expr::new(kind, pos)))
    }

    fn parse_ident(&mut self) -> Result<Ident> {
        if let Some(token) = self.token.clone() {
            println!("ident: {}", token.to_string());
            match token.kind() {
                TokenKind::Identifier(val) => {
                    self.advance();
                    Ok(Ident::new(val.as_str(), token.pos().clone()))
                }
                _ =>
                    Err(
                        Error::ExpectedToken {
                            expected: TokenKind::Identifier(String::new()),
                            found: token.clone(),
                            pos: token.pos().clone()
                        })
            }
        }
        else {
            unimplemented!()
        }
    }

    fn parse_period_suffix(&mut self, operand: Ptr<Expr>) -> Result<Ptr<Expr>> {
        self.expect(TokenKind::Control(Ctrl::Period))?;
        let mut pos = operand.pos().clone();
        pos.span.extend(1);
       
        if let Some(token) = self.token.clone() {
            println!("period_suffix {}", token.to_string());
            match token.kind() {
                TokenKind::Identifier(val) => {
                    let ident = self.parse_ident()?;
                    pos.span.extend(val.len());
                    let kind = ExprKind::Field(operand, ident);
                    Ok(Ptr::new(Expr::new(kind, pos)))
                }
                _ => {
                    Err(
                        Error::ExpectedToken {
                            expected: TokenKind::Identifier(String::new()),
                            found: token.clone(),
                            pos: token.pos().clone()
                        })
                }
            }
        }
        else {
            unimplemented!()
        }
    }

    /// Handles the parsing of expressions that have elementes following
    /// the root of the operand.
    fn parse_suffix_expr(&mut self, mut operand: Ptr<Expr>) -> Result<Ptr<Expr>> {
        loop {
            if let Some(token) = &self.token {
                match token.kind() {
                    TokenKind::Control(Ctrl::Paren(Orientation::Left)) =>
                        operand = self.parse_function_call(operand)?,
                    TokenKind::Control(Ctrl::Period) =>
                        operand = self.parse_period_suffix(operand)?,
                    _ => break,
                }
            }
            else {
                break;
            }
        }
        Ok(operand)
    }

    /// Handles the parsing of a primary expression.
    fn parse_primary_expr(&mut self) -> Result<Ptr<Expr>> {
        let operand = self.parse_bottom_expr()?;

        self.parse_suffix_expr(operand)
    }

    fn parse_unary_expr(&mut self) -> Result<Ptr<Expr>> {
        self.parse_primary_expr()
    }

    fn parse_assoc_expr(&mut self, prec_min: usize) -> Result<Ptr<Expr>> {
        let mut expr = self.parse_unary_expr()?;

        println!("Parse Assoc: {}", self.token.as_ref().map_or(String::new(),
        |token| token.to_string()));

        while self.check_current(|token| token.prec() >= prec_min) {
            // it known this is safe because Self::check_current would have returned
            // false if it was none.
            let token = self.token.clone().unwrap();
            let token_prec = token.prec();

            if token_prec < prec_min {
                break;
            }

            println!("Operator: {}", token.to_string());

            self.advance();
           
            if !token.is_operator() && !token.is_assignment() {
                unimplemented!()
            }

            let mut pos = expr.pos().clone();

            // change this.
            pos.span.extend(1);

            let rhs = self.parse_assoc_expr(token_prec + 1)?;

            pos.span.extend(rhs.as_ref().pos().span.len());

            let op = match token.kind() {
                TokenKind::Operator(op) => op.clone(),
                _ => unreachable!(),
            };

            let kind = ExprKind::Binary(op, expr, rhs);

            let bin_op = Ptr::new(
                Expr::new(kind, pos)
            );

            expr = bin_op;
        }

        Ok(expr)
    }

    pub fn parse_expr_with_res(&mut self, res: Restrictions) -> Result<Ptr<Expr>> {
        let save_res = self.restrictions.clone();
        self.restrictions.extend(res);

        let expr = self.parse_assoc_expr(1);
        self.restrictions = save_res;

        expr
    }

    pub fn parse_expr(&mut self) -> Result<Ptr<Expr>> {
        self.parse_assoc_expr(1)
    }

    pub fn parse_stmt(&mut self) -> Result<Ptr<Stmt>> {
        if let Some(token) = self.token.clone() {
            match token.kind() {
                TokenKind::Keyword(kw) => match kw {
                    Kw::Let |
                    Kw::Mut |
                    Kw::Struct |
                    Kw::Fn |
                    Kw::Use |
                    Kw::Trait => {
                        let item = self.parse_item()?;
                        let pos = item.pos().clone();
                        let kind = StmtKind::ItemStmt(item);
                        Ok(Ptr::new(Stmt::new(kind, pos)))
                    }
                    _ => {
                        let expr = self.parse_expr()?;
                        let pos = expr.as_ref().pos().clone();
                        // if

                        let kind = if self.check(&TokenKind::Control(Ctrl::Semicolon)) &&
                                                self.expr_needs_semicolon(&expr) {
                            StmtKind::SemiStmt(expr)
                        }
                        else {
                            StmtKind::ExprStmt(expr)
                        };

                        Ok(Ptr::new(Stmt::new(kind, pos)))
                    }
                },
                _ => {
                    let expr = self.parse_expr()?;
                    let pos = expr.as_ref().pos().clone();
                    // if

                    let kind = if self.check(&TokenKind::Control(Ctrl::Semicolon)) {
                        StmtKind::SemiStmt(expr)
                    }
                    else {
                        StmtKind::ExprStmt(expr)
                    };

                    Ok(Ptr::new(Stmt::new(kind, pos)))
                }
            }
        }
        else {
            unimplemented!()
        }
    }

    fn parse_pattern(&mut self) -> Result<Ptr<Pattern>> {
        unimplemented!()
    }

    fn parse_item(&mut self) -> Result<Ptr<Item>> {
        unimplemented!()
    }

    fn expr_needs_semicolon(&self, expr: &Ptr<Expr>) -> bool {
        match expr.as_ref().kind() {
            _ => true,
        }
    }

    fn item_needs_semicolon(&self, item: &Ptr<Item>) -> bool {
        match item.as_ref().kind() {
            _ => true,
        }
    }
}
