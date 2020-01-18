extern crate snafu;
use snafu::{ensure, Backtrace, ErrorCompat, ResultExt, Snafu};
use super::token::{Token, FilePos, Position, Span, TokenKind, Ctrl, Kw, Op, Orientation};
use super::ast::*;
use super::tokenstream::TokenStream;
use std::iter::Iterator;
use std::path::PathBuf;
use std::cmp::{Eq, PartialEq};
use std::collections::HashSet;
use crate::syntax::token::Ctrl::Paren;

#[derive(Debug, Clone)]
pub enum Error {
    ParseError,
    OtherError(String),
}

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Restriction {
    TypeExprOnly,
    NoVisibility,
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
    token: Token,
    peek: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, path: PathBuf) -> Result<Self> {
        let mut stream = TokenStream::<'a>::new(source, path);
        let token = stream.next();
        let peek = stream.next();

        if token.is_none() {
            Err(Error::OtherError("No valid tokens in file".to_string()))
        }
        else {
            Ok(Self {
                stream,
                index: 0usize,
                restrictions: Restrictions::default(),
                token: token.unwrap(),
                peek
            })
        }
    }

    fn advance(&mut self) {
        if self.peek.is_some() {
            self.token = self.peek.clone().unwrap();
            self.peek = self.stream.next();
            self.index += 1;
        }
    }

    fn check_current<P>(&self, p: P) -> bool
    where P: Fn(&Token) -> bool {
        p(&self.token)
    }

    fn check(&self, kind: &TokenKind) -> bool {
        self.check_current(|token| token.check(kind.clone()))
    }

    fn check_one_of(&self, kinds: Vec<TokenKind>) -> bool {
        for kind in &kinds {
            if self.check(kind) {
                return true;
            }
        }
        return false;
    }

    fn check_peek(&self, kind: TokenKind) -> bool {
        self.peek.as_ref().map_or(false, |token| token.check(kind))
    }


    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        if self.check(&kind) {
            let token = self.token.clone();
            self.advance();
            Ok(token)
        }
        else {
            Err(Error::ParseError)
        }
    }

    fn parse_block_expr(&mut self) -> Result<Ptr<Expr>> {
       let mut pos = self.token.pos().clone();
       let open = self.expect(TokenKind::Control(Ctrl::Bracket(Orientation::Left)))?;
       pos.span.extend_to(&open);

       let mut stmts = Vec::new();

       while !self.check(&TokenKind::Control(Ctrl::Bracket(Orientation::Right))) {
           match self.parse_stmt() {
               Ok(stmt) => {
                   pos.span.extend_node(stmt.as_ref());
                   stmts.push(stmt);
               },
               Err(err) => {
                   println!("{:?}", err);
                   // self.sync();
               }
           }
       }

       pos.span.extend_to(&self.token);
       self.advance();

       let kind = ExprKind::Block(stmts);
       Ok(Ptr::new(Expr::new(kind, pos)))
    }

    fn parse_name(&mut self) -> Result<Ptr<Expr>> {
       let ident = self.parse_ident()?;
       let mut pos = ident.pos().clone();

       let kind = if self.check(&TokenKind::Control(Ctrl::Brace(Orientation::Left))) {
           pos.span.extend_to(&self.token);
           self.advance();
           let mut types = Vec::new();
           let mut expect_expression = false;
           while !self.check(&TokenKind::Control(Ctrl::Brace(Orientation::Left))) {
               let expr = self.parse_expr_with_res(Restrictions::default().with(Restriction::TypeExprOnly))?;
               pos.span.extend_node(expr.as_ref());
               types.push(expr);

               if self.check(&TokenKind::Control(Ctrl::Comma)) {
                   pos.span.extend_to(&self.token);
                   self.advance();
                   expect_expression = true;
               }
               else {
                   expect_expression = false;
                   break;
               }
           }

           let found = self.expect(TokenKind::Control(Ctrl::Brace(Orientation::Right)))?;
           pos.span.extend_to(&found);

           if expect_expression {
               return Err(Error::ParseError);
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
        println!("parse_bottom_expr {}", self.token.to_string());
        let pos = self.token.pos().clone();
        let expr =
            match self.token.kind().clone() {
                TokenKind::Identifier(_) => {
                    self.parse_name()?
                },
                TokenKind::IntegerLiteral(val) => {
                    self.advance();
                    Ptr::new(Expr::new(ExprKind::IntegerLiteral(val), pos))
                },
                TokenKind::FloatLiteral(val)   => {
                    self.advance();
                    Ptr::new(Expr::new(ExprKind::FloatLiteral(val), pos))
                },
                TokenKind::StringLiteral(val)  => {
                    self.advance();
                    Ptr::new(Expr::new(ExprKind::StringLiteral(val.clone()), pos))
                },
                TokenKind::CharLiteral(val)    => {
                    self.advance();
                    Ptr::new(Expr::new(ExprKind::CharLiteral(val), pos))
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
                _ => {
                    return Err(Error::ParseError);
                }
            };
        Ok(expr)
    }

    fn parse_function_call(&mut self, operand: Ptr<Expr>) -> Result<Ptr<Expr>> {
        let open = self.expect(TokenKind::Control(Ctrl::Paren(Orientation::Left)))?;

        let mut pos = operand.pos().clone();
        let mut actuals = Vec::new();

        pos.span.extend_to(&open);
        while !self.check(&TokenKind::Control(Ctrl::Paren(Orientation::Right))) {
            let expr = self.parse_expr()?;
            pos.span.extend_node(expr.as_ref());
            actuals.push(expr);

            if self.check(
                &TokenKind::Control(Ctrl::Comma)
            ) {
                pos.span.extend_to(&self.token);
                self.advance();
            }
            else {
                break;
            }
        }

        self.expect(TokenKind::Control(Ctrl::Paren(Orientation::Right)))?;
        pos.span.extend_to(&self.token);


        let kind = ExprKind::FunctionCall(operand, actuals);
        Ok(Ptr::new(Expr::new(kind, pos)))
    }

    fn parse_ident(&mut self) -> Result<Ident> {
        println!("ident: {}", self.token.to_string());
        match self.token.kind().clone() {
            TokenKind::Identifier(val) => {
                let pos = self.token.pos().clone();
                self.advance();
                Ok(Ident::new(val.as_str(), pos))
            }
            _ => {
               Err(Error::ParseError)
            }
        }
    }

    fn parse_period_suffix(&mut self, operand: Ptr<Expr>) -> Result<Ptr<Expr>> {
        let period = self.expect(TokenKind::Control(Ctrl::Period))?;
        let mut pos = operand.pos().clone();
        pos.span.extend_to(&period);

        println!("period_suffix {}", self.token.to_string());
        match self.token.kind().clone() {
            TokenKind::Identifier(_val) => {
                let kind = if self.check_peek(TokenKind::Control(Ctrl::Paren(Orientation::Left))) ||
                             self.check_peek(TokenKind::Control(Ctrl::Brace(Orientation::Left))) {
                    let name = self.parse_name()?;
                    if self.check(&TokenKind::Control(Ctrl::Paren(Orientation::Left))) {
                        let mut actuals = vec![operand];

                        pos.span.extend_to(&self.token);
                        self.advance();

                        while !self.check(&TokenKind::Control(Ctrl::Paren(Orientation::Right))) {
                            let expr = self.parse_expr()?;
                            pos.span.extend_node(expr.as_ref());
                            actuals.push(expr);

                            if self.check(
                                &TokenKind::Control(Ctrl::Comma)
                            ) {
                                pos.span.extend_to(&self.token);
                                self.advance();
                            }
                            else {
                                break;
                            }
                        }
                        ExprKind::MethodCall(name, actuals)
                    }
                    else {
                        return Err(Error::ParseError);
                    }
                }
                else {
                    let ident_save = self.token.clone();
                    let ident = self.parse_ident()?;
                    pos.span.extend_to(&ident_save);

                    ExprKind::Field(operand, ident)
                };

                Ok(Ptr::new(Expr::new(kind, pos)))
            }
            _ => {
                Err(Error::ParseError)
            }
        }
    }

    /// Handles the parsing of expressions that have elementes following
    /// the root of the operand.
    fn parse_suffix_expr(&mut self, mut operand: Ptr<Expr>) -> Result<Ptr<Expr>> {
       loop {
            if self.token.is_eof() {
                break;
            }

            match self.token.kind().clone() {
                TokenKind::Control(Ctrl::Paren(Orientation::Left)) =>
                    operand = self.parse_function_call(operand)?,
                TokenKind::Control(Ctrl::Period) =>
                    operand = self.parse_period_suffix(operand)?,
                _ => break,
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
        let mut pos = self.token.pos().clone();
        match self.token.kind().clone() {
            TokenKind::Operator(op) => match op {
                Op::Minus |
                Op::Tilde |
                Op::Ampersand => {
                    let expr = self.parse_unary_expr()?;
                    pos.extend(expr.pos());
                    let kind = ExprKind::Unary(op.clone(), expr);
                    return Ok(Ptr::new(Expr::new(kind, pos)));

                }
                _ => {}
            }
            _ => {}
        }

        self.parse_primary_expr()
    }

    fn parse_assoc_expr(&mut self, prec_min: usize) -> Result<Ptr<Expr>> {
       let mut expr = self.parse_unary_expr()?;

       println!("Parse Assoc: {}", self.token.to_string());

       while self.check_current(|token| token.prec() >= prec_min) {
           // it known this is safe because Self::check_current would have returned
           // false if it was none.
           let token = self.token.clone();
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

           let rhs = self.parse_assoc_expr(token_prec + 1)?;

           pos.span.extend_node(rhs.as_ref());

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

        if self.check_one_of(vec![
            TokenKind::Keyword(Kw::Let),
            TokenKind::Keyword(Kw::Mut),
            TokenKind::Keyword(Kw::Struct),
            TokenKind::Keyword(Kw::Trait),
            TokenKind::Keyword(Kw::Fn),
            TokenKind::Keyword(Kw::Pub)
        ]) {
            let item = self.parse_item()?;
            let pos = item.as_ref().pos().clone();

            if self.item_needs_semicolon(&item) {
                self.expect(TokenKind::Control(Ctrl::Semicolon))?;
            }

            let kind = StmtKind::ItemStmt(item);
            Ok(Ptr::new(Stmt::new(kind, pos)))
        }
        else {
            let expr = self.parse_expr()?;
            let pos = expr.as_ref().pos().clone();
            // let following_semicolon = self.expr_following_semicolon(&expr);
            let next_semicolon = self.check(&TokenKind::Control(Ctrl::Semicolon));
            let kind = if next_semicolon {
                self.advance();
                StmtKind::SemiStmt(expr)
            }else {
                StmtKind::ExprStmt(expr)
            };

            Ok(Ptr::new(Stmt::new(kind, pos)))
        }
    }

    fn parse_pattern(&mut self) -> Result<Ptr<Pattern>> {
        let (kind, pos)  = match self.token.kind().clone() {
            TokenKind::Identifier(ref val) => {
                let ident = self.parse_ident()?;       
                let pos = ident.pos().clone();
                (PatternKind::Identifier(ident), pos)
            },
            TokenKind::Control(Ctrl::Paren(Orientation::Left)) => {
                println!("Parsering tuple pattern");
                let mut pos = self.token.pos().clone();
                self.advance();

                let mut sub_patterns = Vec::new();
                
                let mut expect_following = false;
                while !self.check(&TokenKind::Control(Ctrl::Paren(Orientation::Right))) {
                    let sub_pattern = self.parse_pattern()?;

                    expect_following = false;
                    pos.span.extend_node(sub_pattern.as_ref());

                    sub_patterns.push(sub_pattern);

                    if self.check(&TokenKind::Control(Ctrl::Comma)) {
                        pos.span.extend_to(&self.token);
                        self.advance();
                        expect_following = true;
                    }
                }

                let end = self.expect(TokenKind::Control(Ctrl::Paren(Orientation::Right)))?;
                pos.span.extend_to(&end);
                
                println!("Expected following {}", expect_following);
                if expect_following {
                    return Err(Error::ParseError); // expecting a pattern following comma
                }
                else {
                    (PatternKind::Tuple(sub_patterns), pos)
                }
            }
            _ => {
                //
                println!("Parse Error: {}", self.token.to_string());
                return Err(Error::ParseError); // failed to find a valid pattern.
            }
        };

        Ok(Ptr::new(Pattern::new(kind, pos)))
    }

    fn parse_mutability(&mut self) -> Result<Mutability> {
        match self.token.kind().clone() {
            TokenKind::Keyword(Kw::Mut) => {
                self.advance();
                Ok(Mutability::Mutable)
            }
            TokenKind::Keyword(Kw::Let) => {
                self.advance();
                Ok(Mutability::Immutable)
            }
            _ => {
                Err(Error::ParseError)
            }
        }
    }

    fn parse_local_item(&mut self, vis: Visibility) -> Result<Ptr<Item>> {
        let mut pos = self.token.pos().clone();
        let mutability = self.parse_mutability()?;

        let pattern = self.parse_pattern()?;

        pos.span.extend_node(pattern.as_ref());

        let ty = if self.check(&TokenKind::Control(Ctrl::Colon)) {
            self.advance();
            Some(self.parse_typespec()?)
        }
        else {
            None
        };

        let init = if self.check(&TokenKind::Operator(Op::Equal)) {
            self.advance();
            Some(self.parse_expr()?)
        }
        else {
            None
        };

        let kind = match (ty, init) {
            (Some(ty), Some(exp)) => {
                pos.span.extend_node(ty.as_ref());
                pos.span.extend_node(exp.as_ref());
                ItemKind::Local(mutability, pattern, ty, exp)
            }
            (Some(ty), None) => {
                pos.span.extend_node(ty.as_ref());
                ItemKind::LocalTyped(mutability, pattern, ty)
            }
            (None, Some(exp)) => {
                pos.span.extend_node(exp.as_ref());
                ItemKind::LocalInit(mutability, pattern, exp)
            }
            (None, None) => {
                println!("Parse Error");
                return Err(Error::ParseError);
            }
        };

        Ok(Ptr::new(Item::new(vis, kind, pos)))
    }

    fn parse_visibility(&mut self) -> Visibility {
        match self.token.kind().clone() {
            TokenKind::Keyword(Kw::Pub) => {
                self.advance();
                Visibility::Public
            },
            _ => {
                Visibility::Private
            }
        }
    }

    pub fn parse_item(&mut self) -> Result<Ptr<Item>> {
        let vis = self.parse_visibility();

        match self.token.kind().clone() {
            TokenKind::Keyword(Kw::Let) |
            TokenKind::Keyword(Kw::Mut) => self.parse_local_item(vis),
            _ => {
                unimplemented!()
            }
        }
    }

    fn parse_typespec(&mut self) -> Result<Ptr<TypeSpec>> {
        unimplemented!();
    }

    fn expr_following_semicolon(&self, expr: &Ptr<Expr>) -> bool {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_expr() {
        let test_input = "x.y + 1.0.to_string";
        let mut parser = Parser::new(test_input, PathBuf::new());

        let valid = match parser.parse_expr() {
            Ok(expr) => true,
            Err(err) => false,
        };
        assert!(valid, "");
    }

    #[test]
    fn test_local_item() {
        let test_input = "let x = 10.0";
        let mut parser = Parser::new(test_input, PathBuf::new());

        let valid = match parser.parse_item() {
            Ok(item) => {
                item.render(0);
                true
            },
            Err(err) => {
                println!("{}", err);
                false
            }
        };

        assert!(valid, "");
    }
}
