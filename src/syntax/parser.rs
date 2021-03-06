use super::ast::*;
use super::token::{Ctrl, FilePos, Kw, Op, Orientation, Position, Span, Token, TokenKind};
use super::tokenstream::TokenStream;
use crate::diagnostics::Diagnostics;
use crate::syntax::ast::Mutability::Mutable;
use crate::syntax::token::Kw::Or;
use std::cmp::{Eq, PartialEq};
use std::collections::HashSet;
use std::iter::Iterator;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub enum Error {
    ParseError,
    OtherError(String),
}

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Restriction {
    TypeExprOnly,
    NoVisibility,
}

#[derive(Debug, Clone)]
pub struct Restrictions {
    res: HashSet<Restriction>,
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
    diagnostics: &'a mut Diagnostics,
    peek: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, path: PathBuf, diagnostics: &'a mut Diagnostics) -> Result<Self> {
        let mut stream = TokenStream::<'a>::new(source, path);
        let token = stream.next();
        let peek = stream.next();

        if token.is_none() {
            Err(Error::OtherError("No valid tokens in file".to_string()))
        } else {
            Ok(Self {
                stream,
                index: 0usize,
                restrictions: Restrictions::default(),
                token: token.unwrap(),
                diagnostics,
                peek,
            })
        }
    }

    pub fn path(&self) -> &PathBuf {
        self.stream.path()
    }

    fn advance(&mut self) {
        if self.peek.is_some() {
            self.token = self.peek.clone().unwrap();
            self.peek = self.stream.next();
            self.index += 1;
        }
    }

    fn check_current<P>(&self, p: P) -> bool
    where
        P: Fn(&Token) -> bool,
    {
        p(&self.token)
    }

    fn check(&self, kind: &TokenKind) -> bool {
        self.check_current(|token| token.check(kind.clone()))
    }

    fn check_keyword(&self, kw: Kw) -> bool {
        self.check(&TokenKind::Keyword(kw))
    }

    fn check_control(&self, ctrl: Ctrl) -> bool {
        self.check(&TokenKind::Control(ctrl))
    }

    fn check_comma(&self) -> bool {
        self.check_control(Ctrl::Comma)
    }

    fn check_one_of(&self, kinds: Vec<TokenKind>) -> bool {
        for kind in &kinds {
            if self.check(kind) {
                return true;
            }
        }
        false
    }

    fn check_peek(&self, kind: TokenKind) -> bool {
        self.peek.as_ref().map_or(false, |token| token.check(kind))
    }

    fn allow(&mut self, kind: TokenKind) -> bool {
        if self.check(&kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        if self.check(&kind) {
            let token = self.token.clone();
            self.advance();
            Ok(token)
        } else {
            self.diagnostics.syntax_error(
                format!(
                    "expecting '{}', found '{}'",
                    kind.to_string(),
                    self.token.to_string()
                )
                .as_str(),
                self.token.pos(),
            );
            Err(Error::ParseError)
        }
    }

    pub fn parse_file(&mut self) -> Result<ParsedFile> {
        let mut file = ParsedFile::new(self.path());

        while !self.check(&TokenKind::Eof) {
            let item = self.parse_item()?;

            if self.item_needs_semicolon(&item) {
                self.expect(TokenKind::Control(Ctrl::Semicolon))?;
            }

            if item.is_import() {
                file.add_import(item);
            } else {
                file.add_item(item);
            }
        }

        self.validate_top_level_items(&file)?;

        Ok(file)
    }

    fn validate_top_level_items(&mut self, file: &ParsedFile) -> Result<()> {
        let mut has_error = false;
        for item in file.items() {
            if item.is_variable() {
                self.diagnostics.syntax_error("invalid top level item declaration", item.pos());
                has_error = true;
            }
        }

        if has_error {
            Err(Error::ParseError)
        }
        else {
            Ok(())
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
                }
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
                let expr = self
                    .parse_expr_with_res(Restrictions::default().with(Restriction::TypeExprOnly))?;
                pos.span.extend_node(expr.as_ref());
                types.push(expr);

                if self.check(&TokenKind::Control(Ctrl::Comma)) {
                    pos.span.extend_to(&self.token);
                    self.advance();
                    expect_expression = true;
                } else {
                    expect_expression = false;
                    break;
                }
            }

            let found = self.expect(TokenKind::Control(Ctrl::Brace(Orientation::Right)))?;
            pos.span.extend_to(&found);

            if expect_expression {
                self.diagnostics
                    .syntax_error("expecting an expression following ','", self.token.pos());
                return Err(Error::ParseError);
            } else {
                ExprKind::NameTyped(ident, types)
            }
        } else {
            ExprKind::Name(ident)
        };

        Ok(Ptr::new(Expr::new(kind, pos)))
    }

    fn parse_bottom_expr(&mut self) -> Result<Ptr<Expr>> {
        let pos = self.token.pos().clone();

        let expr = if self.restrictions.check(Restriction::TypeExprOnly) {
            // if only type expressions are valid then only named
            // expressions are valid.
            self.parse_name()?
        }
        else {
            match self.token.kind().clone() {
                TokenKind::Identifier(_) => self.parse_name()?,
                TokenKind::IntegerLiteral(val) => {
                    self.advance();
                    Ptr::new(Expr::new(ExprKind::IntegerLiteral(val), pos))
                }
                TokenKind::FloatLiteral(val) => {
                    self.advance();
                    Ptr::new(Expr::new(ExprKind::FloatLiteral(val), pos))
                }
                TokenKind::StringLiteral(val) => {
                    self.advance();
                    Ptr::new(Expr::new(ExprKind::StringLiteral(val.clone()), pos))
                }
                TokenKind::CharLiteral(val) => {
                    self.advance();
                    Ptr::new(Expr::new(ExprKind::CharLiteral(val), pos))
                }
                TokenKind::Control(Ctrl::Paren(Orientation::Left)) => {
                    self.advance();
                    let expr = self.parse_expr()?;
                    self.expect(TokenKind::Control(Ctrl::Paren(Orientation::Right)))?;
                    expr
                }
                TokenKind::Control(Ctrl::Bracket(Orientation::Left)) => self.parse_block_expr()?,
                TokenKind::Keyword(Kw::If)
                | TokenKind::Keyword(Kw::While)
                | TokenKind::Keyword(Kw::For)
                | TokenKind::Keyword(Kw::Loop) => self.parse_branch_expr()?,
                _ => {
                    self.diagnostics.syntax_error(
                        format!("expecting expression, found {}", self.token.to_string()).as_str(),
                        self.token.pos(),
                    );
                    return Err(Error::ParseError);
                }
            }
        };
        Ok(expr)
    }

    fn parse_branch_expr(&mut self) -> Result<Ptr<Expr>> {
        let tok = self.token.clone();
        let mut pos = tok.pos().clone();
        self.advance();

        let kind = match tok.kind() {
            TokenKind::Keyword(Kw::If) => {
                // remove struct literal
                let cond = self.parse_expr()?;

                if !self.check(&TokenKind::Control(Ctrl::Bracket(Orientation::Left))) {
                    self.diagnostics.syntax_error(
                        format!("expecting '{{' found {}", self.token.to_string()).as_str(),
                        self.token.pos(),
                    );
                    return Err(Error::ParseError);
                }

                let body = self.parse_expr()?;
                pos.span.extend_node(body.as_ref());
                ExprKind::If(cond, body)
            }
            TokenKind::Keyword(Kw::While) => {
                let cond = self.parse_expr()?;

                if !self.check(&TokenKind::Control(Ctrl::Bracket(Orientation::Left))) {
                    self.diagnostics.syntax_error(
                        format!("expecting '{{' found {}", self.token.to_string()).as_str(),
                        self.token.pos(),
                    );
                    return Err(Error::ParseError);
                }

                let body = self.parse_expr()?;
                pos.span.extend_node(body.as_ref());
                ExprKind::While(cond, body)
            }
            TokenKind::Keyword(Kw::For) => {
                let pattern = self.parse_pattern()?;
                self.expect(TokenKind::Keyword(Kw::In))?;
                let expr = self.parse_expr()?;

                if !self.check(&TokenKind::Control(Ctrl::Bracket(Orientation::Left))) {
                    self.diagnostics.syntax_error(
                        format!("expecting '{{' found {}", self.token.to_string()).as_str(),
                        self.token.pos(),
                    );
                    return Err(Error::ParseError);
                }

                let body = self.parse_expr()?;
                pos.span.extend_node(body.as_ref());
                ExprKind::For(pattern, expr, body)
            }
            TokenKind::Keyword(Kw::Loop) => {
                if !self.check(&TokenKind::Control(Ctrl::Bracket(Orientation::Left))) {
                    self.diagnostics.syntax_error(
                        format!("expecting '{{' found {}", self.token.to_string()).as_str(),
                        self.token.pos(),
                    );
                    return Err(Error::ParseError);
                }

                let body = self.parse_expr()?;
                pos.span.extend_node(body.as_ref());
                ExprKind::Loop(body)
            }
            _ => unreachable!(),
        };

        Ok(Ptr::new(Expr::new(kind, pos)))
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

            if self.check(&TokenKind::Control(Ctrl::Comma)) {
                pos.span.extend_to(&self.token);
                self.advance();
            } else {
                break;
            }
        }

        self.expect(TokenKind::Control(Ctrl::Paren(Orientation::Right)))?;
        pos.span.extend_to(&self.token);

        let kind = ExprKind::FunctionCall(operand, actuals);
        Ok(Ptr::new(Expr::new(kind, pos)))
    }

    fn parse_ident(&mut self) -> Result<Ident> {
        match self.token.kind().clone() {
            TokenKind::Identifier(val) => {
                let pos = self.token.pos().clone();
                self.advance();
                Ok(Ident::new(val.as_str(), pos))
            }
            _ => {
                self.diagnostics.syntax_error(
                    format!("expecting an identifier, found {}", self.token.to_string()).as_str(),
                    self.token.pos(),
                );
                Err(Error::ParseError)
            }
        }
    }

    fn parse_period_suffix(&mut self, operand: Ptr<Expr>) -> Result<Ptr<Expr>> {
        let period = self.expect(TokenKind::Control(Ctrl::Period))?;
        let mut pos = operand.pos().clone();
        pos.span.extend_to(&period);

        match self.token.kind().clone() {
            TokenKind::Identifier(_) => {
                let kind = if self.check_peek(TokenKind::Control(Ctrl::Paren(Orientation::Left)))
                    || self.check_peek(TokenKind::Control(Ctrl::Brace(Orientation::Left)))
                {
                    let name = self.parse_name()?;
                    if self.check(&TokenKind::Control(Ctrl::Paren(Orientation::Left))) {
                        if self.restrictions.check(Restriction::TypeExprOnly) {
                            self.diagnostics.syntax_error(format!("invalid type expression: unexpected '{}'", self.token.to_string()).as_str(), self.token.pos());
                            return Err(Error::ParseError)
                        }
                        let mut actuals = vec![operand];

                        pos.span.extend_to(&self.token);
                        self.advance();

                        while !self.check(&TokenKind::Control(Ctrl::Paren(Orientation::Right))) {
                            let expr = self.parse_expr()?;
                            pos.span.extend_node(expr.as_ref());
                            actuals.push(expr);

                            if self.check(&TokenKind::Control(Ctrl::Comma)) {
                                pos.span.extend_to(&self.token);
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        let end =
                            self.expect(TokenKind::Control(Ctrl::Paren(Orientation::Right)))?;
                        pos.span.extend_to(&end);

                        ExprKind::MethodCall(name, actuals)
                    } else {
                        unreachable!();
                        // self.diagnostics.syntax_error(format!("expecting an identifier, found {}", self.token.to_string()).as_str(), self.token.pos());
                        // return Err(Error::ParseError);
                    }
                } else {
                    let ident_save = self.token.clone();
                    let ident = self.parse_ident()?;
                    pos.span.extend_to(&ident_save);

                    ExprKind::Field(operand, ident)
                };

                Ok(Ptr::new(Expr::new(kind, pos)))
            }
            _ => {
                self.diagnostics.syntax_error(
                    format!("expecting an identifier, found {}", self.token.to_string()).as_str(),
                    self.token.pos(),
                );
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
                TokenKind::Control(Ctrl::Paren(Orientation::Left)) => {
                    if self.restrictions.check(Restriction::TypeExprOnly) {
                        self.diagnostics.syntax_error("unexpected '(' in type expression", self.token.pos());
                        return Err(Error::ParseError);
                    }
                    else {
                        operand = self.parse_function_call(operand)?
                    }
                }
                TokenKind::Control(Ctrl::Period) => operand = self.parse_period_suffix(operand)?,
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
                Op::Minus | Op::Tilde | Op::Ampersand | Op::Astrick => {
                    if self.restrictions.check(Restriction::TypeExprOnly) {
                        self.diagnostics.syntax_error(format!("invalid unary operator in type expression: '{}'", op.to_string()).as_str(), self.token.pos());
                        return Err(Error::ParseError);
                    }
                    else {
                        let expr = self.parse_unary_expr()?;
                        pos.extend(expr.pos());
                        let kind = ExprKind::Unary(op.clone(), expr);
                        return Ok(Ptr::new(Expr::new(kind, pos)));
                    }
                }
                _ => {}
            },
            _ => {}
        }

        self.parse_primary_expr()
    }

    fn parse_assoc_expr(&mut self, prec_min: usize) -> Result<Ptr<Expr>> {
        let mut expr = self.parse_unary_expr()?;

        //    println!("Parse Assoc: {}", self.token.to_string());
        if self.restrictions.check(Restriction::TypeExprOnly) {
            return Ok(expr);
        }

        while self.check_current(|token| token.prec() >= prec_min) {
            // it known this is safe because Self::check_current would have returned
            // false if it was none.
            let token = self.token.clone();
            let token_prec = token.prec();

            if token_prec < prec_min {
                break;
            }

            //    println!("Operator: {}", token.to_string());

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

            let bin_op = Ptr::new(Expr::new(kind, pos));

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
            TokenKind::Keyword(Kw::Pub),
        ]) {
            let item = self.parse_item()?;
            let pos = item.as_ref().pos().clone();

            if self.item_needs_semicolon(&item) {
                self.expect(TokenKind::Control(Ctrl::Semicolon))?;
            }

            let kind = StmtKind::ItemStmt(item);
            Ok(Ptr::new(Stmt::new(kind, pos)))
        } else {
            let expr = self.parse_expr()?;
            let pos = expr.as_ref().pos().clone();
            // let following_semicolon = self.expr_following_semicolon(&expr);
            let next_semicolon = self.check(&TokenKind::Control(Ctrl::Semicolon));
            let kind = if next_semicolon {
                self.advance();
                StmtKind::SemiStmt(expr)
            } else {
                StmtKind::ExprStmt(expr)
            };

            Ok(Ptr::new(Stmt::new(kind, pos)))
        }
    }

    fn parse_pattern(&mut self) -> Result<Ptr<Pattern>> {
        let (kind, pos) = match self.token.kind().clone() {
            TokenKind::Identifier(ref val) => {
                let ident = self.parse_ident()?;
                let pos = ident.pos().clone();
                (PatternKind::Identifier(ident), pos)
            }
            TokenKind::Control(Ctrl::Paren(Orientation::Left)) => {
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

                if expect_following {
                    self.diagnostics
                        .syntax_error("expecting an pattern following ','", self.token.pos());
                    return Err(Error::ParseError); // expecting a pattern following comma
                } else {
                    (PatternKind::Tuple(sub_patterns), pos)
                }
            }
            _ => {
                //
                self.diagnostics.syntax_error(
                    format!(
                        "expecting identifier or '(', found {}",
                        self.token.to_string()
                    )
                    .as_str(),
                    self.token.pos(),
                );
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
                self.diagnostics.syntax_error(
                    format!(
                        "expecting mutability keywork 'let' or 'mut', found {}",
                        self.token.to_string()
                    )
                    .as_str(),
                    self.token.pos(),
                );
                Err(Error::ParseError)
            }
        }
    }
    fn parse_variable_suffix(&mut self) -> Result<(VariableKind, Position)> {
        let mut pos = self.token.pos().clone();
        let ty = if self.allow(TokenKind::Control(Ctrl::Colon)) {
            Some(self.parse_typespec()?)
        } else {
            None
        };

        let init = if self.allow(TokenKind::Operator(Op::Equal)) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        let kind = match (ty, init) {
            (Some(type_spec), Some(init)) => {
                pos.span.extend_node(init.as_ref());
                VariableKind::Full(type_spec, init)
            }
            (Some(type_spec), None) => {
                pos.span.extend_node(type_spec.as_ref());
                VariableKind::Typed(type_spec)
            }
            (None, Some(init)) => {
                pos.span.extend_node(init.as_ref());
                VariableKind::Init(init)
            }
            (None, None) => {
                self.diagnostics.syntax_error(
                    "variable must have either type specification or initializing expression",
                    self.token.pos(),
                );
                return Err(Error::ParseError);
            }
        };
        Ok((kind, pos))
    }

    fn parse_local_item(&mut self, vis: Visibility) -> Result<Ptr<Item>> {
        let mut pos = self.token.pos().clone();
        let mutability = self.parse_mutability()?;
        let pattern = self.parse_pattern()?;
        pos.span.extend_node(pattern.as_ref());

        let (kind, pos) = self.parse_variable_suffix()?;

        let kind = ItemKind::LocalVar(Local {
            mutability,
            local: pattern,
            kind,
            position: pos.clone()
        });

        Ok(Ptr::new(Item::new(vis, kind, pos)))
    }

    fn parse_typeparam(&mut self) -> Result<TypeParam> {
        let name = self.parse_ident()?;
        let mut pos = name.pos().clone();

        let (kind, pos) = if self.allow(TokenKind::Control(Ctrl::Colon)) {
            let mut types = Vec::new();
            loop {
                let ty = self.parse_typespec()?;
                pos.span.extend_node(ty.as_ref());

                types.push(ty);

                if !self.allow(TokenKind::Operator(Op::Plus)) {
                    break;
                }
            }

            (TypeParamKind::BoundedNamed(name, types), pos)
        } else {
            (TypeParamKind::Named(name), pos)
        };

        Ok(TypeParam::new(kind, pos))
    }

    pub fn parse_typeparams(&mut self) -> Result<TypeParams> {
        let start = self.expect(TokenKind::Control(Ctrl::Brace(Orientation::Left)))?;
        let mut pos = start.pos().clone();
        let mut params = Vec::new();

        let mut expecting_type = false;
        while !self.check_control(Ctrl::Brace(Orientation::Right)) {
            expecting_type = false;
            let param = self.parse_typeparam().map(|p| {
                let pos = p.pos().clone(); 
                Ptr::new(Item::new(Visibility::Private, ItemKind::TypeParam(p), pos))
            })?;
            pos.span.extend_node(param.as_ref());
            params.push(param);

            if self.allow(TokenKind::Control(Ctrl::Comma)) {
                expecting_type = true;
            } else {
                break;
            }
        }

        if expecting_type {
            self.diagnostics.syntax_error(
                "expecting type specification following ','",
                self.token.pos(),
            );
            return Err(Error::ParseError);
        }

        let end = self.expect(TokenKind::Control(Ctrl::Brace(Orientation::Right)))?;
        pos.span.extend_to(&end);

        Ok(TypeParams::new(params, pos))
    }

    fn try_parse_typeparams(&mut self) -> Result<Option<TypeParams>> {
        if self.check_control(Ctrl::Brace(Orientation::Left)) {
            Ok(Some(self.parse_typeparams()?))
        } else {
            Ok(None)
        }
    }

    fn parse_field(&mut self) -> Result<Field> {
        let mut pos = self.token.pos().clone();
        let vis = self.parse_visibility();
        let name = self.parse_ident()?;
        let (kind, pos) = self.parse_variable_suffix()?;
        Ok(Field::new(vis, name, kind, pos))
    }

    fn parse_struct_item(&mut self, vis: Visibility) -> Result<Ptr<Item>> {
        let start = self.expect(TokenKind::Keyword(Kw::Struct))?;
        let mut pos = start.pos().clone();
        let name = self.parse_ident()?;

        let type_params = self.try_parse_typeparams()?;
        if type_params.is_some() {
            let params = type_params.as_ref().unwrap();
            pos.span.extend_node(params);
        }

        // parse body of the struct
        self.expect(TokenKind::Control(Ctrl::Bracket(Orientation::Left)))?;
        let mut fields = Vec::new();
        while !self.check(&TokenKind::Control(Ctrl::Bracket(Orientation::Right))) {
            let field = self.parse_field().map(|field| {
                let pos = field.pos().clone();
                let vis = field.vis();
                Ptr::new(Item::new(vis, ItemKind::StructField(field), pos))
            })?;
            pos.span.extend_node(field.as_ref());
            fields.push(field);

            if !self.allow(TokenKind::Control(Ctrl::Comma)) {
                break;
            }
        }

        let end = self.expect(TokenKind::Control(Ctrl::Bracket(Orientation::Right)))?;
        pos.span.extend_to(&end);

        Ok(Ptr::new(Item::new(
            vis,
            ItemKind::Struct(Structure {name, type_params, fields}),
            pos,
        )))
    }

    fn parse_parameter(&mut self) -> Result<Param> {
        let mut pos = self.token.pos().clone();
        let mutability = if self.allow(TokenKind::Keyword(Kw::Mut)) {
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };

        let name = self.parse_ident()?;
        let (kind, pos) = self.parse_variable_suffix()?;

        Ok(Param::new(mutability, name, kind, pos))
    }

    fn parse_parameters(&mut self) -> Result<Vec<Ptr<Item>>> {
        let mut pos = self.token.pos().clone();
        let start = self.expect(TokenKind::Control(Ctrl::Paren(Orientation::Left)))?;
        let mut params = Vec::new();
        let mut expect_param = false;
        while !self.check(&TokenKind::Control(Ctrl::Paren(Orientation::Right))) {
            expect_param = false;
            let param = self.parse_parameter().map(|param|{
                let pos = param.pos().clone();
                Ptr::new(Item::new(
                    Visibility::Private,
                    ItemKind::FunctionParam(param),
                    pos
                ))
            })?;
            pos.span.extend_node(param.as_ref());
            params.push(param);

            if !self.allow(TokenKind::Control(Ctrl::Comma)) {
                break;
            } else {
                expect_param = true;
            }
        }

        if expect_param {
            self.diagnostics
                .syntax_error("expecting parameter following ','", self.token.pos());
            Err(Error::ParseError)
        } else {
            let end = self.expect(TokenKind::Control(Ctrl::Paren(Orientation::Right)))?;
            pos.span.extend_to(&end);
            Ok(params)
        }
    }

    fn parse_function_item(&mut self, vis: Visibility) -> Result<Ptr<Item>> {
        let start = self.expect(TokenKind::Keyword(Kw::Fn))?;
        let mut pos = start.pos().clone();
        let name = self.parse_ident()?;
        let type_params = self.try_parse_typeparams()?;

        let params = self.parse_parameters()?;
        let ret = if !self.check_one_of(vec![
            TokenKind::Control(Ctrl::Bracket(Orientation::Left)),
            TokenKind::Operator(Op::Equal),
        ]) {
            self.parse_typespec()?
        } else {
            Ptr::new(TypeSpec::new_unit_type(self.token.pos()))
        };

        let body = if self.allow(TokenKind::Operator(Op::Equal)) {
            self.parse_expr()?
        } else {
            if self.check(&TokenKind::Control(Ctrl::Bracket(Orientation::Left))) {
                self.parse_expr()?
            } else {
                self.diagnostics.syntax_error(
                    "expecting '=' or '{' to define function body",
                    self.token.pos(),
                );
                return Err(Error::ParseError);
            }
        };
        pos.span.extend_node(body.as_ref());

        let kind = ItemKind::Funct(Function { name, type_params, params, ret, body });
        Ok(Ptr::new(Item::new(vis, kind, pos)))
    }

    fn parse_visibility(&mut self) -> Visibility {
        match self.token.kind().clone() {
            TokenKind::Keyword(Kw::Pub) => {
                self.advance();
                Visibility::Public
            }
            _ => Visibility::Private,
        }
    }

    pub fn parse_item(&mut self) -> Result<Ptr<Item>> {
        let vis = self.parse_visibility();

        match self.token.kind().clone() {
            TokenKind::Keyword(Kw::Let) | TokenKind::Keyword(Kw::Mut) => self.parse_local_item(vis),
            TokenKind::Keyword(Kw::Struct) => self.parse_struct_item(vis),
            TokenKind::Keyword(Kw::Fn) => self.parse_function_item(vis),
            _ => {
                println!("Unimplemented: {}", self.token.to_string());
                unimplemented!()
            }
        }
    }

    fn parse_typespec(&mut self) -> Result<Ptr<TypeSpec>> {
        let (kind, pos) = match self.token.kind().clone() {
            TokenKind::Identifier(_) => {
                let expr = self
                    .parse_expr_with_res(Restrictions::default().with(Restriction::TypeExprOnly))?;
                let pos = expr.pos().clone();
                (TypeSpecKind::ExprType(expr), pos)
            }
            TokenKind::Control(Ctrl::Paren(Orientation::Left)) => {
                let mut pos = self.token.pos().clone();
                self.advance();
                let mut subtypes = Vec::new();
                let mut expect_following = false;
                while !self.check_control(Ctrl::Paren(Orientation::Right)) {
                    expect_following = false;
                    let stype = self.parse_typespec()?;
                    pos.span.extend_node(stype.as_ref());
                    subtypes.push(stype);

                    if self.check_comma() {
                        pos.span.extend_to(&self.token);
                        self.advance();
                        expect_following = true;
                    } else {
                        break;
                    }
                }

                if expect_following {
                    self.diagnostics
                        .syntax_error("expecting type following ','", self.token.pos());
                    return Err(Error::ParseError);
                } else {
                    let token = self.expect(TokenKind::Control(Ctrl::Paren(Orientation::Right)))?;
                    pos.span.extend_to(&token);
                    (TypeSpecKind::TupleType(subtypes), pos)
                }
            }
            TokenKind::Keyword(Kw::Mut) => {
                let mut pos = self.token.pos().clone();
                self.advance();
                let ty = self.parse_typespec()?;
                pos.span.extend_node(ty.as_ref());
                (TypeSpecKind::MutType(ty), pos)
            }
            TokenKind::Operator(Op::Astrick) => {
                let mut pos = self.token.pos().clone();
                self.advance();
                let ty = self.parse_typespec()?;
                pos.span.extend_node(ty.as_ref());
                (TypeSpecKind::PtrType(ty), pos)
            }
            _ => {
                self.diagnostics.syntax_error(
                    format!(
                        "expecting identifier, 'mut', '*', found {}",
                        self.token.to_string()
                    )
                    .as_str(),
                    self.token.pos(),
                );
                return Err(Error::ParseError);
            }
        };

        Ok(Ptr::new(TypeSpec::new(kind, pos)))
    }

    fn expr_following_semicolon(&self, expr: &Ptr<Expr>) -> bool {
        match expr.as_ref().kind() {
            _ => true,
        }
    }

    fn item_needs_semicolon(&self, item: &Ptr<Item>) -> bool {
        match item.as_ref().kind() {
            ItemKind::LocalVar(..) => true,
            _ => false,
        }
    }
}

// #[cfg(test)]
// mod test {
//     use super::*;

//     #[test]
//     fn test_expr() {
//         let test_input = "x.y + 1.0.to_string";
//         let mut parser = Parser::new(test_input, PathBuf::new());

//         let valid = match parser.parse_expr() {
//             Ok(expr) => true,
//             Err(err) => false,
//         };
//         assert!(valid, "");
//     }

//     #[test]
//     fn test_local_item() {
//         let test_input = "let x = 10.0";
//         let mut parser = Parser::new(test_input, PathBuf::new());

//         let valid = match parser.parse_item() {
//             Ok(item) => {
//                 item.render(0);
//                 true
//             }
//             Err(err) => {
//                 println!("{}", err);
//                 false
//             }
//         };

//         assert!(valid, "");
//     }
// }
