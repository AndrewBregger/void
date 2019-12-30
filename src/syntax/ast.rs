use super::token::{TokenKind, FilePos, Position, Span, Op};
use std::rc::Rc;

pub type Ptr<T> = Box<T>;

pub trait TreeRender {
    fn render(&self, idx: u32);
}

pub trait AstNode {
    fn pos(&self) -> &Position;
}

fn indent(idt: u32) -> String {
    let mut value = String::new();

    for _ in 0..idt {
        value.push('\t');
    }

    value
}

#[derive(Debug, Clone)]
pub struct Ident {
    val: String,
    pos: Position,
}

impl Ident {
    pub fn new(val: &str, pos: Position) -> Self {
        Self {
            val: val.to_string(),
            pos
        }
    }

    pub fn value(&self) -> &str {
        self.val.as_str()
    }

    pub fn pos(&self) -> &Position {
        &self.pos
    }
}

impl TreeRender for Ident {
    fn render(&self, idx: u32) {
        println!("{}Ident: {}", indent(idx), self.val.as_str());
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Name(Ident),
    NameTyped(Ident, Vec<Ptr<Expr>>),
    IntegerLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),

    Unary(Op, Ptr<Expr>),
    Binary(Op, Ptr<Expr>, Ptr<Expr>),

    FunctionCall(Ptr<Expr>, Vec<Ptr<Expr>>),

    Field(Ptr<Expr>, Ident),

    Block(Vec<Ptr<Stmt>>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    kind: ExprKind,
    position: Position
}

impl Expr {
    pub fn new(kind: ExprKind, position: Position) -> Self {
        Self {
            kind,
            position
        }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

}

impl AstNode for Expr {
    fn pos(&self) -> &Position {
        &self.position
    }
}

impl TreeRender for Expr {
    fn render(&self, idx: u32) {
        match &self.kind {
            ExprKind::Name(name) => {
                println!("{}Name: {}", indent(idx), name.value());
            },
            ExprKind::NameTyped(name, types) => {
                println!("{}Typed Name: {}", indent(idx), name.value());
                for ty in types {
                    ty.render(idx + 1);
                }
            },
            ExprKind::IntegerLiteral(value) => {
                println!("{}Integer: {}", indent(idx), value);
            },
            ExprKind::FloatLiteral(value) => {
                println!("{}Float: {}", indent(idx), value);
            },
            ExprKind::StringLiteral(ref value) => {
                println!("{}String: {}", indent(idx), value);
            },
            ExprKind::CharLiteral(ch) => {
                println!("{}Char: {}", indent(idx), ch);
            },
            ExprKind::Unary(ref op, ref expr) => {
                println!("{}Unary: {}", indent(idx), op.to_string());
                expr.render(idx + 1);
            },
            ExprKind::Binary(ref op, ref lhs, ref rhs) => {
                println!("{}Binary: {}", indent(idx), op.to_string());
                lhs.render(idx + 1);
                rhs.render(idx + 1);
            },
            ExprKind::FunctionCall(ref operand, ref actuals) => {
                println!("{}Fn Call:", indent(idx));
                operand.render(idx + 1);

                println!("{}Actuals:", indent(idx + 1));
                for actual in actuals {
                    actual.render(idx + 1);
                }
            },
            ExprKind::Field(operand, field) => {
                println!("{}Field:", indent(idx));
                operand.render(idx + 1);
                field.render(idx + 1);
            },
            ExprKind::Block(stmts) => {
                println!("{}Block:", indent(idx));
                for stmt in stmts {
                    stmt.render(idx + 1);
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    ItemStmt(Ptr<Item>),
    ExprStmt(Ptr<Expr>),
    SemiStmt(Ptr<Expr>),
}

#[derive(Debug, Clone)]
pub struct Stmt {
    kind: StmtKind,
    position: Position
}

impl Stmt {
    pub fn new(kind: StmtKind, position: Position) -> Self {
        Self {
            kind,
            position
        }
    }

    pub fn kind(&self) -> &StmtKind {
        &self.kind
    }
}

impl AstNode for Stmt {
    fn pos(&self) -> &Position {
        &self.position
    }
}

impl TreeRender for Stmt {
    fn render(&self, idx: u32) {
        match &self.kind {
            StmtKind::ItemStmt(item) => {
                println!("{}Item Stmt:", indent(idx));
                item.render(idx + 1);
            }
            StmtKind::ExprStmt(expr) => {
                println!("{}Expr Stmt:", indent(idx));
                expr.render(idx + 1);
            }
            StmtKind::SemiStmt(expr) => {
                println!("{}Semi Stmt:", indent(idx));
                expr.render(idx + 1);
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Identifier(Ident),
    Tuple(Vec<Ptr<Pattern>>),
    Variant(Ident, Vec<Ptr<Pattern>>),
//    Struct(Ident, Vec<>)
    Ignore
}

#[derive(Debug, Clone)]
pub struct Pattern {
    kind: PatternKind,
    position: Position,
}

impl Pattern {
    pub fn new(kind: PatternKind, position: Position) -> Self {
        Self {
            kind,
            position
        }
    }

    pub fn pos(&self) -> &Position {
        &self.position
    }
}

impl AstNode for Pattern {
    fn pos(&self) -> &Position {
        &self.position
    }
}

impl TreeRender for Pattern {
    fn render(&self, idx: u32) {
        match &self.kind {
            PatternKind::Identifier(ident) => {
                println!("{}Ident Pattern: {}", indent(idx), ident.value());
            }
            PatternKind::Tuple(patterns) => {
                println!("{}Tuple Pattern:", indent(idx));
                for pattern in patterns {
                    pattern.render(idx + 1);
                }
            }
            PatternKind::Variant(name, patterns) => {
                println!("{}Variant Pattern: {}", indent(idx), name.value());
                for pattern in patterns {
                    pattern.render(idx + 1);
                }
            }
            PatternKind::Ignore => {
                println!("{}Ignore Pattern:", indent(idx));
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeParamsKind {
    // When the type parameter doesn't have any
    Named(Ident),
    BoundedNamed(Ident, Vec<Ptr<Expr>>)
}

#[derive(Debug, Clone)]
pub struct TypeParams {
    kind: TypeParamsKind,
    position: Position
}

impl TypeParams {
    pub fn new(kind: TypeParamsKind, position: Position) -> Self {
        Self {
            kind,
            position
        }
    }

    pub fn kind(&self) -> &TypeParamsKind {
        &self.kind
    }
}

impl AstNode for TypeParams {
    fn pos(&self) -> &Position {
        &self.position
    }
}

impl TreeRender for TypeParams {
    fn render(&self, idx: u32) {
        match self.kind() {
            TypeParamsKind::Named(name) => {
                println!("{}Named: {}", indent(idx), name.value());
            },
            TypeParamsKind::BoundedNamed(name, bounds) => {
                println!("{}Bounded Named: {}", indent(idx), name.value());
                for bound in bounds {
                    bound.render(idx + 1);
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParamKind {
    ParamInit(Ident, Ptr<Expr>),
    ParamTyped(Ident, Ptr<Expr>),
    Param(Ident, Ptr<Expr>, Ptr<Expr>),
}

#[derive(Debug, Clone)]
pub struct Param {
    kind: ParamKind,
    position: Position,
}

impl Param {
    pub fn new(kind: ParamKind, position: Position) -> Self {
        Self {
            kind,
            position,
        }
    }

    pub fn kind(&self) -> &ParamKind {
        &self.kind
    }
}

impl AstNode for Param {
    fn pos(&self) -> &Position {
        &self.position
    }
}

impl TreeRender for Param {
    fn render(&self, idx: u32) {
        match self.kind() {
            ParamKind::ParamInit(name, init) => {
                println!("{}Init Param: {}", indent(idx), name.value());
                init.render(idx + 1);
            },
            ParamKind::ParamTyped(name, types) => {
                println!("{}Typed Param: {}", indent(idx), name.value());
                types.render(idx + 1);
            },
            ParamKind::Param(name, types, init) => {
                println!("{}Param: {}", indent(idx), name.value());
                types.render(idx + 1);
                init.render(idx + 1);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Function(Ident, TypeParams, Vec<Ptr<Param>>, Ptr<Expr>),
    Struct(Ident, TypeParams, Vec<Ptr<Item>>),
    Trait(),
    LocalInit(Ptr<Pattern>, Ptr<Expr>),
    LocalTyped(Ptr<Pattern>, Ptr<Expr>),
    Local(Ptr<Pattern>, Ptr<Expr>, Ptr<Expr>),
}

#[derive(Debug, Clone)]
pub enum Visibility {
    Public,
    Private,
    Module,
}

#[derive(Debug, Clone)]
pub struct Item {
    vis: Visibility,
    kind: ItemKind,
    position: Position
}

impl Item {
    pub fn new(vis: Visibility, kind: ItemKind, position: Position) -> Self {
        Self {
            vis,
            kind,
            position
        }
    }

    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }
}

impl AstNode for Item {
    fn pos(&self) -> &Position {
        &self.position
    }
}

impl TreeRender for Item {
    fn render(&self, idx: u32) {
        match &self.kind {
            ItemKind::Function(name, tparams, params, expr) => {
                println!("{}Function: {}", indent(idx), name.value());
                tparams.render(idx + 1);
                println!("{}Params:", indent(idx + 1));
                for param in params {
                    param.render(idx + 1);
                }
                println!("{}Body:", indent(idx + 1));
                expr.render(idx + 1);
            }
            ItemKind::Struct(name, tparams, fields) => {
                println!("{}Structure: {}", indent(idx), name.value());
                tparams.render(idx + 1);
                println!("{}Fields:", indent(idx + 1));
                for field in fields {
                    field.render(idx + 1);
                }
            }
            ItemKind::LocalInit(pattern, init) => {
                println!("{}Init Local:", indent(idx));
                init.render(idx + 1);
            }
            ItemKind::LocalTyped(pattern, types) => {
                println!("{}Typed Local:", indent(idx));
                types.render(idx + 1);
            }
            ItemKind::Local(pattern, types, init) => {
                println!("{}Local:", indent(idx));
                types.render(idx + 1);
                init.render(idx + 1);
            }
            _ => {}
        }
    }
}
