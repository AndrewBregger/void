use super::token::{TokenKind, FilePos, Position, Span, Op};
use std::rc::Rc;

pub type Ptr<T> = Box<T>;

pub trait TreeRender {
    fn render(&self, indent: u32);
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

pub enum ExprKind {
    Name(String),

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
            ExprKind::Name(ref value) => {
                println!("{}Ident: {}", indent(idx), value);
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

pub enum StmtKind {
    ExprStmt(Ptr<Expr>),
    SemiStmt(Ptr<Expr>),
}

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
