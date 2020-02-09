use super::token::{FilePos, Op, Position, Span};
use std::string::ToString;

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
pub enum Mutability {
    Mutable,
    Immutable,
}

impl ToString for Mutability {
    fn to_string(&self) -> String {
        match self {
            Mutability::Mutable => "mutable",
            Mutability::Immutable => "immutable",
        }
        .to_string()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Visibility {
    Public,
    Private,
    Module,
}

impl ToString for Visibility {
    fn to_string(&self) -> String {
        match self {
            Visibility::Public => "public",
            Visibility::Private => "private",
            Visibility::Module => "module",
        }
        .to_string()
    }
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
            pos,
        }
    }

    #[inline]
    pub fn value(&self) -> &str {
        self.val.as_str()
    }

    #[inline]
    pub fn value_string(&self) -> &String {
        &self.val
    }

    #[inline]
    pub fn pos(&self) -> &Position {
        &self.pos
    }
}

impl TreeRender for Ident {
    fn render(&self, idx: u32) {
        println!(
            "{}Ident: {} {}",
            indent(idx),
            self.val.as_str(),
            self.pos.span
        );
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.val == other.val
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
    /// the first element of .1 is the receiver
    MethodCall(Ptr<Expr>, Vec<Ptr<Expr>>),
    Field(Ptr<Expr>, Ident),
    Block(Vec<Ptr<Stmt>>),

    If(Ptr<Expr>, Ptr<Expr>),
    While(Ptr<Expr>, Ptr<Expr>),
    For(Ptr<Pattern>, Ptr<Expr>, Ptr<Expr>),
    Loop(Ptr<Expr>),
    // ListGenerator(),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    position: Position,
}

impl Expr {
    pub fn new(kind: ExprKind, position: Position) -> Self {
        Self { kind, position }
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
                println!("{}Name: {} {}", indent(idx), name.value(), self.pos().span);
            }
            ExprKind::NameTyped(name, types) => {
                println!(
                    "{}Typed Name: {} {}",
                    indent(idx),
                    name.value(),
                    self.pos().span
                );
                for ty in types {
                    ty.render(idx + 1);
                }
            }
            ExprKind::IntegerLiteral(value) => {
                println!("{}Integer: {} {}", indent(idx), value, self.pos().span);
            }
            ExprKind::FloatLiteral(value) => {
                println!("{}Float: {} {}", indent(idx), value, self.pos().span);
            }
            ExprKind::StringLiteral(ref value) => {
                println!("{}String: {} {}", indent(idx), value, self.pos().span);
            }
            ExprKind::CharLiteral(ch) => {
                println!("{}Char: {} {}", indent(idx), ch, self.pos().span);
            }
            ExprKind::Unary(ref op, ref expr) => {
                println!(
                    "{}Unary: {} {}",
                    indent(idx),
                    op.to_string(),
                    self.pos().span
                );
                expr.render(idx + 1);
            }
            ExprKind::Binary(ref op, ref lhs, ref rhs) => {
                println!(
                    "{}Binary: {} {}",
                    indent(idx),
                    op.to_string(),
                    self.pos().span
                );
                lhs.render(idx + 1);
                rhs.render(idx + 1);
            }
            ExprKind::FunctionCall(ref operand, ref actuals) => {
                println!("{}Fn Call {}:", indent(idx), self.pos().span);
                operand.render(idx + 1);

                println!("{}Actuals:", indent(idx + 1));
                for actual in actuals {
                    actual.render(idx + 1);
                }
            }
            ExprKind::MethodCall(ref operand, ref actuals) => {
                println!("{}Method Call {}:", indent(idx), self.pos().span);
                operand.render(idx + 1);

                println!("{}Actuals:", indent(idx + 1));
                for actual in actuals {
                    actual.render(idx + 1);
                }
            }
            ExprKind::Field(operand, field) => {
                println!("{}Field {}:", indent(idx), self.pos().span);
                operand.render(idx + 1);
                field.render(idx + 1);
            }
            ExprKind::Block(stmts) => {
                println!("{}Block {}:", indent(idx), self.pos().span);
                for stmt in stmts {
                    stmt.render(idx + 1);
                }
            }
            ExprKind::If(cond, body) => {
                println!("{}If {}:", indent(idx), self.pos().span);
                cond.render(idx + 1);
                body.render(idx + 1);
            }
            ExprKind::While(cond, body) => {
                println!("{}While {}:", indent(idx), self.pos().span);
                cond.render(idx + 1);
                body.render(idx + 1);
            }
            ExprKind::For(pattern, expr, body) => {
                println!("{}For {}:", indent(idx), self.pos().span);
                pattern.render(idx + 1);
                expr.render(idx + 1);
                body.render(idx + 1);
            }
            ExprKind::Loop(body) => {
                println!("{}Loop {}:", indent(idx), self.pos().span);
                body.render(idx + 1);
            }
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
    pub kind: StmtKind,
    position: Position,
}

impl Stmt {
    pub fn new(kind: StmtKind, position: Position) -> Self {
        Self { kind, position }
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
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Identifier(Ident),
    Tuple(Vec<Ptr<Pattern>>),
    // Ident here should be changed to a Path of some sort
    Variant(Ident, Vec<Ptr<Pattern>>),
    //    Struct(Ident, Vec<>)
    Ignore,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    kind: PatternKind,
    position: Position,
}

impl Pattern {
    pub fn new(kind: PatternKind, position: Position) -> Self {
        Self { kind, position }
    }

    pub fn kind(&self) -> &PatternKind {
        &self.kind
    }

    pub fn collect_names<'a>(&'a self) -> Vec<&'a String> {
        let mut res = Vec::<&'a _>::new();
        match self.kind() {
            PatternKind::Identifier(ident) => {
                res.push(&ident.val);
            }
            PatternKind::Tuple(patterns) => {
                for pattern in patterns {
                    res.extend(pattern.collect_names());
                }
            }
            PatternKind::Variant(_, patterns) => {
                for pattern in patterns {
                    res.extend(pattern.collect_names());
                }
            }
            PatternKind::Ignore => {},
        }

        res
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
pub enum TypeParamKind {
    // When the type parameter doesn't have any
    Named(Ident),
    BoundedNamed(Ident, Vec<Ptr<TypeSpec>>),
}

#[derive(Debug, Clone)]
pub struct TypeParam {
    kind: TypeParamKind,
    position: Position,
}

impl TypeParam {
    pub fn new(kind: TypeParamKind, position: Position) -> Self {
        Self { kind, position }
    }

    pub fn kind(&self) -> &TypeParamKind {
        &self.kind
    }

    pub fn name(&self) -> &str {
        self.name_string().as_str()
    }

    pub fn name_string(&self) -> &String {
        use TypeParamKind::*;
        match self.kind() {
            Named(n) |
            BoundedNamed(n, ..) => &n.val
        }
    }
}

impl AstNode for TypeParam {
    fn pos(&self) -> &Position {
        &self.position
    }
}

impl TreeRender for TypeParam {
    fn render(&self, idx: u32) {
        match self.kind() {
            TypeParamKind::Named(name) => {
                println!("{}Named: {}", indent(idx), name.value());
            }
            TypeParamKind::BoundedNamed(name, bounds) => {
                println!("{}Bounded Named: {}", indent(idx), name.value());
                for bound in bounds {
                    bound.render(idx + 1);
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeParams {
    types: Vec<Ptr<Item>>,
    position: Position,
}

impl TypeParams {
    pub fn new(types: Vec<Ptr<Item>>, position: Position) -> Self {
        Self { types, position }
    }
}

impl AstNode for TypeParams {
    fn pos(&self) -> &Position {
        &self.position
    }
}

impl TreeRender for TypeParams {
    fn render(&self, idx: u32) {
        println!("{}Type Parameters: {}", indent(idx), self.pos().span);
        for ty in &self.types {
            ty.render(idx + 1);
        }
    }
}


#[derive(Debug, Clone)]
pub enum VariableKind {
    Init(Ptr<Expr>),
    Typed(Ptr<TypeSpec>),
    Full(Ptr<TypeSpec>, Ptr<Expr>),
}

impl TreeRender for VariableKind {
    fn render(&self, idx: u32) {
        use VariableKind::*;
        match &self {
            Init(init) => {
                println!("{}Init:", indent(idx));
                init.render(idx + 1);
            }
            Typed(types) => {
                println!("{}Typed:", indent(idx));
                types.render(idx + 1);
            }
            Full(types, init) => {
                println!("{}Full:", indent(idx));
                types.render(idx + 1);
                init.render(idx + 1);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Local {
    pub mutability: Mutability,
    pub local: Ptr<Pattern>,
    pub kind: VariableKind,
    pub position: Position
}

impl AstNode for Local {
    fn pos(&self) -> &Position {
        &self.position
    }
}

impl TreeRender for Local {
    fn render(&self, idx: u32) {
        println!("{}Field:", indent(idx));
        println!("{}Mut: {}", indent(idx + 1), self.mutability.to_string());
        self.local.render(idx + 1);
        self.kind.render(idx + 1);
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    vis: Visibility,
    name: Ident,
    pub kind: VariableKind,
    position: Position,
}

impl Field {
    pub fn new(vis: Visibility, name: Ident, kind: VariableKind, position: Position) -> Self {
        Self {
            vis,
            name,
            kind,
            position,
        }
    }

    pub fn vis(&self) -> Visibility {
        self.vis
    }

    pub fn kind(&self) -> &VariableKind {
        &self.kind
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn name_string(&self) -> &String {
       &self.name.val
    }
}

impl AstNode for Field {
    fn pos(&self) -> &Position {
        &self.position
    }
}

impl TreeRender for Field {
    fn render(&self, idx: u32) {
        println!("{}Field:", indent(idx));
        println!("{}Vis: {}", indent(idx + 1), self.vis.to_string());
        self.name.render(idx + 1);
        self.kind.render(idx + 1);
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    mutability: Mutability,
    name: Ident,
    pub kind: VariableKind,
    position: Position,
}

impl Param {
    pub fn new(mutability: Mutability, name: Ident, kind: VariableKind, position: Position) -> Self {
        Self {
            mutability,
            name,
            kind,
            position,
        }
    }

    pub fn kind(&self) -> &VariableKind {
        &self.kind
    }

    pub fn name(&self) -> &str {
        self.name_string().as_str()
    }

    pub fn name_string(&self) -> &String {
       &self.name.val
    }
}

impl AstNode for Param {
    fn pos(&self) -> &Position {
        &self.position
    }
}

impl TreeRender for Param {
    fn render(&self, idx: u32) {
        println!("{}Param:", indent(idx));
        println!("{}Mut: {}", indent(idx + 1), self.mutability.to_string());
        self.name.render(idx + 1);
        self.kind.render(idx + 1);
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name:  Ident,
    pub type_params: Option<TypeParams>,
    pub params: Vec<Ptr<Item>>,
    pub ret: Ptr<TypeSpec>,
    pub body: Ptr<Expr>,
}

impl TreeRender for Function {
    fn render(&self, idx: u32) {
        let mut pos = self.name.pos().clone();
        pos.span.extend_node(self.body.as_ref());

        println!("{}Function: {} {}",
            indent(idx),
            self.name.value(),
            pos.span
        );
        // println!("{}Vis: {}", indent(idx + 1));
        self.name.render(idx + 1);
        self.type_params.as_ref().map(|param| param.render(idx + 1));
        println!("{}Params:", indent(idx + 1));
        for param in &self.params {
            param.render(idx + 1);
        }
        println!("{}Return:", indent(idx + 1));
        self.ret.render(idx + 1);
        println!("{}Body:", indent(idx + 1));
        self.body.render(idx + 1);
    }
}

#[derive(Debug, Clone)]
pub struct Structure {
    pub name: Ident,
    pub type_params: Option<TypeParams>,
    pub fields: Vec<Ptr<Item>>
}

impl TreeRender for Structure {
    fn render(&self, idx: u32) {
        let pos = self.name.pos().clone();
        println!(
            "{}Structure: {} {}",
            indent(idx),
            self.name.value(),
            pos.span
        );
        self.name.render(idx + 1);
        self.type_params.as_ref().map(|param| param.render(idx + 1));
        println!("{}Fields:", indent(idx + 1));
        for field in &self.fields {
            field.render(idx + 1);
        }
    }
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Funct(Function),
    Struct(Structure),
    Trait(),
    LocalVar(Local),
    StructField(Field),
    FunctionParam(Param),
    TypeParam(TypeParam),
    // for items defined by the compiler and are not located in a file.
    Internal,
    // for primative types defined by the compiler
    Primative,
    // for items that should not be copied yet. This is really a hack because I do not know how to handle this.
    Invalid,
}

#[derive(Debug, Clone)]
pub struct Item {
    vis: Visibility,
    pub kind: ItemKind,
    position: Position,
}

impl Item {
    pub fn internal_ptr() -> Ptr<Item> {
        Ptr::new(Item::new(
            Visibility::Public,
            ItemKind::Internal,
            Position::zero(),
        ))
    }

    pub fn invalid_ptr() -> Ptr<Item> {
        Ptr::new(Item::new(
            Visibility::Private,
            ItemKind::Invalid,
            Position::zero()
        ))
    }

    pub fn primative_ptr() -> Ptr<Item> {
        Ptr::new(Item::new(
            Visibility::Public,
            ItemKind::Primative,
            Position::zero(),
        ))
    }

    pub fn new(vis: Visibility, kind: ItemKind, position: Position) -> Self {
        Self {
            vis,
            kind,
            position,
        }
    }

    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }

    pub fn is_import(&self) -> bool {
        use ItemKind::*;

        match self.kind() {
            _ => false,
        }
    }

    pub fn is_variable(&self) -> bool {
        use ItemKind::*;

        match self.kind() {
           LocalVar(_) => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        use ItemKind::*;

        match self.kind() {
            Struct(..) => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        use ItemKind::*;

        match self.kind() {
            Funct(..) => true,
            _ => false,
        }
    }

    pub fn is_primative(&self) -> bool {
        use ItemKind::*;

        match self.kind() {
            Primative => true,
            _ => false,
        }
    }

    pub fn name(&self) -> &String {
        use ItemKind::*;
        static INVALID_STRING: String = String::new();

        match self.kind() {
            Funct(f) => &f.name.val,
            Struct(s) => &s.name.val,
            StructField(field) => field.name_string(),
            FunctionParam(param) => param.name_string(),
            TypeParam(type_param) => type_param.name_string(),
            _ => &INVALID_STRING,
        }
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
            ItemKind::Funct(f) => {
                f.render(idx);
            }
            ItemKind::Struct(s) => {
                s.render(idx);
            }
            ItemKind::LocalVar(local) => {
                local.render(idx);
            },
            ItemKind::StructField(field) => {
                field.render(idx);
            },
            ItemKind::FunctionParam(param) => {
                param.render(idx);
            },
            ItemKind::TypeParam(type_param) => {
                type_param.render(idx);
            },
            _ => {}
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeSpecKind {
    ExprType(Ptr<Expr>),
    MutType(Ptr<TypeSpec>),
    TupleType(Vec<Ptr<TypeSpec>>),
    PtrType(Ptr<TypeSpec>),
    UnitType,
}

#[derive(Debug, Clone)]
pub struct TypeSpec {
    pub kind: TypeSpecKind,
    position: Position,
}

impl TypeSpec {
    pub fn new(kind: TypeSpecKind, position: Position) -> Self {
        Self { kind, position }
    }

    pub fn kind(&self) -> &TypeSpecKind {
        &self.kind
    }

    pub fn new_unit_type(pos: &Position) -> Self {
        Self {
            kind: TypeSpecKind::UnitType,
            position: pos.clone(),
        }
    }
}

impl AstNode for TypeSpec {
    fn pos(&self) -> &Position {
        &self.position
    }
}

impl TreeRender for TypeSpec {
    fn render(&self, idx: u32) {
        match self.kind() {
            TypeSpecKind::ExprType(ty) => {
                println!("{}Expression Type:", indent(idx));
                ty.render(idx + 1);
            }
            //            TypeSpecKind::MutType(ty) => {
            //                println!("{}Mutable Type:", indent(idx));
            //                ty.render(idx + 1);
            //            }
            TypeSpecKind::TupleType(types) => {
                println!("{}Tuple Type:", indent(idx));
                for ty in types {
                    ty.render(idx + 1);
                }
            }
            TypeSpecKind::PtrType(ty) => {
                println!("{}Ptr Type:", indent(idx));
                ty.render(idx + 1);
            }
            _ => {}
        }
    }
}

pub struct ParsedFile {
    path: std::path::PathBuf,
    imports: Vec<Ptr<Item>>,
    items: Vec<Ptr<Item>>,
}

impl ParsedFile {
    pub fn new(path: &std::path::PathBuf) -> Self {
        Self {
            path: path.clone(),
            imports: Vec::new(),
            items: Vec::new(),
        }
    }

    pub fn add_import(&mut self, item: Ptr<Item>) {
        self.imports.push(item)
    }

    pub fn add_item(&mut self, item: Ptr<Item>) {
        self.items.push(item)
    }

    pub fn items(&self) -> &[Ptr<Item>] {
        self.items.as_slice()
    }

    pub fn items_mut(&mut self) -> &mut [Ptr<Item>] {
        self.items.as_mut_slice()
    }
}

impl TreeRender for ParsedFile {
    fn render(&self, idx: u32) {
        println!("{}Imports:", indent(idx));
        for imp in &self.imports {
            imp.render(idx + 1);
        }

        println!("{}Items:", indent(idx));
        for item in &self.items {
            item.render(idx + 1);
        }
    }
}
