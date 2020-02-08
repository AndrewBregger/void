use crate::syntax::ast::Ident;
use super::{scope::ScopeId, item_info::ItemId};
use std::rc::Rc;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct StructureType {
    name: Ident,
    field_scope: ScopeId,
    type_param_scope: Option<ScopeId>,
    item_id: ItemId,
}

impl StructureType {
    pub fn new(name: Ident, field_scope: ScopeId, type_param_scope: Option<ScopeId>, item_id: ItemId) -> Self {
        Self {
            name,
            field_scope,
            type_param_scope,
            item_id,
        }
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn item(&self) -> ItemId {
        self.item_id.clone()
    }


    pub fn field_scope(&self) -> &ScopeId {
        &self.field_scope
    }

    pub fn type_param_scope(&self) -> Option<&ScopeId> {
        self.type_param_scope.as_ref()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    param_scope: ScopeId,
    param_types: Vec<Type>,
    ret: Rc<Type>,
    type_param_scope: Option<ScopeId>,
    item_id: ItemId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Char,

    Bool,

    Str,

    Unit,
    //     name,  fields,     type params
    Struct(StructureType),
    //    name,  parameters,    return type
    Funct(FunctionType),
    // a type variable     name
    TypeVariable(Ident),
    // a type variable that had pre conditions to be valid.
    BoundedTypeVariable(Ident, Vec<Type>),
    MutType(Rc<Type>),
    PtrType(Rc<Type>),
    Tuple(Vec<Type>),
    Unknown,
}

pub fn join_type_list(tys: &[Type], sep: &str) -> String {
    let ty_string: Vec<String> = tys.iter().map(|ty| format!("{}", ty)).collect();
    ty_string.join(sep)
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use TypeKind::*;

        match self {
            I8   => write!(f, "i8"),
            I16  => write!(f, "i16"),
            I32  => write!(f, "i32"),
            I64  => write!(f, "i64"),
            U8   => write!(f, "u8"),
            U16  => write!(f, "u16"),
            U32  => write!(f, "u32"),
            U64  => write!(f, "u64"),
            F32  => write!(f, "f32"),
            F64  => write!(f, "f64"),
            Char => write!(f, "char"),
            Bool => write!(f, "bool"),
            Str  => write!(f, "string"),
            Unit => write!(f, "<>"),
            Struct(StructureType {
                name,
                ..
            }) => write!(f, "{}", name.value()),
            Funct(FunctionType {
                param_types,
                ret,
                ..
            }) => write!(f, "({}) {}", join_type_list(param_types.as_slice(), ", "), ret),
            TypeVariable(ident)  |
            BoundedTypeVariable(ident, _) =>  write!(f, "{}", ident.value()),
            MutType(ty) => write!(f, "mut {}", ty),
            PtrType(ty) => write!(f, "*{}", ty),
            Tuple(ref tys) => write!(f, "({})", join_type_list(tys.as_slice(), ", ")),
            _ => { Ok(()) },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeId(usize);

impl TypeId {
    fn next() -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static TOKEN: AtomicUsize = AtomicUsize::new(0);

        Self(TOKEN.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    id: TypeId,
    kind: TypeKind,
}

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        Self {
            id: TypeId::next(), kind
        }
    }

    pub fn kind(&self) -> &TypeKind {
        &self.kind
    }

    pub fn is_primative(&self) -> bool {
        use TypeKind::*;
        match &self.kind {
            I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | F32 | F64 | Char => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        use TypeKind::*;
        match &self.kind {
            I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 |  Char => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        use TypeKind::*;
        match &self.kind {
            F32 | F64 => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        use TypeKind::*;
        match &self.kind {
            Struct(_) => true,
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        use TypeKind::*;
        match &self.kind {
            Tuple(..) => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        use TypeKind::*;
        match &self.kind {
            Funct(_) => true,
            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}