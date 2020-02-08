use crate::syntax::ast::Ident;
use super::{scope::ScopeId, item_info::ItemId};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct StructureType {
    name: Ident,
    field_scope: ScopeId,
    type_param_scope: Option<ScopeId>,
    item_id: ItemId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    name: Ident,
    param_scope: ScopeId,
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
            id: TypeId::next(),
            kind
        }
    }

    pub fn is_primative(&self) -> bool {
        use TypeKind::*;
        match &self.kind {
            I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | F32 | F64 | Char => true,
            _ => false,
        }
    }
}
