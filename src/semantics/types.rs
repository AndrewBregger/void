use crate::syntax::ast::Ident;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
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

    Unit,
    //     name,  fields,     type params
    Struct(Ident, Vec<Ident>, Vec<Rc<Type>>),
    //       name,  parameters,    return type
    Function(Ident, Vec<Rc<Type>>, Rc<Type>),

    Unknown,
}

impl Type {
    pub fn is_primative(&self) -> bool {
        use Type::*;
        match self {
            I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | F32 | F64 | Char => true,
            _ => false,
        }
    }
}
