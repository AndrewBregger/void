pub mod item_info;
pub mod scope;
pub mod types;
pub mod checked_file;

use std::rc::Rc;

use crate::diagnostics::Diagnostics;
use crate::syntax::ast::*;

use item_info::*;
use scope::*;
use types::*;
use checked_file::*;

pub enum Error {
    TypeError,
    InvalidModule,
}

type Result<T> = ::std::result::Result<T, Error>;

pub struct Semantics<'a> {
    diagnocits: &'a mut Diagnostics,
    scope_manager: ScopeManager,
}

impl<'a> Semantics<'a> {
    pub fn new(diagnocits: &'a mut Diagnostics, prelude_path: &str) -> Self {

        let mut checker = Self {
            diagnocits,
            scope_manager: ScopeManager::new()
        };

        checker.load_prelude(prelude_path);

        checker
    }

    fn load_prelude(&mut self, prelude_path: &str) {
        let mut prelude_scope = self.scope_manager.get_prelude_mut();
        prelude_scope
            .entry("i8".to_string())
            .or_insert(ItemInfo::new(Item::internal_ptr(), Rc::new(Type::I8), None));
        prelude_scope
            .entry("i16".to_string())
            .or_insert(ItemInfo::new(Item::internal_ptr(), Rc::new(Type::I16), None));
        prelude_scope
            .entry("i32".to_string())
            .or_insert(ItemInfo::new(Item::internal_ptr(), Rc::new(Type::I32), None));
        prelude_scope
            .entry("i64".to_string())
            .or_insert(ItemInfo::new(Item::internal_ptr(), Rc::new(Type::I64), None));

        prelude_scope
            .entry("u8".to_string())
            .or_insert(ItemInfo::new(Item::internal_ptr(), Rc::new(Type::U8), None));
        prelude_scope
            .entry("u16".to_string())
            .or_insert(ItemInfo::new(Item::internal_ptr(), Rc::new(Type::U16), None));
        prelude_scope
            .entry("u32".to_string())
            .or_insert(ItemInfo::new(Item::internal_ptr(), Rc::new(Type::U32), None));
        prelude_scope
            .entry("u64".to_string())
            .or_insert(ItemInfo::new(Item::internal_ptr(), Rc::new(Type::U64), None));

        prelude_scope
            .entry("char".to_string())
            .or_insert(ItemInfo::new(Item::internal_ptr(), Rc::new(Type::Char), None));
        prelude_scope
            .entry("bool".to_string())
            .or_insert(ItemInfo::new(Item::internal_ptr(), Rc::new(Type::Bool), None));

        prelude_scope
            .entry("__UNIT__".to_string())
            .or_insert(ItemInfo::new(Item::internal_ptr(), Rc::new(Type::Unit), None));
    }

    pub fn check_program(&mut self, file: &ParsedFile) -> Result<CheckedFile> {
        let checked_file = CheckedFile::new();
        Ok(checked_file)
    }
}
