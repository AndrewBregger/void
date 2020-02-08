pub mod checked_file;
pub mod item_info;
pub mod scope;
pub mod types;
pub mod typer;

use std::rc::Rc;
use std::collections::HashSet;

use crate::diagnostics::Diagnostics;
use crate::syntax::token::{Position};
use crate::syntax::ast::*;

use checked_file::*;
use item_info::*;
use scope::*;
use types::*;
use typer::*;

#[derive(Debug, Clone)]
pub enum Error {
    TypeError,
    InvalidModule,
    FatalError,
    UnresovledDependency(String),
}

pub type Result<T> = ::std::result::Result<T, Error>;

pub struct Semantics<'a> {
    diagnocits: &'a mut Diagnostics,
    scope_manager: ScopeManager,
}

impl<'a> Semantics<'a> {
    pub fn new(diagnocits: &'a mut Diagnostics, prelude_path: &str) -> Self {
        let mut checker = Self {
            diagnocits,
            scope_manager: ScopeManager::new(),
        };

        checker.load_prelude(prelude_path);

        checker
    }

    fn load_prelude(&mut self, prelude_path: &str) {
        use TypeKind::*;
        let prelude_scope = self.scope_manager.get_prelude_mut();
        prelude_scope
            .entry("i8".to_string())
            .or_insert(ItemInfo::resolved(
                Ident::new("i8", Position::zero()),
                Item::primative_ptr(),
                Type::new(I8),
                None,
            ));

        prelude_scope
            .entry("i16".to_string())
            .or_insert(ItemInfo::resolved(
                Ident::new("i16", Position::zero()),
                Item::primative_ptr(),
                Type::new(I16),
                None,
            ));

        prelude_scope
            .entry("i32".to_string())
            .or_insert(ItemInfo::resolved(
                Ident::new("i32", Position::zero()),
                Item::primative_ptr(),
                Type::new(I32),
                None,
            ));

        prelude_scope
            .entry("i64".to_string())
            .or_insert(ItemInfo::resolved(
                Ident::new("i64", Position::zero()),
                Item::primative_ptr(),
                Type::new(I64),
                None,
            ));

        prelude_scope
            .entry("u8".to_string())
            .or_insert(ItemInfo::resolved(
                Ident::new("u8", Position::zero()),
                Item::primative_ptr(),
                Type::new(U8),
                None,
            ));

        prelude_scope
            .entry("u16".to_string())
            .or_insert(ItemInfo::resolved(
                Ident::new("u16", Position::zero()),
                Item::primative_ptr(),
                Type::new(U16),
                None,
            ));

        prelude_scope
            .entry("u32".to_string())
            .or_insert(ItemInfo::resolved(
                Ident::new("u32", Position::zero()),
                Item::primative_ptr(),
                Type::new(U32),
                None,
            ));

        prelude_scope
            .entry("u64".to_string())
            .or_insert(ItemInfo::resolved(
                Ident::new("u64", Position::zero()),
                Item::primative_ptr(),
                Type::new(U64),
                None,
            ));

        prelude_scope
            .entry("f32".to_string())
            .or_insert(ItemInfo::resolved(
                Ident::new("f32", Position::zero()),
                Item::primative_ptr(),
                Type::new(F32),
                None,
            ));

        prelude_scope
            .entry("f64".to_string())
            .or_insert(ItemInfo::resolved(
                Ident::new("f64", Position::zero()),
                Item::primative_ptr(),
                Type::new(F64),
                None,
            ));

        prelude_scope
            .entry("char".to_string())
            .or_insert(ItemInfo::resolved(
                Ident::new("char", Position::zero()),
                Item::primative_ptr(),
                Type::new(Char),
                None,
            ));

        prelude_scope
            .entry("bool".to_string())
            .or_insert(ItemInfo::resolved(
                Ident::new("bool", Position::zero()),
                Item::primative_ptr(),
                Type::new(Bool),
                None,
            ));

        prelude_scope
            .entry("__UNIT__".to_string())
            .or_insert(ItemInfo::resolved(
                Ident::new("__UNIT__", Position::zero()),
                Item::primative_ptr(),
                Type::new(Unit),
                None,
            ));
    }

    pub fn check_program(&mut self, file: &mut ParsedFile) -> Result<CheckedFile> {
        let checked_file = CheckedFile::new();

        self.scope_manager.push_scope(ScopeKind::Module);

        let mut top_level_names = HashSet::new();


        for item in file.items() {
            top_level_names.insert(item.name().clone());
        }

        for mut item in file.items_mut() {
            let mut environment = Environment::new(&mut self.scope_manager, &top_level_names, true);
            let mut typer = Typer::new(&mut self.diagnocits);

            match typer.resolve_item(&mut environment, &mut item) {
                Ok(item) => {
                    let current = environment.scope_manager.get_current_mut();
                    let name = item.name();
                    current.entry(name.to_string()).or_insert(item);
                },
                Err(Error::UnresovledDependency(name)) => {
                    // attempt to resolve this name.
                    println!("Found Unresolved depended name: {}", name);
                }
                Err(e) => {
                    println!("Error: {:?}", e);
                }
            }
        }

        self.scope_manager.pop_scope();

        Ok(checked_file)
    }
}
