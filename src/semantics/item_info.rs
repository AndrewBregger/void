use std::rc::Rc;
use super::scope::{Scope, ScopeId};
use std::fmt::Display;
use super::types::{Type, TypeKind};

use crate::syntax::{
    ast::{Item, Ptr, Ident, AstNode},
    token::{Position}
};

#[derive(Debug, Clone, PartialEq)]
pub enum ItemState {
    Resolved,
    Active,
    Unresolved,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ItemId(usize);

impl ItemId {
    pub fn next() -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static TOKEN: AtomicUsize = AtomicUsize::new(0);

        Self(TOKEN.fetch_add(1, Ordering::SeqCst))
    }
}

impl Display for ItemId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Item({})", self.0)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ItemInfo {
    //
    id: ItemId,
    // the item this info is for
    item: Ptr<Item>,
    // the ident of this item.
    name: Ident,
    // the type of this entity
    resovled_type: Type,
    // the scope of this entity
    scope: Option<ScopeId>,
    // the current state of the item.
    state: ItemState,
}

impl ItemInfo {
    pub fn unresolved(name: Ident, item: Ptr<Item>, scope: Option<ScopeId>) -> Self {
	Self {
            id: ItemId::next(),
            item,
            name,
            resovled_type: Type::new(TypeKind::Unknown),
            scope,
            state: ItemState::Unresolved,
        }
    }

    pub fn resolved(name: Ident, item: Ptr<Item>, resovled_type: Type, scope: Option<ScopeId>) -> Self {
        Self {
            id: ItemId::next(),
            item,
            name,
            resovled_type,
            scope,
            state: ItemState::Resolved,
        }
    }


    pub fn resolve(self) -> Self {
        if self.state == ItemState::Resolved {
            self
        }
        else {
            Self {
                state: ItemState::Resolved,
                ..self
            }
	   }
    }

    pub fn pos(&self) -> &Position {
        self.item.pos()
    }

    pub fn id(&self) -> ItemId {
        self.id
    }

    pub fn name(&self) -> &str {
        self.name.value()
    }

    pub fn item(&self) -> &Item {
        self.item.as_ref()
    }

    pub fn item_type(&self) -> &Type {
        &self.resovled_type
    }

    pub fn is_variable(&self) -> bool {
        self.item.is_variable()
    }

    pub fn is_struct(&self) -> bool {
        self.item.is_struct()
    }

    pub fn is_function(&self) -> bool {
        self.item.is_function()
    }

    pub fn is_type(&self) -> bool {
        self.is_struct() // || self.is_trait() || self.is_variant()
    }
}
