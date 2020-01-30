use std::rc::Rc;

use super::scope::Scope;
use super::types::Type;
use crate::syntax::ast::{Item, Ptr};

#[derive(Debug, Clone)]
pub enum ItemState {
    Resolved,
    Active,
    Unresolved,
}

#[derive(Debug, Clone)]
pub struct ItemInfo {
    // the item this info is for
    item: Ptr<Item>,
    // the type of this entity
    resovled_type: Rc<Type>,
    // the scope of this entity
    scope: Option<Rc<Scope>>,
    state: ItemState
}

impl ItemInfo {
    pub fn unresolved(item: Ptr<Item>) -> Self {
        Self::new(item, Rc::new(Type::Unknown), None)
    }

    pub fn new(item: Ptr<Item>, resovled_type: Rc<Type>, scope: Option<Rc<Scope>>) -> Self {
        Self {
            item,
            resovled_type,
            scope,
            state: ItemState::Unresolved
        }
    }

    pub fn resolve(self) -> Self {
        Self {
            state: ItemState::Resolved
            ..self
        }
    }

    pub fn item_type(&self) -> &Rc<Type> {
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
}
