use std::rc::Rc;

use super::scope::{Scope, ScopeId};
use super::types::Type;
use crate::syntax::ast::{Item, Ptr};

#[derive(Debug, Clone, PartialEq)]
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
    scope: Option<ScopeId>,
    // the current state of the item.
    state: ItemState,
}

impl ItemInfo {
    pub fn unresolved(item: Ptr<Item>, scope: Option<ScopeId>) -> Self {
	Self {
            item,
            resovled_type: Rc::new(Type::Unknown),
            scope,
            state: ItemState::Unresolved,
        }
    }

    pub fn resolved(item: Ptr<Item>, resovled_type: Rc<Type>, scope: Option<ScopeId>) -> Self {
        Self {
            item,
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
