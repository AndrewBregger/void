use std::collections::hash_map::Entry;
use std::collections::HashMap;

use super::item_info::{ItemInfo, ItemId};

#[derive(Debug, Clone, Copy)]
pub enum ScopeKind {
    Prelude,
    Struct,
    Block,
    TypeParam,
    Param,
    Module,
}

pub struct ScopeManager {
    scopes: Vec<Scope>,
    current: ScopeId,
}

impl ScopeManager {
    pub fn new() -> Self {
        let prelude = Scope::new(ScopeKind::Prelude);
        assert_eq!(prelude.id(), ScopeId(0));

        let mut manager = Self { scopes: Vec::new(), current: prelude.id() };

        manager.scopes.push(prelude);
        manager
    }

    pub fn push_scope(&mut self, kind: ScopeKind) -> &mut Scope {
        self.new_scope(kind, self.current)
    }

    pub fn pop_scope(&mut self) {
        let current = self.get_current();

        match current.parent {
            Some(pid) => self.current = self.get_scope(&pid).id(),
            None => {},
        }
    }

    pub fn get_current(&self) -> &Scope {
        self.get_scope(&self.current)
    }

    pub fn get_current_mut(&mut self) -> &mut Scope {
        let current = self.current.clone();
        self.get_scope_mut(&current)
    }

    pub fn new_scope(&mut self, kind: ScopeKind, parent: ScopeId) -> &mut Scope {
        let scope = Scope::with_parent(kind, parent);
        self.scopes.push(scope);
        self.scopes.last_mut().unwrap()
    }

    pub fn get_prelude(&self) -> &Scope {
        self.scopes.get(0).unwrap()
    }

    pub fn get_prelude_mut(&mut self) -> &mut Scope {
        self.scopes.get_mut(0).unwrap()
    }

    pub fn get_scope(&self, id: &ScopeId) -> &Scope {
        // this is safe because it is impossible to construct
        // a ScopeId with an invalid value. They will always be valid.
        // This assumption holds for the mutable version of this function.
        self.scopes.get(id.0).unwrap()
    }

    pub fn get_scope_mut(&mut self, id: &ScopeId) -> &mut Scope {
        self.scopes.get_mut(id.0).unwrap()
    }

    pub fn lookup_from<'scope, 'item>(
        &'scope self,
        mut scope: &'scope Scope,
        name: &String,
    ) -> Option<&'item ItemInfo>
    where
        'scope: 'item,
    {
        loop {
            match scope.lookup(name) {
                Some(item) => return Some(item),
                None => match scope.parent {
                    Some(p) => scope = self.get_scope(&p),
                    None => break,
                },
            }
        }
        None
    }

    pub fn lookup_current(&self, name: &String) -> Option<&ItemInfo> {
        let current = self.get_current();
        self.lookup_from(current, name)
    }

    // pub fn lookup_only_current

    pub fn scope_contains(&self, scope: &Scope, name: &String) -> bool {
        scope.lookup(name).is_some()
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    id: ScopeId,
    pub(crate) parent: Option<ScopeId>,
    env: HashMap<String, ItemInfo>,
    pub(crate) kind: ScopeKind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ScopeId(usize);

impl ScopeId {
    pub fn next() -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static TOKEN: AtomicUsize = AtomicUsize::new(0);

        Self(TOKEN.fetch_add(1, Ordering::SeqCst))
    }
}

impl Scope {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            id: ScopeId::next(),
            parent: None,
            env: HashMap::new(),
            kind,
        }
    }

    pub fn id(&self) -> ScopeId {
        self.id.clone()
    }

    pub fn item_by_id(&self, id: ItemId) -> Option<&ItemInfo> {
        self.env.values().find(|&item| item.id() == id)
    }


    pub fn item_by_id_mut(&mut self, id: ItemId) -> Option<&mut ItemInfo> {
        self.env.values_mut().find(|item| item.id() == id)
    }

    pub fn with_parent(kind: ScopeKind, parent: ScopeId) -> Self {
        let mut scope = Self::new(kind);
        scope.parent = Some(parent);
        scope
    }

    pub fn entry(&mut self, name: String) -> Entry<String, ItemInfo> {
        self.env.entry(name)
    }

    pub fn lookup(&self, name: &str) -> Option<&ItemInfo> {
        self.env.get(name)
    }
}
