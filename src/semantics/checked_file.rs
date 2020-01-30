use super::item_info::ItemInfo;

/// A file such that all of its items are resolved.
pub struct CheckedFile {
    items: Vec<ItemInfo>,
}

impl CheckedFile {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }
}
