use std::collections::HashMap;

use crate::object::Object;

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Self>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Self) -> Self {
        let mut result = Self::new();
        result.outer = Some(Box::new(outer));
        result
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let result = self.store.get(name).cloned();

        if result.is_none()
            && let Some(outer) = self.outer.as_ref()
        {
            return outer.get(name);
        }

        result
    }

    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name, value.clone());
        value
    }
}
