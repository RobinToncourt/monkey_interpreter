use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::object::Object;

pub type SharedEnv = Rc<RefCell<Environment>>;

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<SharedEnv>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: SharedEnv) -> Self {
        let mut result = Self::new();
        result.outer = Some(outer);
        result
    }

    pub fn contains(&self, name: &str) -> bool {
        self.store.contains_key(name)
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let result = self.store.get(name).cloned();

        if result.is_none()
            && let Some(outer) = self.outer.as_ref()
        {
            return outer.borrow_mut().get(name);
        }

        result
    }

    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name, value.clone());
        value
    }
}
