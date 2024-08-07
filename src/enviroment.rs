use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::literal_value::LitVal;

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, LitVal>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_local(env: Rc<RefCell<Environment>>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(env),
        }
    }
    
    pub fn define(&mut self, name: String, value: LitVal) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &String) -> Option<LitVal> {
        if let Some(val) = self.values.get(name) {
            return Some(val.clone());
        }

        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow().get(name);
        }

        None
    }

    pub fn assign(&mut self, name: &String, value: LitVal) -> Option<LitVal> {
        if self.values.contains_key(name) {
            return self.values.insert(name.clone(), value);
        }

        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow_mut().assign(name, value);
        }

        None
    }
}
