use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::runtime_entity::RuntimeEntity;

#[derive(Debug, Clone)]
pub struct Environment {
    pub entities: HashMap<String, RuntimeEntity>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            entities: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_local(env: Rc<RefCell<Environment>>) -> Self {
        Self {
            entities: HashMap::new(),
            enclosing: Some(env),
        }
    }
    
    pub fn define(&mut self, name: String, value: RuntimeEntity) {
        self.entities.insert(name, value);
    }

    pub fn get(&self, name: &String) -> Option<RuntimeEntity> {
        if let Some(val) = self.entities.get(name) {
            return Some(val.clone());
        }

        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow().get(name);
        }

        None
    }

    pub fn get_at(&self, distance: &usize, name: &String) -> Option<RuntimeEntity> {
        return match self.ancestors(distance).borrow().entities.get(name) {
            Some(val) => Some(val.clone()),
            None => None
        }
    }

    fn ancestors(&self, distance: &usize) -> Rc<RefCell<Environment>> {
        let mut env = Rc::new(RefCell::new(self.clone()));
        for _ in 0..*distance {
            let enc = env.borrow().clone().enclosing.unwrap();
            env = enc;
        }
        return env;
    }


    pub fn assign(&mut self, name: &String, entity: RuntimeEntity) -> Option<RuntimeEntity> {
        if self.entities.contains_key(name) {
            return self.entities.insert(name.clone(), entity);
        }

        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow_mut().assign(name, entity);
        }

        None
    }
}
