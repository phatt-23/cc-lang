use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::literal_value::LitVal;

#[derive(Debug, Clone)]
pub struct Environment {
    pub values: HashMap<String, LitVal>,
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

    pub fn get_at(&self, distance: &usize, name: &String) -> Option<LitVal> {
        dbg!(distance, name);
        return match self.ancestors(distance).borrow().clone().values.get(name) {
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
