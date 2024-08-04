use std::collections::HashMap;

use crate::expression::LitVal;

#[derive(Debug, Clone)]
pub struct Enviroment {
    values: HashMap<String, LitVal>,
    pub enclosing: Option<Box<Enviroment>>,
}

impl Enviroment {
    pub fn new() -> Self {
        Self { values: HashMap::new(), enclosing: None, }
    }

    pub fn new_local(env: Box<Enviroment>) -> Self {
        Self { values: HashMap::new(), enclosing: Some(env), }
    }
    
    pub fn define(&mut self, name: String, value: LitVal) {
        self.values.insert(name, value);
    } 

    pub fn get(&self, name: &String) -> Option<&LitVal> {
        let val = self.values.get(name);
        match (val, &self.enclosing) {
            (Some(v), _) => Some(v),
            (None, Some(env)) => env.get(name),
            (None, None) => None,
        }
    }

    pub fn assign(&mut self, name: &String, value: LitVal) -> Option<LitVal> {
        if self.values.contains_key(name) {
            self.values.insert(name.clone(), value)
        } else {
            match self.enclosing {
                Some(ref mut enclosing_env) => {
                    enclosing_env.assign(name, value.clone())
                }
                None => None,
            }
        }
    }
}
