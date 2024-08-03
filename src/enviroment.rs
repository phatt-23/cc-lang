use std::collections::HashMap;

use crate::expression::LitVal;


pub struct Enviroment {
    values: HashMap<String, LitVal>,
}

impl Enviroment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: LitVal) {
        self.values.insert(name, value);
    } 

    pub fn get(&self, name: String) -> Option<&LitVal> {
        self.values.get(&name)
    }

    pub fn assign(&mut self, name: String, value: LitVal) -> Option<LitVal> {
        if self.values.contains_key(&name) {
            self.values.insert(name, value)
        } else {
            None
        }
    }
}
