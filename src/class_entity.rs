use std::collections::HashMap;

use crate::{callable_entity::{Callable, CallableEntity}, loc_error::LocErr, runtime_entity::{IntoRuntimeEntity, RuntimeEntity}};



#[derive(Clone)]
pub struct Class {
    pub name: String
}

impl IntoRuntimeEntity for Class {
    fn into_runtime_entity(self) -> RuntimeEntity {
        RuntimeEntity::Callable(CallableEntity::Class(self))
    }
}

impl Class {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl std::fmt::Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Class({})", self.name)
    }
}

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\\{}", self.name)
    }
}

impl Callable for Class {
    fn arity(&self) -> usize { 0 }
    fn call(&self, args: Vec<RuntimeEntity>) -> Result<Option<RuntimeEntity>, LocErr> { 
        assert!(args.is_empty(), "The default constructor doesn't take in any arguments.");
        Ok(Some(RuntimeEntity::ClassInstance(
            ClassInstance { name: self.name.clone(), fields: HashMap::new() }
        )))
    }
}

#[derive(Debug, Clone)]
pub struct ClassInstance {
    pub name: String,
    pub fields: HashMap<String, RuntimeEntity>,
}

impl ClassInstance {
    pub fn get(&mut self, name: &String) -> Result<RuntimeEntity, String> {
        dbg!(&self.fields, std::ptr::addr_of!(self));
        println!("---------------------------");

        match self.fields.get(name) {
            Some(field) => Ok(field.clone()),
            None => Err(format!("Attempted to access an undefined property {} from class instance {}.", name, self.name))
        }
    }
    
    pub fn set(&mut self, name: String, value: RuntimeEntity) -> Option<RuntimeEntity> {
        println!("inserting {name} with value {value} to map");
        let opt = self.fields.insert(name, value);

        dbg!(&self.fields, std::ptr::addr_of!(self));
        println!("---------------------------");
        
        opt
    }
}

impl IntoRuntimeEntity for ClassInstance {
    fn into_runtime_entity(self) -> RuntimeEntity {
        RuntimeEntity::ClassInstance(self)
    }
}

impl std::fmt::Display for ClassInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\\inst {}", self.name)
    }
}