use std::rc::Rc;

use crate::{callable_entity::{Callable, CallableEntity}, loc_error::LocErr, runtime_entity::{IntoRuntimeEntity, RuntimeEntity}};

#[derive(Clone)]
pub struct Function {
    pub ident: String,
    pub arity: usize,
    pub func: Rc<dyn Fn(Vec<RuntimeEntity>) -> Result<Option<RuntimeEntity>, LocErr>>,
}

impl IntoRuntimeEntity for Function {
    fn into_runtime_entity(self) -> RuntimeEntity {
        RuntimeEntity::Callable(CallableEntity::Function(self))
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Function({}/{})", self.ident, self.arity)
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}/{})", self.ident, self.arity)
    }
}

impl Function {
    pub fn new(ident: String, arity: usize, func: Rc<dyn Fn(Vec<RuntimeEntity>) -> Result<Option<RuntimeEntity>, LocErr>>) -> Self {
        Self { ident, arity, func }
    }
}

impl Callable for Function {
    fn arity(&self) -> usize { 
        self.arity 
    }
    fn call(&self, args: Vec<RuntimeEntity>) -> Result<Option<RuntimeEntity>, LocErr> { 
        (self.func)(args) 
    }
}